#!/usr/bin/env bash
# A/B leak-test runner for automation-service.
#
# Usage (from anywhere; must be run with docker compose v2 available):
#
#   AUTOMATION_IMAGE=automation-service:baseline LABEL=baseline ./scripts/run-test.sh
#   AUTOMATION_IMAGE=automation-service:fixed     LABEL=fixed     ./scripts/run-test.sh
#
# Optional env vars:
#   CYCLES     number of start/stop cycles (default 20)
#   AUTO_NAME  automation name to cycle (default "leaktest", the Lua
#              test script; use "Gold" for the no-Lua control run —
#              see README.md "Isolating a residual leak")
#
# Produces results-<LABEL>.csv in test/perf-ab/ with one sample per
# cycle. Runs are strictly sequential against a fresh stack (broker,
# db, logs) per run.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR/.."

IMAGE="${AUTOMATION_IMAGE:?Set AUTOMATION_IMAGE, e.g. AUTOMATION_IMAGE=automation-service:baseline}"
LABEL="${LABEL:-run}"
CYCLES="${CYCLES:-20}"
AUTO_NAME="${AUTO_NAME:-leaktest}"
CSV="results-${LABEL}.csv"
CTR="perf-ab-automation-service"
export AUTOMATION_IMAGE="$IMAGE"

pub() { # pub '<json control message>'
  docker compose exec -T mosquitto \
    mosquitto_pub -h localhost -t "automation-service/set" -m "$1" >/dev/null
}

alive() {
  [[ "$(docker inspect -f '{{.State.Running}}' "$CTR" 2>/dev/null || true)" == "true" ]]
}

die_if_dead() {
  if ! alive; then
    echo "!! service container is not running — check ./logs/logfile" >&2
    docker compose ps -a >&2 || true
    exit 1
  fi
}

echo "== perf A/B run: image=$IMAGE label=$LABEL cycles=$CYCLES automation=$AUTO_NAME"

# Fresh state for every run
docker compose down -v --remove-orphans >/dev/null 2>&1 || true
rm -rf data logs
mkdir -p data logs
rm -f "$CSV"
echo "timestamp_epoch,fds,rss_kb,hwm_kb,db_handles,threads,cycle" > "$CSV"

# Broker first, then wait until it accepts connections
docker compose up -d mosquitto >/dev/null
for _ in $(seq 1 30); do
  docker compose exec -T mosquitto \
    mosquitto_pub -h localhost -t perf-ab/ping -m ping >/dev/null 2>&1 && break
  sleep 1
done

# Service; readiness = "Running StateManager" appears in the log file.
# The wait loop pings the service's status topic (any message there
# triggers a Daemon.Status round-trip) which both generates log traffic
# (flushing buffered log output) and confirms the daemon is processing.
docker compose up -d automation-service >/dev/null
started=""
for _ in $(seq 1 60); do
  die_if_dead
  docker compose exec -T mosquitto \
    mosquitto_pub -h localhost -t automation-service/status -m ping >/dev/null 2>&1 || true
  if grep -q "Running StateManager" logs/logfile 2>/dev/null; then
    started=1
    break
  fi
  sleep 2
done
if [[ -z "$started" ]]; then
  echo "!! service did not become ready — check ./logs/logfile" >&2
  exit 1
fi
sleep 5 # let StateManager/HTTPDefault settle

# Baseline sample before any cycles
echo "$("$SCRIPT_DIR/collect-metrics.sh" "$CTR"),0" | tee -a "$CSV"

for i in $(seq 1 "$CYCLES"); do
  pub "{\"start\":\"$AUTO_NAME\"}"
  sleep 2
  die_if_dead

  pub "{\"stop\":\"$AUTO_NAME\"}"
  sleep 3
  die_if_dead

  echo "$("$SCRIPT_DIR/collect-metrics.sh" "$CTR"),$i" | tee -a "$CSV"
done

echo
echo "== done: $CSV"
echo "-- first sample:"
sed -n '2p' "$CSV"
echo "-- last sample:"
tail -1 "$CSV"

docker compose down -v --remove-orphans >/dev/null