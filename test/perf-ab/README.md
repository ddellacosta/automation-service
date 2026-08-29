# Leak/perf A/B test harness for automation-service

Runs one automation-service image at a time against a local mosquitto
broker, drives repeated start/stop cycles of a minimal Lua automation
over MQTT, and samples the running container's file-descriptor count,
RSS, peak RSS, and open SQLite handles per cycle.

Used to verify the resource-leak fixes merged in #47 (`95012f8`;
`9399625` SQLite connection leak, `dda3243` Lua state leak) by comparing
a pre-fix image ("baseline") against a post-fix image ("fixed").

## Prerequisites

- docker + docker compose v2
- two local image tags:
  - `automation-service:baseline` — pre-fix build (`2d3f637`)
  - `automation-service:fixed` — build with the two leak fixes

## Getting the two images

The fixes are merged to main (#47, `95012f8`); the last pre-fix commit is
`2d3f637`.

**Route A (recommended — exact control): build both locally with nix
from the repo:**

    cd /path/to/automation-service
    git checkout 2d3f637
    nix build
    docker load -i result | sed -E 's/^Loaded image: (.+)$/\1/' \
      | xargs -I{} docker tag {} automation-service:baseline

    git checkout main              # includes the leak fixes = fixed
    nix build
    docker load -i result | sed -E 's/^Loaded image: (.+)$/\1/' \
      | xargs -I{} docker tag {} automation-service:fixed

Both images use the same config format (both are post-PR-#45), so the
harness's single `config/` works for both runs. First build can take a
while without the cachix cache (`ddellacosta`).

**Route B (fewer builds): local baseline + GHCR fixed.** Build only the
baseline locally from `2d3f637` (as above), and use the CI-built image
for the fixed side (main was merged and CI has pushed `latest`):

    docker pull ghcr.io/ddellacosta/automation-service:latest
    docker tag ghcr.io/ddellacosta/automation-service:latest automation-service:fixed

(Using your 16-month-old image as baseline is possible but needs an
old-format config — not recommended.)

## Running

From this directory (`test/perf-ab/`):

    AUTOMATION_IMAGE=automation-service:baseline LABEL=baseline ./scripts/run-test.sh
    AUTOMATION_IMAGE=automation-service:fixed     LABEL=fixed     ./scripts/run-test.sh

Each run: fresh broker + service + db + logs, waits for readiness,
runs `CYCLES` (default 20) start/stop cycles of the `leaktest` Lua
automation, samples metrics after each cycle into `results-<LABEL>.csv`,
then tears everything down. ~6s per cycle.

The two runs are strictly sequential (same MQTT topics would collide
if run in parallel).

## Expected results

| metric                  | baseline                            | fixed                          |
|-------------------------|-------------------------------------|--------------------------------|
| `fds`                   | +2 per cycle (leaked SQLite conns)  | flat                           |
| `db_handles`            | grows monotonically                 | 0–2 (transient during writes)  |
| `rss_kb`                | ratchets up cycle over cycle        | plateaus after a few cycles    |
| `hwm_kb`                | keeps rising                        | plateaus                       |

GHC does not eagerly return freed heap to the OS, so judge `rss_kb` by
its slope over the run (ratchet vs plateau), not by it returning to the
startup value. A sample taken right after a state write may transiently
catch 1–2 open db handles in the fixed image — that is the write in
flight, not a leak; the next cycle's sample will confirm.

## Troubleshooting

- `Timed out waiting for mosquitto` — broker didn't come up; check
  `docker compose logs mosquitto`.
- `service container died during cycle N` — the service crashed;
  inspect `logs/logfile` in this directory.
- Readiness is confirmed via the service log (`Running StateManager`)
  with periodic MQTT pings to flush buffered log output; startup
  timeout is 120s.

## Optional extras

**fsync counting (patch 1's transaction work):** from the host, while a
run is in progress:

    strace -f -p $(docker inspect -f '{{.State.Pid}}' perf-ab-automation-service) \
      -e trace=fsync,fdatasync -c

Baseline does one commit per statement (DELETE + each INSERT); fixed
does one commit per update — expect roughly 3–5× fewer fsyncs per
state change. Ctrl-C to get the count.

**Restart/restore check (requires faking zigbee2mqtt bridge messages so
`RestartConditions` are met):**

    # with leaktest running and the stack up:
    docker compose exec -T mosquitto mosquitto_pub -h localhost -r \
      -t zigbee2mqtt/bridge/devices \
      -m '[{"ieee_address":"0x0000000000000001","friendly_name":"perf-ab-fake","type":"FakeDevice"}]'
    docker compose exec -T mosquitto mosquitto_pub -h localhost -r \
      -t zigbee2mqtt/bridge/groups \
      -m '[{"id":1,"friendly_name":"perf-ab-group","members":[],"scenes":[]}]'
    docker compose restart automation-service
    # leaktest should come back by itself after restart (restored from SQLite)

Then clean up the retained messages:

    docker compose exec -T mosquitto mosquitto_pub -h localhost -r \
      -t zigbee2mqtt/bridge/devices -n
    docker compose exec -T mosquitto mosquitto_pub -h localhost -r \
      -t zigbee2mqtt/bridge/groups -n