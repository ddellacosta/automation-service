#!/usr/bin/env bash
# Prints one CSV line for the running automation-service container:
#
#   timestamp_epoch,fds,rss_kb,hwm_kb,db_handles,threads
#
# Uses only `docker exec` (no host /proc access or privileges needed);
# the service runs as PID 1 in the container, and the image ships
# busybox/coreutils.
#
# threads is the OS thread count of the service. The service runs GHC
# with -N (one OS worker per capability) and uses bound threads for
# Lua operations, so expect a small constant baseline; any
# cycle-over-cycle growth is the signature of thread retention (e.g.
# lingering cancel-asyncs from stopAutomation, or bound threads not
# released by the Lua lifecycle).
set -euo pipefail

CTR="${1:-perf-ab-automation-service}"

now=$(date +%s)
fds=$(docker exec "$CTR" sh -c 'ls /proc/1/fd 2>/dev/null | wc -l')
mem=$(docker exec "$CTR" sh -c 'grep -E "^(VmRSS|VmHWM)" /proc/1/status')
rss=$(printf '%s\n' "$mem" | sed -n 's/VmRSS:[[:space:]]*\([0-9]*\).*/\1/p')
hwm=$(printf '%s\n' "$mem" | sed -n 's/VmHWM:[[:space:]]*\([0-9]*\).*/\1/p')
# grep -c always prints the count (0) even when it exits 1 on no match
db=$(docker exec "$CTR" sh -c 'ls -l /proc/1/fd 2>/dev/null | grep -c automationState' || true)
threads=$(docker exec "$CTR" sh -c 'grep -E "^Threads" /proc/1/status' \
  | sed -n 's/Threads:[[:space:]]*\([0-9]*\).*/\1/p')

echo "$now,$fds,$rss,$hwm,$db,$threads"