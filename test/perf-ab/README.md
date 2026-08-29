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
runs `CYCLES` (default 20) start/stop cycles of `AUTO_NAME` (default
`leaktest`, the Lua test script), samples metrics after each cycle
into `results-<LABEL>.csv`, then tears everything down. ~6s per cycle.

The two runs are strictly sequential (same MQTT topics would collide
if run in parallel).

## Expected results

| metric                  | baseline                            | fixed                           |
|-------------------------|-------------------------------------|---------------------------------|
| `fds`                   | +2 per cycle (leaked SQLite conns)  | flat                            |
| `db_handles`            | grows monotonically                 | 0 (transient during writes)     |
| `rss_kb`                | ratchets ~450 KB/cycle              | ~11× slower, still creeping     |
| `hwm_kb`                | keeps rising                        | rises at the reduced rate       |
| `threads`               | —                                   | constant                        |

GHC does not eagerly return freed heap to the OS, so judge `rss_kb` by
its slope over the run (ratchet vs plateau), not by it returning to the
startup value. A sample taken right after a state write may transiently
catch 1–2 open db handles in the fixed image — that is the write in
flight, not a leak; the next cycle's sample will confirm.

Note after the 100-cycle soak on the fixed image: fds and db handles
stayed perfectly flat (the merged leak fixes holding), but RSS still
crept at a persistent ~35–45 KB/cycle — a residual leak beyond the
two merged fixes. The Lua-vs-Gold control run below localizes it.

## Isolating a residual leak: Lua vs Gold control run

The soak's creep is pure heap retention (fds flat). To split it
between the *daemon machinery* exercised by every start/stop cycle
(async spawn/cancel, ThreadMap, broadcast-channel churn, SQLite
writes) and the *Lua path* (two interpreter states per cycle, 28
registered API-function closures, subscribe machinery), cycle a
pure-Haskell automation instead of a Lua script:

    AUTO_NAME=Gold      AUTOMATION_IMAGE=automation-service:fixed LABEL=fixed-gold ./scripts/run-test.sh
    AUTO_NAME=leaktest  AUTOMATION_IMAGE=automation-service:fixed LABEL=fixed-lua  ./scripts/run-test.sh

Gold exercises the same daemon-side machinery per cycle but no Lua.
Two profile differences, both fine for this comparison:

- Gold finds no registered devices in this environment (no
  zigbee2mqtt), so its run finishes immediately instead of idling
  until stopped; the cycle then exercises spawn + cancel of an
  already-completed async.
- Gold registers the same device/group on every start, and only
  Lua-script cleanups deregister, so its registration entries
  accumulate one NonEmpty element per cycle (a few dozen bytes each —
  negligible for RSS, but a known issue for the fix list: automations
  should deregister on stop, and addRegisteredResource should not
  append duplicates).

Interpretation:

| observation                | verdict                              |
|----------------------------|----------------------------------------|
| Gold flat, Lua grows       | leak is Lua-side: interpreter lifecycle (hslua close path, API-function stable pointers) or subscribe machinery |
| Gold grows too             | daemon-side leak shared by all automation types — prime suspect: the fire-and-forget `async (cancel ...)` in stopAutomation (§3) |
| `threads` grows (either arm) | thread retention: lingering cancel-asyncs or unreleased bound threads from the Lua lifecycle |
| both flat                  | residual was allocator/GC behavior; re-run the Lua arm with CYCLES=200 to confirm convergence |

Expect a small constant `threads` baseline (GHC -N workers + main +
MQTT/HTTP threads + one bound thread per running Lua automation); any
monotonic growth across cycles is the signal.

## GC diagnostic run: live bytes vs fragmentation

The Gold control run showed the residual creep is shared/daemon-side
(~36 KB/cycle Gold vs ~53 KB/cycle Lua), with no accountable live-data
growth in the no-Lua code path — leaving two hypotheses: hidden
retention, or GHC allocator/heap dynamics (GHC does not compact the
heap by default, and freed heap is not eagerly returned to the OS).
The discriminator is the RTS's own live-bytes statistics: if live
bytes at GC grow with cycles it's a real leak; if they're stable
while RSS grows, it's allocator/fragmentation behavior.

Run the diagnostic on the Gold arm at soak length:

    COMPOSE_FILE=compose.yaml:compose.gc.yaml \
      AUTO_NAME=Gold CYCLES=100 \
      AUTOMATION_IMAGE=automation-service:fixed LABEL=fixed-gc \
      ./scripts/run-test.sh

`compose.gc.yaml` overrides the container command to add
`+RTS -Slogs/gc.log -RTS` (merged with the image's baked-in
`-with-rtsopts=-N -T`), writing one statistics line per GC into the
mounted `logs/` directory. Then:

    ./scripts/analyze-gc.sh          # defaults to logs/gc.log

Interpretation:

- A large column growing roughly linearly first→last, matching the
  `rss_kb` creep in the CSV → live set is growing: real leak, hunt
  the object.
- Live-sized columns flat while `rss_kb` still creeps → allocator/
  fragmentation dynamics; knobs to try next: `+RTS -c -RTS` (dynamic
  compaction) in the same override, or `MALLOC_ARENA_MAX=2` for the
  C-side (glibc arena) component.

Notes:

- `analyze-gc.sh` prints raw samples plus a per-column
  first/last/min/max summary because the per-GC log format varies by
  GHC version; if column identification is ambiguous, paste its whole
  output back for interpretation.
- If the service fails to start under the override, this GHC may not
  accept `-S<file>` — change the command to
  `["+RTS", "-S", "-RTS"]` and capture `docker compose logs
  automation-service` before the stack is torn down.
- `logs/` is wiped by the next `run-test.sh` invocation, so run the
  analyzer (or copy `gc.log` out) right after the diagnostic run.

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