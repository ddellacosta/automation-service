#!/usr/bin/env bash
# Summarizes the per-GC RTS statistics log (logs/gc.log) produced by the
# GC diagnostic run (compose.gc.yaml override). Answers one question:
#
#   are live bytes at GC growing over the run (a real leak), or stable
#   while RSS grows (allocator/heap dynamics)?
#
# The per-GC log format varies by GHC version, so this prints raw
# samples plus a per-numeric-column first/last/min/max summary rather
# than betting on column names.
set -euo pipefail

LOG="${1:-logs/gc.log}"

if [[ ! -f "$LOG" ]]; then
  cat >&2 <<EOF
No GC log at $LOG. Run the GC diagnostic first, e.g.:

  COMPOSE_FILE=compose.yaml:compose.gc.yaml \\
    AUTO_NAME=Gold CYCLES=100 \\
    AUTOMATION_IMAGE=automation-service:fixed LABEL=fixed-gc \\
    ./scripts/run-test.sh
EOF
  exit 1
fi

total=$(wc -l < "$LOG")

cat <<EOF
== gc.log ($LOG): $total lines ==

-- first 3 lines:
$(head -3 "$LOG")

-- last 3 lines:
$(tail -3 "$LOG")

-- numeric column summary (first / last / min / max per column):
EOF

awk '
  /^[[:space:]]*[0-9]/ || /^Gen:/ {
    n++
    for (i = 1; i <= NF; i++) {
      if ($i ~ /^[0-9]+$/) {
        if (!(i in seen)) {
          seen[i] = 1
          first[i] = $i
          min[i] = $i
          max[i] = $i
        } else {
          if ($i + 0 < min[i] + 0) min[i] = $i
          if ($i + 0 > max[i] + 0) max[i] = $i
        }
        last[i] = $i
        if (i > maxc) maxc = i
      }
    }
  }
  END {
    if (n == 0) { print "  (no numeric data lines found)"; exit }
    printf "  (%d data lines)\n", n
    for (i = 1; i <= maxc; i++)
      if (i in seen)
        printf "  col%-2d  first=%-14s last=%-14s min=%-14s max=%-14s\n", \
               i, first[i], last[i], min[i], max[i]
  }
' "$LOG"

cat <<'EOF'

Interpretation:
- A large column growing roughly linearly first->last (and matching
  the rss_kb creep in the CSV) => live set growing: real leak.
  Beware: an integer column growing ~5.3s per cycle is elapsed time,
  not memory.
- Live-sized columns flat while rss_kb still creeps => allocator/
  fragmentation dynamics (next knobs to try: +RTS -c dynamic
  compaction in the same compose override, or MALLOC_ARENA_MAX=2 for
  the C-side/glibc arena component).
If the column identification is ambiguous, paste this whole output
back for interpretation.
EOF