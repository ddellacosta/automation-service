#!/usr/bin/env bash
# Summarizes a closure-type heap profile (logs/automation-service.hp)
# produced by the -hT diagnostic run (compose.hp.yaml override).
#
# Two .hp gotchas learned the hard way:
#
# 1. Sample timestamps are CPU-seconds, NOT wall-clock. A mostly-idle
#    app's ~10-minute run may span only ~5 CPU-seconds. Judge profile
#    coverage by sample count and content, not by the time value.
# 2. Sample 1 is an empty pre-startup snapshot (live=0). Growth must be
#    measured from a POST-STARTUP baseline — default: the sample 30%
#    into the file (override with the 2nd arg, a percentage).
#
# Usage: ./scripts/analyze-hp.sh [path-to-.hp] [baseline_pct]
set -euo pipefail

HP="${1:-logs/automation-service.hp}"
PCT="${2:-30}"

if [[ ! -f "$HP" ]]; then
  cat >&2 <<EOF
No heap profile at $HP. Run the -hT diagnostic first, e.g.:

  COMPOSE_FILE=compose.yaml:compose.hp.yaml \\
    AUTO_NAME=Gold CYCLES=100 \\
    AUTOMATION_IMAGE=automation-service:fixed LABEL=fixed-hp \\
    ./scripts/run-test.sh
EOF
  exit 1
fi

TMP=$(mktemp)
trap 'rm -f "$TMP"' EXIT

awk -v pct="$PCT" '
  /^BEGIN_SAMPLE/ { n++; t[n] = $2; next }
  /^END_SAMPLE/   { next }
  n > 0 && NF >= 2 {
    v[n SUBSEP $1] += $NF + 0
    if (!($1 in types)) types[$1] = 1
    tot[n] += $NF + 0
  }
  END {
    b = int(n * pct / 100); if (b < 2) b = 2
    printf "SUMMARY samples=%d  baseline=sample %d (cpu-t=%s)  last=sample %d (cpu-t=%s)\n", n, b, t[b], n, t[n]
    printf "SUMMARY live_bytes: baseline=%d  last=%d  growth=%+d\n", tot[b], tot[n], tot[n] - tot[b]
    step = int(n / 15); if (step < 1) step = 1
    print "SUMMARY per-sample live series (sample cpu-t live):"
    for (i = 2; i < n; i += step)
      printf "SUMMARY   %d %s %d\n", i, t[i], tot[i]
    printf "SUMMARY   %d %s %d\n", n, t[n], tot[n]
    for (k in types) {
      a = v[b SUBSEP k] + 0
      c = v[n SUBSEP k] + 0
      print c - a, k, a, c
    }
  }
' "$HP" > "$TMP"

grep "^SUMMARY" "$TMP" | sed 's/^SUMMARY //'
echo
echo "delta_bytes  closure_type  (baseline / last):"
grep -v "^SUMMARY" "$TMP" | sort -rn | head -15

cat <<'EOF'

Cheat sheet (what growth in each type usually means):
  STACK (one-time step, not per-cycle)  thread stacks grew once — not a leak
  TChan.TCons + TVAR + app message types  channel history retained —
                       a reader's read-pointer never advances
  ARR_WORDS            raw byte arrays — ByteString/Text buffers
  MUT_ARR_PTRS_*      mutable arrays — growing in-memory structures
  THUNK / AP_STACK     unevaluated laziness retained — classic space
                       leak by construction, not data accumulation
Paste this output back for interpretation if unclear.
EOF