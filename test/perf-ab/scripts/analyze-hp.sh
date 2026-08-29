#!/usr/bin/env bash
# Summarizes a closure-type heap profile (logs/automation-service.hp)
# produced by the -hT diagnostic run (compose.hp.yaml override).
#
# The -S run answered "is the live set growing?" (yes, ~7 KB/cycle);
# this answers "growing in WHAT?" — it diffs per-closure-type live
# bytes between the first and last samples and prints the top
# growers.
#
# Usage: ./scripts/analyze-hp.sh [path-to-.hp]  (default:
# logs/automation-service.hp)
set -euo pipefail

HP="${1:-logs/automation-service.hp}"

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

awk '
  /^BEGIN_SAMPLE/ { ins = 1; delete cur; sampletot = 0; next }
  /^END_SAMPLE/   {
    ins = 0
    n++
    if (n == 1) {
      t1 = $2
      for (k in cur) first[k] = cur[k]
      totfirst = sampletot
    }
    t2 = $2
    for (k in cur) {
      last[k] = cur[k]
      if (cur[k] + 0 > max[k] + 0) max[k] = cur[k]
    }
    totlast = sampletot
    next
  }
  ins && NF >= 2 { cur[$1] = $NF + 0; sampletot += $NF + 0 }
  END {
    print "SUMMARY samples=" n "  first_sample_t=" t1 "s  last_sample_t=" t2 "s"
    print "SUMMARY total_live_first=" totfirst + 0 "  total_live_last=" totlast + 0 "  delta=" totlast - totfirst
    # data lines: delta, type, first, last, max  (types absent from
    # sample 1 are treated as growing from 0)
    for (k in last)
      print last[k] - (first[k] + 0), k, first[k] + 0, last[k] + 0, max[k] + 0
  }
' "$HP" > "$TMP"

grep "^SUMMARY" "$TMP" | sed 's/^SUMMARY //'
echo
echo "delta_bytes  closure_type  (first / last / max):"
grep -v "^SUMMARY" "$TMP" | sort -rn | head -15

cat <<'EOF'

Cheat sheet (what growth in each type usually means):
  TSO / STACK          thread/stack retention — async/thread lifecycle
  ARR_WORDS            raw byte arrays — ByteString/Text buffers
                       (accumulating messages, strings, log data)
  MUT_ARR_PTRS_*       mutable arrays — growing in-memory structures
  TVAR / TMVAR         STM retention — TChan cells not consumed,
                       MVars (one per async) not collected
  THUNK / AP_STACK     unevaluated laziness retained — classic space
                       leak by construction, not data accumulation
Paste this output back for interpretation if unclear.
EOF