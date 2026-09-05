# White-box STM instrumentation — analysis and resurrection notes

This branch preserves a vendored copy of Leuschner/Breitner's `trackSTM`
(`Control.Concurrent.STM.Stats`, BSD3), which instruments individual STM
transactions: per-transaction commit/retry counts, runtime warnings on
retry thresholds, and deadlock exceptions that name the transaction you
were blocked on.

## Status when saved (2026-09-05)

- `Env.hs` wiring was a **passthrough stub**
  (`trackNamedSTM = \_n v -> atomically v`), so the instrumentation
  never collected a single data point.
- `Daemon.hs` had a commented `dumpSTMStats` hook at shutdown.
- `automation-service.cabal` carried `containers`, `stm`, and
  `template-haskell` solely for this module (nothing else in `src/`
  imports them; the app uses `UnliftIO.STM` via unliftio).

## Black-box vs. white-box — when to reach for which

| | `test/perf-ab` (black-box workload harness, `72595d57`) | this module (white-box STM instrumentation) |
|---|---|---|
| Measures | Outcomes over realistic load: fd count, RSS, peak RSS, SQLite handles, per-GC stats (`+RTS -s`), heap profiles (`-h`) | Per-transaction commits, retries, retry ratio; runtime threshold warnings |
| Answers | "Is something leaking? Did the fix work?" | "**Which** transaction is contended or retry-storming?" |
| Infection | None — external compose scripts + RTS flags | Every TVar/`atomically` site must route through `track*` variants |
| Overhead | None on the service itself | Small per-transaction cost + global `unsafePerformIO` maps |

They are orthogonal, not competing: perf-ab tells you *that* something is
wrong (and whether a fix worked); this module tells you *where* in
STM-land — but only if the question is STM-specific.

## The one unique sharp tool

`BlockedIndefinitelyOnNamedSTM`: GHC's plain `BlockedIndefinitelyOnSTM`
tells you a thread was killed for blocking on STM forever, but not on
*what*. With `trackNamedSTM` wiring, the exception carries the
transaction's name — in an app with a dozen broadcast channels and TVars
(subscriptions, threadMap, devices, groups, restartConditions, …), that
naming is the difference between a five-minute diagnosis and an
afternoon.

## Why it was removed

1. Never ran (stub wiring), so it produced no knowledge.
2. The infection *is* the mechanism: stats only mean something if every
   STM site is wrapped, which is a standing maintenance tax for a rare
   question.
3. The module is third-party and recoverable (this branch; upstream
   `trackSTM` by Joachim Breitner).

## How to resurrect

1. Take `src/Service/STM/Stats.hs` from this branch.
2. Cabal: add back `containers`, `stm`, `template-haskell`, and
   `Service.STM.Stats` to the library exposed-modules.
3. Replace the stub in `Service.Env.initialize` with the real
   `trackNamedSTM` for every TVar/TChan creation.
4. Route suspect `atomically` call-sites through `trackNamedSTM`
   (or `trackThisSTM` with `-XTemplateHaskell` for auto-naming).
5. Call `dumpSTMStats` from the `Daemon.hs` shutdown path (the hook was
   at `run'` cleanup, after cancelling automations).

Reference: black-box harness lives at `test/perf-ab/` (commit `72595d57`,
PR #49), including its README and `compose.gc.yaml` / `compose.hp.yaml`
RTS-flag variants.