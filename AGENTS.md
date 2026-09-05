# AGENTS.md

Notes for AI coding agents (and humans) working in this repository.
Read this before making changes; add new hard-won lessons here.

## Project layout

- Haskell backend: `src/`, `app/`, `test/`; PureScript/Elmish UI: `ui/src/`
- Playwright-based UI tests: `ui/test/` (entry point: `Test.Runner`)
- Nix flake builds everything; CI: `.github/workflows/main.yml`

## Running things

- UI tests, fast local loop:
  `cd ui && spago bundle -p automation-service-test && node test/run-tests.mjs`
  (`run-tests.mjs` starts http-server, waits for readiness, runs the
  bundled suite, tears the server down, forwards the exit code)
- The *real* CI condition, locally:
  `nix build .#automation-service-ui-test -o frontend-out && cat frontend-out/share/test-log.txt`
- Frontend test failures embed page console output, network log, and an
  HTML snapshot in the error message; failures also attach a full-page
  screenshot to the Allure result. Don't re-add print debugging — read
  what's already captured.

## PureScript gotchas

- argonaut JSON building: `:=` is *non-associative*, so two bare `:=`
  cannot appear in one unparenthesized expression; `~>` chains
  right-associatively and `jsonEmptyObject` (type `Json`) terminates the
  chain at the **end**. House style: see `mkAllureJson` in
  `ui/test/src/Test/Spec/Reporter/Allure.purs`.
- `Data.String.replaceAll` takes a **literal** pattern, not a regex.
  Regex substitution must go through FFI (see `safeFilename_`).
- PureScript `Effect` functions called from JS return a **thunk that
  must be invoked**: `handler(arg)()`, not `handler(arg)`. Missing the
  trailing `()` silently no-ops the handler — this cost a full
  debugging cycle (see the event-listener FFIs in
  `ui/test/src/Test/Playwright.js`, which get this right).

## Frontend tests / nix sandbox

- `ui/index.js` and `ui/css/*.css` are **gitignored build artifacts**.
  Clean nix trees don't have them; the test derivation copies them from
  the `automation-service-ui` build output.
- The nix build sandbox has **no network**: after changing
  `ui/package-lock.json`, regenerate the offline npm cache with
  `prefetch-npm-deps ui/package-lock.json` and update `npmDepsHash` in
  `flake.nix` (note: there are two — one for the UI, one for the backend).
- The sandbox has **no fonts**: without `FONTCONFIG_FILE`
  (`makeFontsConf`), chromium renders text with zero-size bounding boxes
  and Playwright *visible*-state waits fail with "locator resolved to
  hidden" even though the app is fully rendered.
- chromium in the sandbox needs the `--no-sandbox` family of launch
  flags (see `ui/test/src/Test/Playwright.js`).
- http-server: pass the served directory as the **first** positional
  argument; a trailing positional was observed to be ignored.
- The app's WebSocket is always intercepted in tests
  (`routeWebSocket`); fixture data is fed from inside the route
  handler, so there is no race with the app's connection. See
  `Test.Main`.

## CI

- GitHub Actions must target node24 (or be composite/shell-only).
  When bumping an action, check its `action.yml` `runs.using` — the
  latest release of an action is not guaranteed to be node24.
- The per-branch Allure manifest (`allure-action/<branch>/<suite>/data.json`)
  consumed by `generate-allure-site` is written by
  `mgrybyk-org/allure-report-branch-js-action` — verify its output
  format when changing that action's major version.

## Never commit

- `config/config.dhall` contains local credentials (MQTT password,
  cert paths). Keep local configuration out of git.