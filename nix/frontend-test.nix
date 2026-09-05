{ automation-service-ui
, automation-service-ui-npm-deps
, node_version
, pkgs
, spagoLock ? ../ui/spago.lock
, spagoYaml ? ../ui/spago.yaml
, src ? ../ui
}:

let
  drv = pkgs.mkSpagoDerivation {
    inherit spagoLock spagoYaml src;

    name = "automation-service frontend-test.nix";

    nativeBuildInputs = [
      automation-service-ui
      automation-service-ui-npm-deps
      node_version
      pkgs.chromium
      pkgs.esbuild
      pkgs.purs
      pkgs.spago-unstable
    ];

    buildPhase = ''
      runHook preBuild

      ln -sf ${automation-service-ui-npm-deps}/lib/node_modules ./node_modules
      spago bundle -p automation-service-test

      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall

      mkdir -p $out/share

      # Allow test failures without aborting the build so we can
      # harvest logs and save status as a file
      set +e

      # Confirm what we're about to serve (see TEST_APP_ROOT below)
      ls -l "${automation-service-ui}/ui/index.js" || true

      # run-tests.mjs: starts http-server for the app, waits for it to
      # come up, runs the test bundle, tears the server down, and exits
      # with the bundle's exit code.
      #
      # The app is served from the automation-service-ui derivation
      # output rather than this source tree: ui/index.js and the
      # sass-compiled css are gitignored build artifacts, so they only
      # exist in the built app (locally this happens to work because
      # build artifacts are lying around, but not in a clean nix build)
      PLAYWRIGHT_CHROMIUM_EXECUTABLE_PATH=${pkgs.chromium}/bin/chromium \
      PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD=1 \
      TEST_APP_ROOT=${automation-service-ui}/ui \
        node test/run-tests.mjs 2>&1 | tee .test-log.txt

      status=''${PIPESTATUS[0]}

      set -e

      # Normalize to 0/1 and save for later jobs
      if [ "$status" -eq 0 ]; then
        echo 0 > .test-exit-code
      else
        echo 1 > .test-exit-code
      fi

      # Ship status/log for later use (same pattern as backend)
      cp -v .test-exit-code $out/share/test-exit-code || echo 1 > $out/share/test-exit-code
      [ -f .test-log.txt ] && cp -v .test-log.txt $out/share/test-log.txt || true

      # Allure results are written by the test bundle itself
      # (Test.Spec.Reporter.Allure); the publish-reports job consumes these
      cp -r allure-results $out/share/

      runHook postInstall
    '';
  };
in
  drv
