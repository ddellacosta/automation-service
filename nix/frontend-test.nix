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

      # Confirm the built app artifacts exist (see the cp below)
      ls -l "${automation-service-ui}/ui/index.js" || true

      # Bring the built app artifacts into the served tree: index.js and
      # the sass-compiled css are gitignored, so they exist only in the
      # automation-service-ui build output, not in this source tree.
      cp ${automation-service-ui}/ui/index.js .
      cp ${automation-service-ui}/ui/css/*.css css/
      cp ${automation-service-ui}/ui/css/*.css.map css/ || true

      # run-tests.mjs: starts http-server for the app, waits for it to
      # come up, runs the test bundle, tears the server down, and exits
      # with the bundle's exit code. It serves the CWD, which now
      # contains both the tracked source files and the built artifacts.
      PLAYWRIGHT_CHROMIUM_EXECUTABLE_PATH=${pkgs.chromium}/bin/chromium \
      PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD=1 \
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

    # The build sandbox has no system fonts or fontconfig; without them
    # chromium renders text with zero-size bounding boxes, so elements are
    # laid out but Playwright's visible-state waits (waitForSelector) time
    # out with "locator resolved to hidden"
    FONTCONFIG_FILE = pkgs.makeFontsConf {
      fontDirectories = [ pkgs.dejavu_fonts ];
    };
  };
in
  drv
