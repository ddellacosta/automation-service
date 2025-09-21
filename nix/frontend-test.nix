{ automation-service-ui-npm-deps
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
      cp node_modules/mocha/mocha.js node_modules/mocha/mocha.css test/browser/
      spago bundle -p automation-service-test

      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall

      mkdir -p $out/share

      # Allow test failures without aborting the build so we can
      # harvest logs and save status as a file
      set +e

      #
      # kinda weird to do this in the installPhase but ¯\_(ツ)_/¯
      #
      # added these three when I kept getting failures when tests
      # failed, also bumped timeout from 60000 to 120000
      #
      #  -a disable-dev-shm-usage \
      #  -a no-zygote \
      #  -a single-process \
      #
      ./node_modules/.bin/mocha-headless-chrome \
        -t 120000 \
        -e ${pkgs.chromium}/bin/chromium \
        -a no-sandbox \
        -a disable-setuid-sandbox \
        -a disable-dev-shm-usage \
        -a no-zygote \
        -a single-process \
        -a allow-file-access-from-files \
        -r json \
        -o test-output.json \
        -f test/browser/index.html \
        2>&1 | tee .test-log.txt

      status=''${PIPESTATUS[0]}
      set -e

      ls -al
      ls -al test/browser

      # Normalize to 0/1 and save for later jobs
      if [ "$status" -eq 0 ]; then
        echo 0 > .test-exit-code
      else
        echo 1 > .test-exit-code
      fi

      # Ship status/log for later use (same pattern as backend)
      cp -v .test-exit-code $out/share/test-exit-code || echo 1 > $out/share/test-exit-code
      [ -f .test-log.txt ] && cp -v .test-log.txt $out/share/test-log.txt || true

      node test/convert-mocha-to-allure.mjs test-output.json

      cp test-output.json $out/share/
      cp -r allure-results $out/share/

      runHook postInstall
    '';
  };
in
  drv
