{ automation-service-npm-deps
, node_version
, overrides
, pkgs
, src ? ../.
}:

let
  hl = pkgs.haskell.lib;

  # Customize the Haskell package set if needed
  haskellPackages = pkgs.haskell.packages.ghc964.override {
    overrides = overrides;
  };

  base = haskellPackages.callCabal2nix "automation-service" src { };
  baseNoDocs = hl.dontHaddock base;

  drv_ = hl.overrideCabal baseNoDocs (old: {
    doCheck = true;

    checkPhase = ''
      runHook preCheck

      # Allow test failures without aborting the build so we can
      # harvest logs and save status as a file
      set +e

      ./Setup test --show-details=streaming \
        --test-option=--color=always \
        --test-option=--xml=report.xml \
        2>&1 | tee .test-log.txt

      status=''${PIPESTATUS[0]}
      set -e

      # Normalize to 0/1 and save for later jobs
      if [ "$status" -eq 0 ]; then
        echo 0 > .test-exit-code
      else
        echo 1 > .test-exit-code
      fi

      runHook postCheck
    '';

    installPhase = ''
      runHook preInstall

      mkdir -p $out/share

      cp -v .test-exit-code $out/share/test-exit-code || echo 1 > $out/share/test-exit-code
      [ -f .test-log.txt ] && cp -v .test-log.txt $out/share/test-log.txt || true

      if [ -f report.xml ]; then
        cp -v report.xml $out/share/report.xml
      else
        cat > "$out/share/report.xml" <<'EOF'
<?xml version="1.0" encoding="UTF-8"?>
<testsuite name="automation-service" tests="0" failures="0" errors="1" time="0">
  <testcase classname="automation-service" name="test-run">
    <error message="Test run failed before producing JUnit XML. See test-log.txt." />
  </testcase>
</testsuite>
EOF
      fi

      ln -sf ${automation-service-npm-deps}/lib/node_modules ./node_modules
      node test/convert-junit-to-allure.mjs report.xml
      cp -r allure-results $out/share/

      runHook postInstall
    '';
  });

  # Ensure Node is on PATH for build/check/install phases
  drv = drv_.overrideAttrs (old: {
    nativeBuildInputs = (old.nativeBuildInputs or []) ++ [ node_version ];
  });
in
  drv
