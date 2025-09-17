{ pkgs, src, overrides ? ../. }:

let
  hl = pkgs.haskell.lib;

  # Customize the Haskell package set if needed
  haskellPackages = pkgs.haskell.packages.ghc964.override {
    overrides = overrides;
  };

  base = haskellPackages.callCabal2nix "automation-service" src { };
  baseNoDocs = hl.dontHaddock base;

  drv = hl.overrideCabal baseNoDocs (old: {
    doCheck = true;

    checkPhase = ''
      runHook preCheck
      set +e
      ./Setup test --show-details=streaming \
        --test-option=--color=always \
        --test-option=--xml=report.xml \
        2>&1 | tee .test-log.txt
      status=$\{PIPESTATUS[0]\}
      echo "$status" > .test-exit-code
      set -e
      runHook postCheck
    '';

    installPhase = ''
      runHook preInstall
      mkdir -p "$out/share/junit"
      cp -v .test-exit-code "$out/share/junit/test-exit-code" || echo 1 > "$out/share/junit/test-exit-code"
      [ -f .test-log.txt ] && cp -v .test-log.txt "$out/share/junit/test-log.txt" || true
      if [ -f report.xml ]; then
        cp -v report.xml "$out/share/junit/report.xml"
      else
        cat > "$out/share/junit/report.xml" <<'EOF'
<?xml version="1.0" encoding="UTF-8"?>
<testsuite name="automation-service" tests="0" failures="0" errors="1" time="0">
  <testcase classname="automation-service" name="test-run">
    <error message="Test run failed before producing JUnit XML. See test-log.txt." />
  </testcase>
</testsuite>
EOF
      fi
      runHook postInstall
    '';
  });
in
  drv
