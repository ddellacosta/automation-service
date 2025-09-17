{
  description = "automation-service";

  inputs = {
    # updated 2024-10-20
    # # for PureScript 0.15.11
    # nixpkgs.url = "nixpkgs/9957cd48326fe8dbd52fdc50dd2502307f188b0d";
    # PureScript 0.15.15
    nixpkgs.url = "nixpkgs/4eb33fe664af7b41a4c446f87d20c9a0a6321fa3";
    flake-utils.url = "github:numtide/flake-utils";
    mkSpagoDerivation.url = "github:jeslie0/mkSpagoDerivation";
    ps-overlay.url = "github:thomashoneyman/purescript-overlay";
    purescript-registry = {
      url = "github:purescript/registry";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, mkSpagoDerivation, ps-overlay, purescript-registry }:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ mkSpagoDerivation.overlays.default
                       ps-overlay.overlays.default ];
        };
        t = pkgs.lib.trivial;
        hl = pkgs.haskell.lib;
        haskellPackages = pkgs.haskell.packages.ghc964;

        name = "automation-service";

        node_version = pkgs.nodejs_22;

        automation-service-npm-deps =
          pkgs.buildNpmPackage {
            name = "automation-service-npm-deps";
            # prefetch-npm-deps package-lock.json
            npmDepsHash = "sha256-4sWlMyeFAu/4FkWVqANyJiyQbub5WZWB6bok3ZFav00=";
            src = ./ui;
            nodejs = node_version;
            # need this for spago and logging
            makeCacheWriteable = true;
            dontNpmBuild = true;

            env = {
              PUPPETEER_SKIP_DOWNLOAD = true;
            };

            installPhase = ''
              runHook preInstall

              mkdir -p $out/lib
              cp -r node_modules $out/lib

              runHook postInstall
            '';
          };

        # Frontend build (no tests)
        automation-service-ui-build =
          pkgs.mkSpagoDerivation {
            spagoYaml = ./ui/spago.yaml;
            spagoLock = ./ui/spago.lock;
            src = ./ui;

            nativeBuildInputs = [
              automation-service-npm-deps
              node_version
              pkgs.esbuild
              pkgs.purs
              pkgs.spago-unstable
            ];

            buildPhase = ''
              runHook preBuild

              ln -sf ${automation-service-npm-deps}/lib/node_modules ./node_modules

              # Build the application bundle (no tests)
              spago bundle -p automation-service

              cd css
              npx sass .
              cd ..

              ls -al

              runHook postBuild
            '';

            installPhase = ''
              runHook preInstall
              mkdir -p $out/ui
              cp -r . $out/ui/.
              runHook postInstall
            '';
          };

        # Frontend test runner
        automation-service-ui-test-runner =
          pkgs.mkSpagoDerivation {
            spagoYaml = ./ui/spago.yaml;
            spagoLock = ./ui/spago.lock;
            src = ./ui;

            nativeBuildInputs = [
              automation-service-npm-deps
              node_version
              pkgs.chromium
              pkgs.esbuild
              pkgs.purs
              pkgs.spago-unstable
            ];

            buildPhase = ''
              runHook preBuild

              ln -sf ${automation-service-npm-deps}/lib/node_modules ./node_modules
              cp node_modules/mocha/mocha.js node_modules/mocha/mocha.css test/browser/

              # Build bundle
              spago bundle -p automation-service-test

              runHook postBuild
            '';

            installPhase = ''
              runHook preInstall

              # kinda weird to do this in the installPhase but ¯\_(ツ)_/¯
              ./node_modules/.bin/mocha-headless-chrome \
                -t 60000 \
                -e ${pkgs.chromium}/bin/chromium \
                -a no-sandbox \
                -a disable-setuid-sandbox \
                -a allow-file-access-from-files \
                -r json \
                -o test_output.json \
                -f test/browser/index.html

              mkdir -p $out/ui-test
              cp test_output.json $out/ui-test/

              runHook postInstall
            '';
          };

        haskellOverrides = self: super: {
          astro = hl.dontCheck (hl.markUnbroken super.astro);

          # https://github.com/stoeffel/tasty-test-reporter/pull/13
          tasty-test-reporter = self.callCabal2nix "tasty-test-reporter"
            (pkgs.fetchFromGitHub {
              owner = "jberthold";
              repo = "tasty-test-reporter";
              rev = "3abbfa942553f0986393af44eda0cb69bcaba6ad";
              # ref = "bump-upper-bounds-and-enable-tasty-1.5";
              sha256 = "sha256-lyOmsC4nm5ofmRrUawkXFERzyU0WERlZ3x+g/7l7sCM=";
            }) {};
        };

        # Backend application builder function
        automation-service = devTools:
          let
            addBuildTools = (t.flip hl.addBuildTools) (devTools ++ [
              automation-service-ui-build
              pkgs.zlib
            ]);
          in
            haskellPackages.developPackage {
              root = ./.;
              name = name;
              returnShellEnv = !(devTools == []);

              overrides = haskellOverrides;

              modifier = (t.flip t.pipe) [
                addBuildTools
                hl.dontHaddock
                hl.dontCheck # Don't run tests during build
                hl.enableExecutableProfiling
                (drv: hl.overrideCabal drv (attrs: {
                  configureFlags = [
                    "--ghc-options=-fprof-auto -fno-prof-count-entries"
                  ];
                }))
              ];
            };

        # New: a pure derivation that runs Haskell tests and emits report.xml into the output
        backend-junit-report =
          import ./nix/backend-junit-report.nix {
            inherit pkgs;
            src = ./.;
            overrides = haskellOverrides;
          };

      in {
        packages = {
          automation-service = automation-service [ ];
          backend-junit-report = backend-junit-report;

          automation-service-npm-deps = automation-service-npm-deps;
          automation-service-ui = automation-service-ui-build;
          automation-service-ui-test-runner = automation-service-ui-test-runner;

          default = pkgs.dockerTools.buildImage {
            name = "automation-service";
            created = "now";

            extraCommands = ''
              mkdir ./app
              chmod 755 ./app

              cp -r ${automation-service-ui-build}/ui ./app/ui
            '';

            copyToRoot = pkgs.buildEnv {
              name = "image-root";
              paths = [
                pkgs.bashInteractive
                pkgs.busybox
                pkgs.coreutils
                pkgs.vim
                self.packages.${system}.automation-service
              ];
              pathsToLink = [
                "/bin"
              ];
            };

            config = {
              WorkingDir = "/app";
 
              Cmd = [ "/bin/automation-service" ];

              Volumes = {
                "/app" = {};
              };
            };
          };
        };

        devShell = automation-service (with haskellPackages; [
          cabal-fmt
          cabal-install
          ghc-events
          ghcid
          haskell-language-server
          hlint
          node_version
          pkgs.allure
          pkgs.chromium
          pkgs.esbuild
          pkgs.jq
          pkgs.libxml2 # for xmllint
          pkgs.lua
          pkgs.mosquitto
          pkgs.prefetch-npm-deps
          pkgs.purs
          pkgs.skopeo # for calculating sha256 of docker image
          pkgs.spago-unstable
          pkgs.watchexec
          stylish-haskell
          # threadscope # marked as broken :-(
        ]);
      });
}
