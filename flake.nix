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
            npmDepsHash = "sha256-LxT+rS1TtlAw1iiClqXisTijXl8idvMQLCdOiaWuMVM=";
            src = ./.;
            nodejs = node_version;
            # need this for spago and logging
            makeCacheWriteable = true;
            dontNpmBuild = true;

            installPhase = ''
              runHook preInstall

              mkdir -p $out/lib
              cp -r node_modules $out/lib

              runHook postInstall
            '';
          };

        automation-service-ui-npm-deps =
          pkgs.buildNpmPackage {
            name = "automation-service-ui-npm-deps";
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

        automation-service-ui =
          pkgs.mkSpagoDerivation {
            spagoYaml = ./ui/spago.yaml;
            spagoLock = ./ui/spago.lock;
            src = ./ui;

            nativeBuildInputs = [
              automation-service-ui-npm-deps
              node_version
              pkgs.esbuild
              pkgs.purs
              pkgs.spago-unstable
            ];

            buildPhase = ''
              runHook preBuild

              ln -sf ${automation-service-ui-npm-deps}/lib/node_modules ./node_modules

              # Build the application bundle (no tests)
              spago bundle -p automation-service

              cd css
              npx sass .
              cd ..

              runHook postBuild
            '';

            installPhase = ''
              runHook preInstall
              mkdir -p $out/ui
              cp -r . $out/ui/.
              runHook postInstall
            '';
          };

        automation-service-ui-test =
          import ./nix/frontend-test.nix {
            inherit automation-service-ui-npm-deps node_version pkgs;
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

        automation-service = devTools:
          let
            addBuildTools = (t.flip hl.addBuildTools) (devTools ++ [
              automation-service-ui
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

        automation-service-test =
          import ./nix/backend-test.nix {
            inherit automation-service-npm-deps node_version pkgs; 
            overrides = haskellOverrides;
          };

        allure-site-generator = devTools:
          let
            addBuildTools = (t.flip hl.addBuildTools) (devTools ++ [
              pkgs.zlib
            ]);
          in
            haskellPackages.developPackage {
              root = ./generate-allure-site/.;
              name = "generate-allure-site";
              returnShellEnv = !(devTools == []);

              overrides = haskellOverrides;

              modifier = (t.flip t.pipe) [
                addBuildTools
                hl.dontHaddock
                hl.enableExecutableProfiling
              ];
            };

      in {
        packages = {
          automation-service-npm-deps = automation-service-npm-deps;
          automation-service-ui = automation-service-ui;
          automation-service-ui-test = automation-service-ui-test;

          automation-service = automation-service [ ];
          automation-service-test = automation-service-test;

          allure-site-generator = allure-site-generator [];

          default = pkgs.dockerTools.buildImage {
            name = "automation-service";
            created = "now";

            extraCommands = ''
              mkdir ./app
              chmod 755 ./app

              cp -r ${automation-service-ui}/ui ./app/ui
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
