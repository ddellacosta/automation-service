#
# Based originally on this:
#  https://magnus.therning.org/2022-03-13-simple-nix-flake-for-haskell-development.html
#
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
  };

  outputs = { self, nixpkgs, flake-utils, mkSpagoDerivation, ps-overlay }:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
      # with nixpkgs.legacyPackages.${system};
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
            npmDepsHash = "sha256-6g3rK2KZv5s37axx6S2kP6yIcRD307WWlKHv1egqduY=";
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
              automation-service-npm-deps
              node_version
              pkgs.chromium
              pkgs.esbuild
              pkgs.purs
              pkgs.spago-unstable
              pkgs.which
            ];

            version = "0.1.0";

            buildPhase = ''
              runHook preBuild

              ln -sf ${automation-service-npm-deps}/lib/node_modules ./node_modules
              cp node_modules/mocha/mocha.js node_modules/mocha/mocha.css test/browser/

              npx spago bundle -p automation-service-test

              #  > \Error: Failed to launch the browser process!
              #        > [342:342:0206/014943.932611:FATAL:setuid_sandbox_host.cc(163)]
              # The SUID sandbox helper binary was found, but is not
              # configured correctly. Rather than run without
              # sandboxing I'm aborting now. You need to make sure
              # that /nix/store/499bwk374kxvq6kylfwqgcx70h40zyas-chromium-129.0.6668.100-sandbox/bin/__chromium-suid-sandbox
              # is owned by root and has mode 4755.
              #
              # '--no-sandbox' is the hacky unsafe solution to the above. See e.g.
              # https://github.com/flathub/com.visualstudio.code/issues/223
              npx mocha-headless-chrome \
                -t 60000 \
                -e $(which chromium) \
                -a no-sandbox \
                -a disable-setuid-sandbox \
                -a allow-file-access-from-files \
                -f test/browser/index.html

              # dump out chrome logs to the file chrome_debug.log in
              # the current directory
              # npx mocha-headless-chrome -t 60000 -p raf -e $(which chromium) -a v=1 -a enable-logging -a user-data-dir=./ -a no-sandbox -a disable-setuid-sandbox -a allow-file-access-from-files -f test/browser/index.html
              # cat chrome_debug.log

              spago bundle -p automation-service

              runHook postBuild
            '';

            installPhase = ''
              runHook preInstall

              mkdir -p $out/ui
              cp -r . $out/ui/.

              runHook postInstall
            '';
          };

        automation-service = devTools:
          let
            addBuildTools = (t.flip hl.addBuildTools) (devTools ++ [
              automation-service-ui
              pkgs.zlib
            ]);
          in
            haskellPackages.developPackage {
              # this prevents CHANGELOG/LICENSE/etc. from being found
              # root = lib.sourceFilesBySuffices ./. [ ".cabal" ".hs" ];
              root = ./.;
              name = name;
              returnShellEnv = !(devTools == []);

              # https://hydra.nixos.org/build/225575437
              # https://github.com/ddellacosta/automation-service/issues/8
              overrides = _self: super: {
                astro = hl.dontCheck (hl.markUnbroken super.astro);
              };

              modifier = (t.flip t.pipe) [
                addBuildTools
                hl.dontHaddock
                hl.enableExecutableProfiling
                (drv: hl.overrideCabal drv (attrs: {
                  configureFlags = [
                    "--ghc-options=-fprof-auto -fno-prof-count-entries"
                  ];
                }))
              ];


            };

      in {
        packages = {
          automation-service = automation-service [ ];
          automation-service-npm-deps = automation-service-npm-deps;
          automation-service-ui = automation-service-ui;

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
          pkgs.chromium
          pkgs.esbuild
          pkgs.jq
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
