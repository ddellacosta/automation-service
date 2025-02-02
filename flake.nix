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
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
      with nixpkgs.legacyPackages.${system};
      let
        t = lib.trivial;
        hl = haskell.lib;
        haskellPackages = haskell.packages.ghc964;

        name = "automation-service";

        project = devTools:
          let
            addBuildTools = (t.flip hl.addBuildTools) (devTools ++ [
              pkgs.purescript
              pkgs.nodejs_22
              pkgs.esbuild
              pkgs.sass
              zlib
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

        #
        # # should I bump this? - 2023-10-19
        #
        # last bumped 2024-10-20
        # - imageDigest = "sha256:31b808456afccc2a419507ea112e152cf27e9bd2527517b0b6ca8639cc423501";
        # - sha256 = "0bbw3r0civlcm3inj23fq8f25aw63rnaay09qjbrvfjd7pcfbyqn";
        # - finalImageTag = "2.15.0";

        #
        # previous ^ when bumped on 2025-02-01
        #
        #  imageDigest = "sha256:fd7a5c67d396fe6bddeb9c10779d97541ab3a1b2a9d744df3754a99add4046f1";
        #  sha256 = "1izbcfac0cac0jg1q3x834lkdqc0q2dh670bj0zsyadhn0m7f3v6";
        #  finalImageTag = "2.24.9";
        #
        # nixFromDockerHub = pkgs.dockerTools.pullImage {
        #   imageName = "nixos/nix";
        #   imageDigest = "sha256:a91fd903ad94ddde7d41257e0291973771eca41b56473f969e5860814dd5fd2c";
        #   sha256 = "1ni4dwqxr8540gsnn1ch5vdj2f2v4yy35z4dss72vlx8x3j78ac4";
        #   finalImageName = "nixos/nix";
        #   finalImageTag = "2.26.1";
        # };

      in {
        packages = {
          pkg = project [ ];

          default = pkgs.dockerTools.buildImage {
            name = "automation-service";
            created = "now";
            # fromImage = nixFromDockerHub;

            extraCommands = ''
              mkdir ./app
              chmod 755 ./app
            '';

            copyToRoot = pkgs.buildEnv {
              name = "image-root";
              paths = [ self.packages.${system}.pkg ];
              pathsToLink = [ "/bin" ];
            };

            config = {
              WorkingDir = "/app";

              Cmd = [ "/bin/automation-service" ];

              Volumes = {
                "/app" = {};
              };
            };
          };

          raw = self.packages.${system}.pkg;
        };

        devShell = project (with haskellPackages; [
          cabal-fmt
          cabal-install
          haskell-language-server
          hlint
          ghc-events
          ghcid
          jq
          lua
          mosquitto
          skopeo # for calculating sha256 of docker image
          stylish-haskell
          # marked as broken :-(
          # threadscope
          watchexec
        ]);
      });
}
