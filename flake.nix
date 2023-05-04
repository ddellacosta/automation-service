#
# Based originally on this:
#  https://magnus.therning.org/2022-03-13-simple-nix-flake-for-haskell-development.html
#
{
  description = "automation-service";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
      with nixpkgs.legacyPackages.${system};
      let
        t = lib.trivial;
        hl = haskell.lib;
        haskellPackages = haskell.packages.ghc927;

        name = "automation-service";

        project = devTools: # [1]
          let
            # addBuildTools = (t.flip hl.addBuildTools) (devTools ++ [ zlib cacert ]);
            addBuildTools = (t.flip hl.addBuildTools) (devTools ++ [ zlib ]);
          in
            haskellPackages.developPackage {
              # this prevents CHANGELOG/LICENSE/etc. from being found
              # root = lib.sourceFilesBySuffices ./. [ ".cabal" ".hs" ];
              root = ./.;
              name = name;
              returnShellEnv = !(devTools == []);

              modifier = (t.flip t.pipe) [
                addBuildTools
                hl.dontHaddock
              ];
            };

        nixFromDockerHub = pkgs.dockerTools.pullImage {
          imageName = "nixos/nix";
          imageDigest = "sha256:31b808456afccc2a419507ea112e152cf27e9bd2527517b0b6ca8639cc423501";
          sha256 = "0bbw3r0civlcm3inj23fq8f25aw63rnaay09qjbrvfjd7pcfbyqn";
          finalImageName = "nixos/nix";
          finalImageTag = "2.15.0";
        };

      in {
        packages.pkg = project [];

        defaultPackage = pkgs.dockerTools.buildImage {
          name = "automation-service";
          created = "now";
          fromImage = nixFromDockerHub;

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

        devShell = project (with haskellPackages; [
          cabal-fmt
          cabal-install
          haskell-language-server
          hlint
          ghcid
          watchexec
          mosquitto
          jq
          (lua5_4.withPackages(ps: with ps; [ cjson ]))
        ]);
      });
}
