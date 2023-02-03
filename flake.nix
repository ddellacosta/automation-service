#
# Based originally on this:
#  https://magnus.therning.org/2022-03-13-simple-nix-flake-for-haskell-development.html
#
{
  description = "actions-service";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    # flake-utils.lib.eachDefaultSystem (system:
    # flake-utils.lib.eachSystem ["x86_64-linux" "x86_64-darwin" "aarch64-darwin"] (system:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
      with nixpkgs.legacyPackages.${system};
      let
        nixos-lib = nixpkgs + "/nixos/lib";
        t = lib.trivial;
        hl = haskell.lib;
        haskellPackages = haskell.packages.ghc925;

        name = "actions-service";

        tests = nixos-lib.runTest {
          imports = [ ./test.nix ];
          hostPkgs = nixpkgs;  # the Nixpkgs package set used outside the VMs
          defaults.services.foo.package = mypkg;
        };

        project = devTools: # [1]
          let
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
                # hl.enableStaticLibraries
                # hl.justStaticExecutables
                # hl.disableLibraryProfiling
                # hl.disableExecutableProfiling
                # hl.dontCheck
              ];
            };

      in {
        packages.pkg = project []; # [3]

        #defaultPackage = self.packages.${system}.pkg;

        defaultPackage = pkgs.dockerTools.buildImage {
          name = "actions-service";
          
          #runAsRoot = ''
          #  #!${pkgs.runtimeShell}
          #  mkdir -p /app
          #'';

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
            Cmd = [ "/bin/actions-service" ];
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
        ]);
      });
}
