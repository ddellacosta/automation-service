# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: CC0-1.0

{
  description = "My haskell application";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # haskellPackages = pkgs.haskellPackages;
        haskellPackages = pkgs.haskell.packages.ghc925;

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

        packageName = "actions-service";
      in {
        packages.${packageName} =
          haskellPackages.callCabal2nix packageName self rec {
            # Dependency overrides go here
          };

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
            paths = [ self.packages.${system}.${packageName} ];
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

        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.haskell-language-server # you must build it with your ghc to work
            ghcid
            cabal-install
            zlib
            watchexec
            mosquitto
          ];
          inputsFrom = builtins.attrValues self.packages.${system};
        };
      });
}
