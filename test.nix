import "${hostPkgs}/nixos/tests/make-test-python.nix" ({ pkgs, ...}: {
  system = "x86_64-linux";

  # One or more machines:
  nodes =
    { server = { config, pkgs, ... }: {

    #  machine =
    #    { config, pkgs, ... }: { … };
    #  machine2 =
    #    { config, pkgs, ... }: { … };
    #  …

    };

  testScript =
    ''
      cabal test --test-show-details=always --test-options "--color=always"
    '';
    };
})
