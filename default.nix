let pkgs = import <nixpkgs> {};
in pkgs.haskellPackages.callPackage ./concurrent-extra.nix {}
