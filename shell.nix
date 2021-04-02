{ pkgs ? import (builtins.fetchTarball "https://github.com/nixos/nixpkgs/tarball/bda379c6c0fd66bcb1a676c7352e86cbd39d95b8") {} }:

let
  hsPkgs = import ./default.nix {};
  terraform = pkgs.terraform_0_14.withPlugins(p: [ p.aws ]);
in hsPkgs.shellFor {
  # Include only the *local* packages of your project.
  packages = ps: with ps;
    [ shiba ];

  # Builds a Hoogle documentation index of all dependencies,
  # and provides a "hoogle" command to search the index.
  withHoogle = true;

  # You might want some extra tools in the shell (optional).

  # Some common tools can be added with the `tools` argument
  tools = { cabal = "3.2.0.0"; hlint = "2.2.11"; };
  # See overlays/tools.nix for more details

  # Some you may need to get some other way.
  buildInputs = with hsPkgs.haskellPackages;
    [ terraform ];

  # Prevents cabal from choosing alternate plans, so that
  # *all* dependencies are provided by Nix.
  exactDeps = true;
}