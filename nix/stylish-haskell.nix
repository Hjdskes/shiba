{ pkgs ? import ../haskell.nix }:

let
  hsPkgs = pkgs.haskell-nix.stackProject {
    compiler-nix-name = "ghc8104";
    modules = [];
    src = pkgs.fetchFromGitHub {
      owner = "jaspervdj";
      repo = "stylish-haskell";
      # 0.12.2.0
      rev = "84770e33bb6286c163c3b2b10fa98d264f6672b8";
      sha256 = "1jc844x8v93xgnl6fjrk31gb2zr96nxbqmgmnc4hdfhfyajh5y7w";
    };
  };
in
  hsPkgs.stylish-haskell.components.exes.stylish-haskell