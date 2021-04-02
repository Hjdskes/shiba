{ haskellNix ? import (builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/e7961eee7bbaaa195b3255258f40d5536574eb74.tar.gz") {}
, nixpkgsSrc ? haskellNix.sources.nixpkgs-2009
, nixpkgsArgs ? haskellNix.nixpkgsArgs
, pkgs ? import nixpkgsSrc nixpkgsArgs
}: pkgs.haskell-nix.project {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "shiba";
    src = ./.;
  };
  compiler-nix-name = "ghc8104";
}
