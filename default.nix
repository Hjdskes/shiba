{ pkgs ? import ./haskell-nix.nix }:

pkgs.haskell-nix.project {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "shiba";
    src = ./.;
  };
  compiler-nix-name = "ghc8104";
}
