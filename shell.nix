{ compiler   ? "ghc844"
, haddock    ? true
, test       ? true
, benchmarks ? false
, nixpkgs    ? import <nixpkgs> {}
}:
with builtins;
let
  default = import ./default.nix  {inherit compiler haddock test benchmarks pkgs;};
in
    if nixpkgs.lib.inNixShell
      then default.myPackage.env
      else default.myPackage


