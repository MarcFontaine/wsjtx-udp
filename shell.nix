{ compiler   ? "ghc844"
, haddock    ? true
, test       ? true
, benchmarks ? false
, pkgs    ? import <nixpkgs> {}
}:
with builtins;
let
  default = import ./default.nix {inherit compiler haddock test;};
in
    if pkgs.lib.inNixShell
      then default.myPackage.env
      else default.myPackage


