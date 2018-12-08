{ compiler   ? "ghc844"
, haddock    ? false
, test       ? false
, benchmarks ? false
, nixpkgs ? import <nixpkgs> {}
}:
with builtins;
let
  lib         = nixpkgs.haskell.lib;
  callPackage = nixpkgs.haskell.packages.${compiler}.callPackage;

  doHaddock = if haddock
    then lib.doHaddock
    else lib.dontHaddock;
  doTest = if test
    then lib.doCheck
    else lib.dontCheck;
  doBench = if benchmarks
    then lib.doBenchmark
    else nixpkgs.lib.id;

  myPackage = doHaddock(doTest(doBench(
    callPackage ./wsjtx-udp.nix {}
    )));
in { inherit myPackage; }

