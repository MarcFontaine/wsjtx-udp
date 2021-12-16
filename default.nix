{ compiler   ? "ghc901"
, haddock    ? false
, test       ? false
, benchmarks ? false
, pkgs       ? import <nixpkgs> {}
}:
with builtins;
let

  lib        = pkgs.haskell.lib;
  setTargets = package:
    (let

      doHaddock = if haddock    then lib.doHaddock   else lib.dontHaddock;
      doTest    = if test       then lib.doCheck     else lib.dontCheck;
      doBench   = if benchmarks then lib.doBenchmark else x: x;
     in
      doHaddock(doTest(doBench package))
    );

  wsjtx-udp = setTargets (pkgs.haskell.packages.${compiler}.callPackage ./wsjtx-udp.nix {});

in
  { inherit wsjtx-udp; }
