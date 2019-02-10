{ haddock    ? false
, test       ? false
, benchmarks ? false
}:
with builtins;
let
  nixpkgs-rev = "08d245eb31a3de0ad73719372190ce84c1bf3aee";
  nixpkgs-url = "https://github.com/NixOS/nixpkgs-channels/archive/${nixpkgs-rev}.tar.gz";
  pkgs        = import (builtins.fetchTarball { url=nixpkgs-url; }) {};

  lib       = pkgs.haskell.lib;
  setTargets  = package:
    (let

      doHaddock = if haddock    then lib.doHaddock   else lib.dontHaddock;
      doTest    = if test       then lib.doCheck     else lib.dontCheck;
      doBench   = if benchmarks then lib.doBenchmark else x: x;
     in
      doHaddock(doTest(doBench package))
    );


  haskellPackages = pkgs.haskellPackages.override {
      overrides = self: super: {
        binary-parsers = lib.dontCheck super.binary-parsers;
  };
  };

  setStaticFlags = this: lib.overrideCabal this
    (drv: {
        enableSharedExecutables = false;
        enableSharedLibraries = false;
        configureFlags = [
           "--ghc-option=-optl=-static"
           "--ghc-option=-optl=-pthread"
           "--ghc-option=-optl=-L${pkgs.gmp6.override { withStatic = true; }}/lib"
           "--ghc-option=-optl=-L${pkgs.zlib.static}/lib"
           "--ghc-option=-optl=-L${pkgs.glibc.static}/lib"
          ];
    });

  wsjtx-udp-static = setTargets (setStaticFlags (haskellPackages.callPackage ./wsjtx-udp.nix {})) ;

in
  { inherit wsjtx-udp-static; }
