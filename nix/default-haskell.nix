{
  pkgs       ? import <nixpkgs> {}
}:
with pkgs;
let
#  haskell = import ./haskell.nix {inherit pkgs;};
  haskell = import ./haskell.nix {pkgs=pkgs.pkgsCross.musl64; };

  pkgSet = haskell.mkCabalProjectPkgSet {
    plan-pkgs = import ./cabal-env-pkgs.nix;
    pkg-def-extras = [];
    modules = [
    {
       packages."wsjtx-udp".components.exes."wsjtx-dump-udp".configureFlags =
        [
      "--disable-executable-dynamic"
      "--disable-shared"
      "--ghc-option=-optl=-pthread"
      "--ghc-option=-optl=-static"
      "--ghc-option=-optl=-L${pkgsStatic.gmp6}/lib"
      "--ghc-option=-optl=-L${pkgsStatic.libffi}/lib"
      ];
      }
    ];
  };

in pkgSet.config.hsPkgs // { _config = pkgSet.config; }
