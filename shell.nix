{ compiler   ? "ghc863"
, haddock    ? true
, test       ? true
, benchmarks ? false
, pkgs       ? import <nixpkgs> {}
}:
with builtins;
let
  default = import ./default.nix  {inherit compiler haddock test benchmarks pkgs;};
in
    if pkgs.lib.inNixShell
      then default.wsjtx-udp.env
      else default.wsjtx-udp


