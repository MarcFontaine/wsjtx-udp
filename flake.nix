{
  description = "flake for wsjtx-udp";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
  outputs =
    { self, nixpkgs }:
    let
      pkgs = import nixpkgs { system = "x86_64-linux"; };
    in
    {
      formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.nixfmt-rfc-style;
      packages.x86_64-linux.wsjtx-udp = (import ./default.nix { inherit pkgs; }).wsjtx-udp;
      defaultPackage.x86_64-linux = self.packages.x86_64-linux.wsjtx-udp;
      overlay = final: prev: { wsjtx-udp = self.packages.x86_64-linux.wsjtx-udp; };
    };
}
