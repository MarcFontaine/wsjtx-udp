{
  description = "wsjtx-udp as a flake";
  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-23.11;
  outputs = { self, nixpkgs }:
    let
      pkgs = import nixpkgs { system = "x86_64-linux"; };
    in
    {
    packages.x86_64-linux.wsjtx-udp = (import ./default.nix {inherit pkgs;}).wsjtx-udp;
    defaultPackage.x86_64-linux = self.packages.x86_64-linux.wsjtx-udp;
    overlay = final: prev: { wsjtx-udp = self.packages.x86_64-linux.wsjtx-udp; };
  };
}
