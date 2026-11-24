{
  description = "Bottega: a Racket core with three dialects, disegno, prospettiva and affresco";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";

  outputs = { self, nixpkgs, ... }:
    let
      # ponytail: x86_64-linux only, like disegno; genAttrs over systems when needed.
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      devShells.${system}.default = pkgs.mkShell { packages = [ pkgs.racket ]; };
    };
}
