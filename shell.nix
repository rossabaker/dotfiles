{ sources ? import ./nix/sources.nix }:

let
  pkgs = import sources.nixpkgs {};
in
  with pkgs; pkgs.mkShell {
    buildInputs = [
      fd
      gnumake
      jq
      niv
      nixpkgs-fmt
    ];
  }
