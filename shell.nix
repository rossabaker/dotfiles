{ sources ? import ./nix/sources.nix }:

let
  overlay = _: pkgs: {
    niv = import sources.niv {};
  };
  pkgs = import sources.nixpkgs {
    overlays = [ overlay ];
    config = {};
  };
in
  with pkgs; pkgs.mkShell {
    buildInputs = [
      gnumake
      jq
      niv.niv
      nixpkgs-fmt
    ];
  }
