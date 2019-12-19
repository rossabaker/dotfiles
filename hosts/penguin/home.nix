{ pkgs, ... }:

{
  imports = [
    (import ../../config/home.nix {
      pkgs = pkgs;
    })
  ];
}
