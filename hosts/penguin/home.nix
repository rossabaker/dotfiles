{ pkgs, ... }:

{
  imports = [
    (import ../../home.nix {
      pkgs = pkgs;
    })
  ];
}
