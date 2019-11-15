{ pkgs, ... }:

{
  imports = [
    (import ../../config/home-linux.nix {
      dpi = 172;
      pkgs = pkgs;
    })
  ];
}
