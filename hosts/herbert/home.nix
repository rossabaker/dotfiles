{ pkgs, ... }:

{
  imports = [
    (import ../../config/home-linux.nix { dpi = 90; pkgs = pkgs; })
    (import ../../modules/work.nix)
  ];
}
