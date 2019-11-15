{ pkgs, ... }:

{
  imports = [
    (import ../../config/home-linux.nix { pkgs = pkgs; })
    (import ../../modules/work.nix)
  ];
}
