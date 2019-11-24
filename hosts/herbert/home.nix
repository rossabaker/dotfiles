{ pkgs, ... }:

{
  imports = [
    (import ../../config/home-linux.nix { dpi = 96; pkgs = pkgs; })
    (import ../../modules/work.nix)
  ];

  programs.git.signing = {
    key = "0x0CACC3F315F745F1";
    signByDefault = true;
  };
}
