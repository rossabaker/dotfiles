{ pkgs, ... }:

{
  imports = [
    (import ../../config/home.nix { dpi = 96; pkgs = pkgs; })
    (import ../../modules/work)
  ];

  programs.git.signing = {
    key = "0x0CACC3F315F745F1";
    signByDefault = true;
  };
}
