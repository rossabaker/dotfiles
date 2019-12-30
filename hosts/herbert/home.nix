{ pkgs, ... }:

{
  imports = [
    (import ../../modules/home.nix { inherit pkgs; })
    (import ../../modules/desktop.nix { inherit pkgs; })
    (import ../../modules/work)
  ];

  programs.git.signing = {
    key = "0x0CACC3F315F745F1";
    signByDefault = true;
  };
}
