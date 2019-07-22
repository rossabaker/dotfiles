{ config, pkgs, ... }:

{
  programs.git = {
    enable = true;
    userEmail = "ross@rossabaker.com";
    userName = "Ross A. Baker";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
