{ pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;
  
  home.packages = [
    pkgs.chromium
    pkgs.google-chrome
    pkgs.slack
    pkgs.spotify
  ];

  home.file = {
    ".xmonad/xmonad.hs".source = ./xmonad/xmonad.hs;
  };
}
