{ pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;
  
  home.packages = [
    pkgs.chromium
    pkgs.google-chrome
    pkgs.slack
  ];
}
