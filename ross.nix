{ config, pkgs, ... }:

{
  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = pkgs: {
      unstable = import <nixos-unstable> {
        config = config.nixpkgs.config;
      };
    };
  };
  
  home.packages = with pkgs; [
    chromium
    google-chrome
    networkmanagerapplet
    rofi
    slack
    spotify
    taffybar
  ];

  home.file = {
    ".config/taffybar/taffybar.hs".source = ./xmonad/taffybar.hs;

    ".emacs.d/init.el".source = ./emacs/init.el;

    ".xmonad/xmonad.hs".source = ./xmonad/xmonad.hs;
    ".xmonad/xmonad-session-rc".source = ./xmonad/xmonad-session-rc;
  };

  # Broken, I think due to https://github.com/NixOS/nixos-channel-scripts/issues/9
  programs.command-not-found.enable = true;

  programs.emacs = {
    enable = true;
    extraPackages = epkgs:
      (with epkgs.melpaStablePackages; [
        better-defaults
        haskell-mode
        magit
        nix-mode
        restart-emacs
        use-package
      ]);
  };

  programs.git = {
    enable = true;
    userName = "Ross A. Baker";
    userEmail = "ross@rossabaker.com";
    aliases = {
      "st" = "status --short";
    };
    ignores = [ "*~" "\#*#" "*.elc" ".\#*" ];
  };
}
