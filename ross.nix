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

    ".xprofile".source = ./X/xprofile;
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

  systemd.user.services.emacs-daemon = {
    Unit = {
      Description = "Emacs text editor";
      Documentation = "info:emacs man:emacs(1) https://gnu.org/software/emacs/";
    };
    Service = {
      Type = "forking";
      ExecStart = "${pkgs.stdenv.shell} -l -c 'exec %h/.nix-profile/bin/emacs --daemon'";
      ExecStop = "%h/.nix-profile/bin/emacsclient --eval '(kill-emacs)'";
      Restart = "on-failure";
    };
    Install = {
      WantedBy = [ "default.target" ];
    };
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
