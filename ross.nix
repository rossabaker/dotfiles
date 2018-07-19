{ config, pkgs, ... }:

let
  gitter = with pkgs; callPackage ./gitter.nix {};
  spotifywm = with pkgs; callPackage ./spotifywm.nix {};
in {
  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = pkgs: {
      unstable = import <nixos-unstable> {
        config = config.nixpkgs.config;
      };
    };
  };
  
  home.packages = with pkgs; [
    cbatticon
    chromium
    evince
    firefox
    gitter
    google-chrome
    networkmanagerapplet
    pasystray
    pavucontrol
    rofi
    slack
    spotifywm
    taffybar
    termite
    xsettingsd
  ];

  home.file = {
    ".config/taffybar/taffybar.hs".source = ./xmonad/taffybar.hs;

    ".emacs.d/init.el".source = ./emacs/init.el;

    ".xmonad/xmonad.hs".source = ./xmonad/xmonad.hs;

    ".xprofile".source = ./X/xprofile;
    ".xsettingsd".source = ./X/xsettingsd;    
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
        material-theme
        nix-mode
        restart-emacs
        use-package
      ]);
  };
  
  gtk = {
    enable = true;
    font = {
      name = "Roboto 9.75";
      package = pkgs.roboto;
    };
    iconTheme = {
      name = "Papirus-Adapta-Nokto";
      package = pkgs.papirus-icon-theme;
    };
    theme = {
      name = "Adapta-Nokto";
      package = pkgs.adapta-gtk-theme;
    };
    gtk2.extraConfig = ''
      gtk-cursor-blink = 0
      gtk-key-theme-name = "Emacs"
    '';
    gtk3.extraConfig = {
      gtk-cursor-blink = false;
      gtk-key-theme-name = "Emacs";
    };
  };

  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
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
      Restart = "always";
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
