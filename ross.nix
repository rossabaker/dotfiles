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
    ghc
    gitter
    gnupg
    google-chrome
    ispell
    networkmanagerapplet
    pasystray
    pavucontrol
    ripgrep
    rofi
    sbt
    scala
    slack
    spotifywm
    taffybar
    termite
    xorg.xbacklight
    xsettingsd
  ];

  home.file = {
    ".config/taffybar/taffybar.hs".source = ./xmonad/taffybar.hs;

    ".emacs.d/init.el".source = ./emacs/init.el;

    ".xmonad/xmonad.hs".source = ./xmonad/xmonad.hs;

    ".xsettingsd".source = ./X/xsettingsd;
  };

  # Broken, I think due to https://github.com/NixOS/nixos-channel-scripts/issues/9
  programs.command-not-found.enable = true;

  programs.bash = {
    enable = true;
    shellAliases = {
      spotify = "spotify --force-device-scale-factor=1.75";
    };
  };

  programs.emacs = {
    enable = true;
    package = pkgs.unstable.emacs;
    extraPackages = epkgs:
      (with epkgs.melpaStablePackages; [
        ace-window
        aggressive-indent
        avy
        beacon
        better-defaults
        company
        copy-as-format
        counsel
        counsel-projectile
        diff-hl
        diminish
        dumb-jump
        edit-server
        expand-region
        flycheck
        free-keys
        guru-mode
        haskell-mode
        ialign
        ivy
        know-your-http-well
        magit
        markdown-mode
        material-theme
        multiple-cursors
        nix-mode
        persistent-scratch
        rainbow-delimiters
        scala-mode
        sbt-mode
        shell-pop
        swiper
        which-key
        whitespace-cleanup-mode
        yaml-mode
        yasnippet
      ]) ++ (with epkgs.melpaPackages; [
        avy-zap
        change-inner
        restclient
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
      Type = "simple";
      ExecStart = "${pkgs.stdenv.shell} -l -c 'exec %h/.nix-profile/bin/emacs --fg-daemon'";
      ExecStop = "%h/.nix-profile/bin/emacsclient --eval '(kill-emacs)'";
      Restart = "always";
    };
    Install = {
      WantedBy = [ "default.target" ];
    };
  };

  programs.autorandr = {
    enable = true;
  };

  programs.git = {
    enable = true;
    userName = "Ross A. Baker";
    userEmail = "ross@rossabaker.com";
    aliases = {
      "st" = "status --short";
    };
    ignores = [ "*~" "\#*#" "*.elc" ".\#*" ];
    extraConfig = ''
      [url "git@github.com:"]
      insteadOf = "gh:"
    '';
  };

  xsession = {
    enable = true;

    initExtra = ''
      xsettingsd &

      cbatticon -u 10 -c "systemctl suspend" -l 15 -r 3 &
      nm-applet &
      pasystray &

      taffybar &
    '';

    pointerCursor = {
      package = pkgs.vanilla-dmz;
      name = "Vanilla-DMZ";
      size = 56;
    };

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = haskellPackages: [
        haskellPackages.taffybar
        haskellPackages.xmonad-contrib
        haskellPackages.xmonad-extras
        haskellPackages.xmonad
      ];
    };
  };
}
