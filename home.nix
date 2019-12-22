{ pkgs,
  dpi ? 96,
  ... }:

let
  sources = import ./nix/sources.nix;
  all-hies = import sources.all-hies {};
  niv = import sources.niv {};
in rec {
  imports = [
    ./modules/chrome
    ./modules/emacs
    ./modules/i3
  ];

  fonts.fontconfig.enable = true;

  gtk = {
    enable = true;
    iconTheme = {
      package = pkgs.qogir-icon-theme;
      name = "Qogir";
    };
    theme = {
      name = "Qogir-Tomorrow-dark";
      package = pkgs.qogir-tomorrow-theme;
    };
  };

  home = {
    file = {
      ".config/direnv/direnvrc".source = ./config/direnv/direnvrc;
      ".config/polybar/dynamic.config".text = ''
         [bar/main]
         dpi = ${toString dpi}
         height = ${toString (dpi / 4)}
      '';
      ".xsettingsd".text = ''
        Xft/DPI ${toString (dpi * 1024)}
      '';
    };

    keyboard.options = [
      "ctrl:nocaps"
    ];

    packages = [
      (all-hies.selection { selector = p: { inherit (p) ghc865; }; })
      niv.niv
      pkgs.aspell
      pkgs.aspellDicts.en
      pkgs.bashInteractive
      pkgs.dhall
      pkgs.direnv
      pkgs.gitter
      pkgs.gnome3.dconf # gtk doesn't configure without it
      pkgs.gnupg
      pkgs.hasklig
      pkgs.jq
      pkgs.material-design-icons
      pkgs.sbt-extras
      pkgs.siji
      pkgs.slack
      pkgs.spotify
      pkgs.unifont
      pkgs.xmonad-log
      pkgs.xorg.xbacklight
      pkgs.xsecurelock
      pkgs.xsettingsd
      pkgs.xss-lock
    ];

    sessionVariables = {
      ALTERNATE_EDITOR = "";
      EDITOR = "emacsclient -t";
      VISUAL = "emacsclient -c";
    };
  };

  nixpkgs.config = import ./config/nixpkgs/config.nix;
  nixpkgs.overlays = (import ./config/nixpkgs/config.nix).overlays;

  programs = {
    autorandr = {
      enable = true;
    };

    bash = {
      enable = true;
      initExtra = ''
        vterm_prompt_end(){
          case $TERM in
            xterm-*)
              printf "\e]51;A$(whoami)@$(hostname):$(pwd)\e\\"
          esac
        }
        PS1='$(vterm_prompt_end)'"$PS1"
      '';

      sessionVariables = {
        NIX_PATH = "$HOME/.nix-defexpr/channels\${NIX_PATH:+:}$NIX_PATH";
        # Hack aronud https://github.com/rycee/home-manager/issues/423 for termite
        TERMINFO_DIRS="$HOME/.nix-profile/share/terminfo:/lib/terminfo";
        # https://github.com/NixOS/nixpkgs/issues/38991#issuecomment-496332104
        LOCALE_ARCHIVE_2_11=''$(nix-build --no-out-link "<nixpkgs>" -A glibcLocales)/lib/locale/locale-archive'';
        LOCALE_ARCHIVE_2_27=''$(nix-build --no-out-link "<nixpkgs>" -A glibcLocales)/lib/locale/locale-archive'';
        LOCALE_ARCHIVE="/usr/bin/locale";
      };
    };

    direnv = {
      enable = true;
      enableBashIntegration = true;
    };

    git = {
      enable = true;
      extraConfig = {
        url = {
          "git@github.com" = { insteadOf = "gh"; } ;
        };
      };
      ignores = [
        ".bloop"
        ".metals"
        ".direnv.d/env-*"
      ];
      userEmail = "ross@rossabaker.com";
      userName = "Ross A. Baker";
    };

    # Let Home Manager install and manage itself.
    home-manager.enable = true;

    rofi = {
      enable = true;
      font = "sans-serif 12";
      extraConfig = ''rofi.dpi: ${toString dpi}'';
      theme = ./config/rofi/tomorrow-night.rasi;
    };

    termite = {
      enable = true;
      cursorBlink = "off";
      cursorColor = "#cc6666";
      foregroundColor = "#c5c8c6";
      backgroundColor = "#1d1f21";
      highlightColor = "#373b41";
      colorsExtra = ''
        color0  = #1d1f21
        color1  = #cc6666
        color2  = #b5bd68
        color3  = #f0c674
        color4  = #81a2be
        color5  = #b294bb
        color6  = #8abeb7
        color7  = #c5c8c6
        color8  = #1d1f21
        color9  = #cc6666
        color10 = #b5bd68
        color11 = #f0c674
        color12 = #81a2be
        color13 = #b294bb
        color14 = #8abeb7
        color15 = #c5c8c6
      '';
      font = "Hasklig 12";
    };
  };

  qt = {
    platformTheme = "gtk";
  };

  services = {
    compton = {
      enable = true;
      fade = true;
      fadeSteps = [ "0.05" "0.02" ];
      shadow = true;
    };

    emacs.enable = true;

    gpg-agent = {
      enable = false; # Using system one for now: https://github.com/NixOS/nixpkgs/issues/72597
      enableSshSupport = true;
    };

    redshift = {
      enable = true;
      brightness = {
        day = "0.8";
        night = "0.6";
      };
      latitude = "39.77";
      longitude = "-86.16";
      tray = true;
    };

    screen-locker = {
      enable = true;
      lockCmd = "${pkgs.xsecurelock}/bin/xsecurelock";
    };
  };

  systemd.user.services = {
    xsettingsd = {
      Unit = {
        Description = "X Settings Daemon";
        After = [ "graphical-session-pre.target" ];
        PartOf = [ "graphical-session.target" ];
      };

      Service = {
        ExecStart = "${pkgs.xsettingsd}/bin/xsettingsd xsettingsd";
        ExecStop = "/usr/bin/env pkill xsettingsd";
      };

      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
    };
  };

  xdg.configFile."nixpkgs/config.nix".source = ./config/nixpkgs/config.nix;

  xsession = {
    enable = true;

    initExtra = ''
      # I have ThinkPads. Remap PrtSc to menu.
      ${pkgs.xorg.xmodmap}/bin/xmodmap -e "keycode 107 = Menu"
    '';
  };
}
