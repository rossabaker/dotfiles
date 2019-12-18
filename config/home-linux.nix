{ dpi ? 96, pkgs, ... }:

let
  xsettingsd = pkgs.xsettingsd;
in {
  imports = [
    ./home.nix
    ../modules/emacs
    ../modules/i3
  ];

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
      ".config/polybar/dynamic.config".text = ''
         [bar/main]
         dpi = ${toString dpi}
         height = ${toString (dpi / 4)}
      '';
      ".config/systemd/user/cros-garcon.service.d" = {
         source = systemd/cros-garcon.service.d;
         recursive = true;
      };
      ".xsettingsd".text = ''
        Xft/DPI ${toString (dpi * 1024)}
      '';
    };

    keyboard.options = [
      "ctrl:nocaps"
    ];

    packages = [
      pkgs.gitter
      pkgs.gnome3.dconf # gtk doesn't configure without it
      pkgs.slack
      pkgs.spotify
      pkgs.xorg.xbacklight
      pkgs.xsecurelock
      pkgs.xsettingsd
      pkgs.xss-lock
      pkgs.xmonad-log
    ];
  };

  programs = {
    autorandr = {
      enable = true;
    };

    bash = {
      sessionVariables = {
        # Hack aronud https://github.com/rycee/home-manager/issues/423 for termite
        TERMINFO_DIRS="$HOME/.nix-profile/share/terminfo:/lib/terminfo";
        # https://github.com/NixOS/nixpkgs/issues/38991#issuecomment-496332104
        LOCALE_ARCHIVE_2_11=''$(nix-build --no-out-link "<nixpkgs>" -A glibcLocales)/lib/locale/locale-archive'';
        LOCALE_ARCHIVE_2_27=''$(nix-build --no-out-link "<nixpkgs>" -A glibcLocales)/lib/locale/locale-archive'';
        LOCALE_ARCHIVE="/usr/bin/locale";
      };
    };

    rofi = {
      enable = true;
      font = "sans-serif 12";
      extraConfig = ''rofi.dpi: ${toString dpi}'';
      theme = ../config/rofi/tomorrow-night.rasi;
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

  services = {
    compton = {
      enable = true;
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

  qt = {
    platformTheme = "gtk";
  };

  systemd.user.services = {
    xsettingsd = {
      Unit = {
        Description = "X Settings Daemon";
        After = [ "graphical-session-pre.target" ];
        PartOf = [ "graphical-session.target" ];
      };

      Service = {
        ExecStart = "${xsettingsd}/bin/xsettingsd xsettingsd";
        ExecStop = "/usr/bin/env pkill xsettingsd";
      };

      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
    };
  };

  xsession = {
    enable = true;

    initExtra = ''
      # I have ThinkPads. Remap PrtSc to menu.
      ${pkgs.xorg.xmodmap}/bin/xmodmap -e "keycode 107 = Menu"
    '';
  };
}
