{ dpi ? 96, pkgs, ... }:

let
  xsettingsd = pkgs.xsettingsd;
in {
  imports = [ ./home.nix ];

  gtk = {
    enable = true;
    iconTheme = {
      package = pkgs.papirus-icon-theme;
      name = "Papirus";
    };
    theme = {
      name = "Arc-Spacemacs";
      package = pkgs.arc-spacemacs-theme;
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
      pkgs.google-chrome
      pkgs.slack
      pkgs.spotify
      pkgs.xorg.xbacklight
      pkgs.xsettingsd
      pkgs.xmonad-log
    ];
  };

  programs = {
    autorandr = {
      enable = true;

      profiles = {
        "mobile" = {
          fingerprint = {
            eDP1 = "00ffffffffffff0006afeb3200000000251b0104a5221378020925a5564f9b270c50540000000101010101010101010101010101010152d000a0f0703e803020350058c1100000180000000f0000000000000000000000000020000000fe0041554f0a202020202020202020000000fe00423135365a414e30332e32200a000d";
          };
          config = {
            eDP1 = {
              enable = true;
              dpi = 176;
              mode = "3840x2160";
              position = "0x0";
              primary = true;
              rate = "60.00";
            };
          };
        };
      };
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
      theme = ../config/rofi/nord.rasi;
    };

    termite = {
      enable = true;
      cursorBlink = "off";
      cursorColor = "#e3dedd";
      foregroundColor = "#b2b2b2";
      backgroundColor = "#292b2e";
      highlightColor = "#444155";
      colorsExtra = ''
        color0  = #0a0814
        color1  = #f2241f
        color2  = #67b11d
        color3  = #b1951d
        color4  = #3a81c3
        color5  = #a31db1
        color6  = #21b8c7
        color7  = #b2b2b2
        color8  = #0a0814
        color9  = #f2241f
        color10 = #67b11d
        color11 = #b1951d
        color12 = #3a81c3
        color13 = #a31db1
        color14 = #21b8c7
        color15 = #b2b2b2
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

    polybar = {
      enable = true;
      config = ./polybar/config;
      script = "${pkgs.polybar}/bin/polybar main &";
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

    windowManager = {
      xmonad = {
        enable = true;
        extraPackages = haskellPackages: [
          haskellPackages.dbus
          haskellPackages.xmonad-contrib
        ];
        config = xmonad/xmonad.hs;
      };
    };
  };
}
