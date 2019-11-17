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
      name = "Nordic";
      package = pkgs.nordic;
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
      cursorColor = "#d8de99";
      cursorForegroundColor = "#2e3440";
      foregroundColor = "#d8dee9";
      foregroundBoldColor = "#d8dee9";
      backgroundColor = "#2e3440";
      highlightColor = "#4c566a";
      colorsExtra = ''
        color0  = #3b4252
        color1  = #bf616a
        color2  = #a3be8c
        color3  = #ebcb8b
        color4  = #81a1c1
        color5  = #b48ead
        color6  = #88c0d0
        color7  = #e5e9f0
        color8  = #4c566a
        color9  = #bf616a
        color10 = #a3be8c
        color11 = #ebcb8b
        color12 = #81a1c1
        color13 = #b48ead
        color14 = #8fbcbb
        color15 = #eceff4
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
