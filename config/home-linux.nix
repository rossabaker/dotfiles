{ pkgs, ... }:

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
  };

  home = {
    file = {
      ".config/systemd/user/cros-garcon.service.d" = {
         source = systemd/cros-garcon.service.d;
         recursive = true;
      };
      ".xsettingsd".source = xsettingsd/.xsettingsd;
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

    termite = {
      enable = true;
      colorsExtra = ''
         color0 = #1d1f21
         color8 = #969896
         color1 = #912226
         color9 = #cc6666
         color2 = #778900
         color10 = #b5bd68
         color3 = #ae7b00
         color11 = #f0c674
         color4 = #1d2594
         color12 = #81a2be
         color5 = #682a9b
         color13 = #b294bb
         color6 = #2b6651
         color14 = #8abeb7
         color7 = #929593
         color15 = #ecebec
      '';
      backgroundColor = "#1d1f21";
      foregroundColor = "#c5c8c6";
      foregroundBoldColor = "#ffffff";
      highlightColor = "#d6d6d6";
      cursorBlink = "off";
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
      script = "polybar example &";
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
