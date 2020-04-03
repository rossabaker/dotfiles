{ pkgs
, dpi ? 96
, ...
}:

{
  imports = [
    ./chrome
    ./i3
  ];

  home = {
    file = {
      ".config/polybar/dynamic.config".text = ''
        [bar/main]
        dpi = ${toString dpi}
        height = ${toString (dpi / 4)}
      '';
      ".xsettingsd".text = ''
        Xft/DPI ${toString (dpi * 1024)}
      '';
    };

    packages = [
      pkgs.spotify
      pkgs.xorg.xbacklight
      pkgs.xsecurelock
      pkgs.xsettingsd
      pkgs.xss-lock
    ];
  };

  programs = {
    autorandr = {
      enable = true;
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
    picom = {
      enable = true;
      fade = true;
      fadeSteps = [ "0.05" "0.02" ];
      shadow = true;
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

  xsession = {
    enable = true;

    initExtra = ''
      ${pkgs.feh}/bin/feh --bg-fill -s ${../wallpapers/mandelbrot.png}

      # I have ThinkPads. Remap PrtSc to menu.
      ${pkgs.xorg.xmodmap}/bin/xmodmap -e "keycode 107 = Super_R"
    '';
  };
}
