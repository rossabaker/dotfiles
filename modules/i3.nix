{ pkgs, lib, ... }:

{
  xsession.windowManager.i3 =
    let
      mod = "Mod4";
    in {
      enable = true;
      package = pkgs.i3-gaps;
      config = {
        bars = [];
        modifier = mod;
        keybindings = lib.mkOptionDefault {
          "${mod}+Return" = "exec ${pkgs.termite}/bin/termite";
          "${mod}+d" = "exec ${pkgs.rofi}/bin/rofi -show run";
          "${mod}+a" = "focus parent";
          "${mod}+j" = "focus left";
          "${mod}+k" = "focus down";
          "${mod}+l" = "focus up";
          "${mod}+semicolon" = "focus right";
          "${mod}+Shift+j" = "move window left";
          "${mod}+Shift+k" = "move window down";
          "${mod}+Shift+l" = "move window up";
          "${mod}+Shift+semicolon" = "move window right";
          XF86AudioMute = "exec ${pkgs.pulseaudio}/bin/pactl set-sink-mute @DEFAULT_SINK@ toggle";
          XF86AudioLowerVolume = "exec ${pkgs.pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ -5%";
          XF86AudioRaiseVolume = "exec ${pkgs.pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ +5%";
          XF86AudioMicMute = "exec ${pkgs.pulseaudio}/bin/pactl set-source-mute @DEFAULT_SOURCE@ toggle";
          XF86MonBrightnessDown = "exec ${pkgs.xorg.xbacklight}/bin/xbacklight -dec 5";
          XF86MonBrightnessUp = "exec ${pkgs.xorg.xbacklight}/bin/xbacklight -inc 5";
        };
        startup = [
          { command = "systemctl --user restart polybar";
            always = true;
            notification = false;
          }
        ];
        gaps = {
          inner = 4;
          outer = 0;
        };
      };
    };

  services.polybar = {
    enable = true;
    package = pkgs.polybar.override {
      i3Support = true;
    };
    config = ../config/polybar/config;
    script = ''polybar main &'';
  };
}
