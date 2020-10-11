{ pkgs, lib, ... }:

{
  xsession.windowManager.i3 =
    let
      mod = "Mod4";

      ws = {
        www = "1";
        code = "2";
        chat = "3";
        comms = "4";
        media = "9";
      };

      outputs = {
        primary = "DP-1.2";
        left = "DP-0";
        right = "DP-1.1";
      };

      i3 = pkgs.i3-gaps;

      focus-or-run-emacsclient = pkgs.writeScriptBin "focus-or-run-emacsclient" ''
        #!${pkgs.stdenv.shell}
        ${i3}/bin/i3-msg '[class="Emacs"] focus' | ${pkgs.jq}/bin/jq -e .[0].success || emacsclient -c
      '';
    in
      {
        enable = true;
        package = i3;
        config = {
          bars = [];
          fonts = [ "sans-serif 7" ];
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
            "${mod}+comma" = "focus output ${outputs.left}";
            "${mod}+period" = "focus output ${outputs.primary}";
            "${mod}+slash" = "focus output ${outputs.right}";
            "${mod}+Shift+comma" = "move window to output ${outputs.left}; focus output ${outputs.left}";
            "${mod}+Shift+period" = "move window to output ${outputs.primary}; focus output ${outputs.primary}";
            "${mod}+Shift+slash" = "move window to output ${outputs.right}; focus output ${outputs.right}";
            "${mod}+Ctrl+comma" = "move workspace to output ${outputs.left}";
            "${mod}+Ctrl+period" = "move workspace to output ${outputs.primary}";
            "${mod}+Ctrl+slash" = "move workspace to output ${outputs.right}";
            "${mod}+apostrophe" = "exec ${focus-or-run-emacsclient}/bin/focus-or-run-emacsclient";
            XF86AudioMute = "exec ${pkgs.pulseaudio}/bin/pactl set-sink-mute @DEFAULT_SINK@ toggle";
            XF86AudioLowerVolume = "exec ${pkgs.pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ -5%";
            XF86AudioRaiseVolume = "exec ${pkgs.pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ +5%";
            XF86AudioMicMute = "exec ${pkgs.pulseaudio}/bin/pactl set-source-mute @DEFAULT_SOURCE@ toggle";
            XF86MonBrightnessDown = "exec ${pkgs.xorg.xbacklight}/bin/xbacklight -dec 5";
            XF86MonBrightnessUp = "exec ${pkgs.xorg.xbacklight}/bin/xbacklight -inc 5";
          };
          startup = [
            {
              command = "systemctl --user restart polybar";
              always = true;
              notification = false;
            }
          ];
          assigns = {
            "${ws.code}" = [
              { class = "^Emacs$"; }
            ];
            "${ws.chat}" = [
              { class = "^Gitter$"; }
              { class = "^Slack$"; }
            ];
            "${ws.comms}" = [
              { instance = "^mail.google.com__.*"; }
              { instance = "^calendar.google.com__.*"; }
            ];
            "${ws.media}" = [
              { class = "^Spotify$"; }
            ];
          };
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
    config = ./polybar/config;
    script = ''polybar main &'';
  };
}
