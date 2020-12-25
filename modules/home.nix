{ pkgs
, dpi ? 96
, ...
}:

let
  sources = import ../nix/sources.nix;
  pkgs-ashkitten = import sources.nixpkgs-ashkitten {};
  nix-haskell-tags = import sources.nix-haskell-tags;
  er-nix = import sources.er-nix;
in
rec {
  imports = [
    ./emacs
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
      ".stack/config.yaml".source = ../config/stack/config.yaml;
      ".sbt" = {
        source = ../config/sbt;
        recursive = true;
      };
    };

    keyboard.options = [
      "ctrl:nocaps"
    ];

    packages = [
      pkgs.aspell
      pkgs.aspellDicts.en
      pkgs.bashInteractive
      pkgs.cachix
      pkgs.dhall
      pkgs.gitAndTools.gh
      pkgs.gitter
      pkgs-ashkitten.glimpse
      pkgs.gnome3.dconf # gtk doesn't configure without it
      pkgs.gnupg
      pkgs.hasklig
      nix-haskell-tags.nix-haskell-tags-exe
      pkgs.jq
      pkgs.material-design-icons
      pkgs.metals
      pkgs.niv
      pkgs.nix-prefetch-git
      (pkgs.sbt.override {
        jre = pkgs.openjdk8;
      })
      pkgs.siji
      pkgs.slack
      pkgs.unifont
    ] ++ builtins.attrValues er-nix.tools.haskell-language-servers;

    sessionVariables = {
      ALTERNATE_EDITOR = "";
      EDITOR = "emacsclient -t";
      VISUAL = "emacsclient -c";
    };
  };

  nixpkgs.config = import ../config/nixpkgs/config.nix;
  nixpkgs.overlays = (import ../config/nixpkgs/config.nix).overlays;

  programs = {
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
        TERMINFO_DIRS = "$HOME/.nix-profile/share/terminfo:/lib/terminfo";
        # https://github.com/NixOS/nixpkgs/issues/38991#issuecomment-496332104
        LOCALE_ARCHIVE_2_11 = ''$(nix-build --no-out-link "<nixpkgs>" -A glibcLocales)/lib/locale/locale-archive'';
        LOCALE_ARCHIVE_2_27 = ''$(nix-build --no-out-link "<nixpkgs>" -A glibcLocales)/lib/locale/locale-archive'';
        LOCALE_ARCHIVE = "/usr/bin/locale";
      };
    };

    direnv = {
      enable = true;
      enableBashIntegration = true;
      enableNixDirenvIntegration = true;
    };

    git = {
      enable = true;
      extraConfig = {
        github = {
          "user" = "rossabaker";
        };
        gitlab = {
          "user" = "rossabaker";
        };
        url = {
          "git@github.com" = { insteadOf = "gh"; };
        };
      };
      ignores = [
        ".bloop"
        ".bsp/"
        ".direnv/"
        ".metals"
        "metals.sbt"
      ];
      userEmail = "ross@rossabaker.com";
      userName = "Ross A. Baker";
    };

    # Let Home Manager install and manage itself.
    home-manager.enable = true;
  };

  qt = {
    platformTheme = "gtk";
  };

  services = {
    dunst = {
      enable = true;
      settings = {
        global = {
          dmenu = "${pkgs.rofi}/bin/rofi -dmenu";
          follow = "mouse";
          format = "<b>%s</b>\\n%b";
          frame_color = "#555555";
          frame_width = 2;
          geometry = "500x5-5+30";
          horizontal_padding = 8;
          icon_position = "off";
          line_height = 0;
          markup = "full";
          padding = 8;
          separator_color = "frame";
          separator_height = 2;
          transparency = 10;
          word_wrap = true;
        };

        shortcuts = {
          context = "mod4+grave";
          close = "mod4+backslash";
        };

        urgency_low = {
          background = "#1d1f21";
          foreground = "#4da1af";
          frame_color = "#4da1af";
          timeout = 10;
        };

        urgency_normal = {
          background = "#1d1f21";
          foreground = "#70a040";
          frame_color = "#70a040";
          timeout = 15;
        };

        urgency_critical = {
          background = "#1d1f21";
          foreground = "#dd5633";
          frame_color = "#dd5633";
          timeout = 0;
        };
      };
    };

    emacs.enable = true;

    gpg-agent = {
      enable = true;
      enableSshSupport = true;

      defaultCacheTtl = 8 * 60 * 60;
      maxCacheTtl = 24 * 60 * 60;
    };

    kbfs.enable = true;
    keybase.enable = true;
  };

  xdg.configFile."nixpkgs/config.nix".source = ../config/nixpkgs/config.nix;
}
