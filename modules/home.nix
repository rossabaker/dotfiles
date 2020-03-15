{ pkgs
, dpi ? 96
, ...
}:

let
  sources = import ../nix/sources.nix;
  all-hies = import sources.all-hies {};
  niv = import sources.niv {};
in
rec {
  imports = [
    ./code.nix
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
      ".config/direnv/direnvrc".source = ../config/direnv/direnvrc;
      ".stack/config.yaml".source = ../config/stack/config.yaml;
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
      pkgs.gitAndTools.gh
      pkgs.gitter
      pkgs.gnome3.dconf # gtk doesn't configure without it
      pkgs.gnupg
      pkgs.hasklig
      pkgs.jq
      pkgs.material-design-icons
      pkgs.metals
      pkgs.sbt-extras
      pkgs.siji
      pkgs.slack
      pkgs.unifont
    ];

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
    };

    git = {
      enable = true;
      extraConfig = {
        url = {
          "git@github.com" = { insteadOf = "gh"; };
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
  };

  qt = {
    platformTheme = "gtk";
  };

  services = {

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
