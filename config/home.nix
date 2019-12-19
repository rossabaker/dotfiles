{ pkgs, ... }:

let
  sources = import ../nix/sources.nix;
  all-hies = import sources.all-hies {};
  niv = import sources.niv {};
in rec {
  imports = [
    ../modules/chrome
  ];

  fonts.fontconfig.enable = true;

  home = {
    file = {
      ".config/direnv/direnvrc".source = direnv/direnvrc;
    };

    packages = [
      (all-hies.selection { selector = p: { inherit (p) ghc865; }; })
      niv.niv
      pkgs.aspell
      pkgs.aspellDicts.en
      pkgs.bashInteractive
      pkgs.dhall
      pkgs.direnv
      pkgs.gnupg
      pkgs.hasklig
      pkgs.jq
      pkgs.material-design-icons
      pkgs.sbt-extras
      pkgs.siji
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
  };

  xdg.configFile."nixpkgs/config.nix".source = ../config/nixpkgs/config.nix;
}
