{ config, pkgs, ... }:

{
  home = {
    file = {
      ".emacs.d/init.el".source = emacs/init.el;
    };
  };

  programs = {
    emacs = {
      enable = true;
      extraPackages = epkgs: [
        epkgs.base16-theme
        epkgs.better-defaults
        epkgs.counsel
        epkgs.delight
        epkgs.ivy        
        epkgs.nix-mode
        epkgs.magit
        epkgs.swiper
        epkgs.use-package
        epkgs.ws-butler
      ];
    };

    git = {
      enable = true;
      extraConfig = ''
        [url "git@github.com:"]
          insteadOf = "gh:"
      '';
      userEmail = "ross@rossabaker.com";
      userName = "Ross A. Baker";
    };

    # Let Home Manager install and manage itself.
    home-manager.enable = true;

    zsh = {
      enable = true;
      sessionVariables = {
        NIX_PATH = "$HOME/.nix-defexpr/channels\${NIX_PATH:+:}$NIX_PATH";
      };
    };
  };

  services = {
    emacs.enable = true;

    gpg-agent = {
      enable = true;
      enableSshSupport = true;
    };
  };
}
