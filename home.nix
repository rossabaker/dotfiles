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
        epkgs.nix-mode
        epkgs.magit
        epkgs.use-package
      ];
    };

    git = {
      enable = true;
      userEmail = "ross@rossabaker.com";
      userName = "Ross A. Baker";
    };

    # Let Home Manager install and manage itself.
    home-manager.enable = true;

    zsh = {
      enable = true;
    };
  };
}
