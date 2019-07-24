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
        epkgs.company-lsp
        epkgs.counsel
        epkgs.counsel-projectile
        epkgs.delight
        epkgs.flycheck
        epkgs.haskell-mode
        epkgs.ivy
        epkgs.lsp-mode
        epkgs.lsp-ui
        epkgs.nix-mode
        epkgs.magit
        epkgs.projectile
        epkgs.restart-emacs
        epkgs.sbt-mode
        epkgs.scala-mode
        epkgs.swiper
        epkgs.use-package
        epkgs.ws-butler
      ];
      overrides = self: super:
        let
          inherit (pkgs) fetchFromGitHub fetchurl stdenv;
          inherit (stdenv) lib;
        in {
          lsp-mode = self.melpaBuild {
            pname = "lsp-mode";
            version = "20190723.2001";
            src = fetchFromGitHub {
              owner = "emacs-lsp";
              repo = "lsp-mode";
              rev = "614e33450c8a6faf3d72502eb44cee4412663f4a";
              sha256 = "05qm1dk26426gpbcjcqzzs05fxi7js0g0fifvaxj0gm4pndizbi2";
            };
            recipe = fetchurl {
              url = "https://raw.githubusercontent.com/milkypostman/melpa/51a19a251c879a566d4ae451d94fcb35e38a478b/recipes/lsp-mode";
              sha256 = "0cklwllqxzsvs4wvvvsc1pqpmp9w99m8wimpby6v6wlijfg6y1m9";
              name = "lsp-mode";
            };
            packageRequires = with self; [ dash dash-functional emacs f ht markdown-mode spinner ];
            meta = {
              homepage = "https://melpa.org/#/lsp-mode";
              license = lib.licenses.free;
            };
          };
        }
        ;
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
