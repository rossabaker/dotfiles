{ config, pkgs, ... }:

{
  fonts.fontconfig.enable = true;

  home = {
    file = {
      ".config/systemd/user/cros-garcon.service.d" = {
         source = systemd/cros-garcon.service.d;
         recursive = true;
      };
      ".emacs.d/custom.el".source = emacs/custom.el;
      ".emacs.d/init.el".source = emacs/init.el;
    };

    packages = [
      pkgs.bashInteractive
      pkgs.hasklig
    ];
  };

  programs = {
    bash = {
      enable = true;
      sessionVariables = {
        NIX_PATH = "$HOME/.nix-defexpr/channels\${NIX_PATH:+:}$NIX_PATH";
      };
    };

    emacs = {
      enable = true;
      extraPackages = epkgs: [
        epkgs.ace-window
        epkgs.atomic-chrome
        epkgs.avy
        epkgs.base16-theme
        epkgs.bazel-mode
        epkgs.beacon
        epkgs.better-defaults
        epkgs.company-lsp
        epkgs.company-quickhelp
        epkgs.counsel
        epkgs.counsel-projectile
        epkgs.crux
        epkgs.delight
        epkgs.dockerfile-mode
        epkgs.electric-operator
        epkgs.expand-region
        epkgs.flycheck
        epkgs.git-gutter
        epkgs.haskell-mode
        epkgs.hasklig-mode
        epkgs.ivy
        epkgs.ivy-rich
        epkgs.lsp-mode
        epkgs.lsp-ui
        epkgs.ivy
        epkgs.list-environment
        epkgs.multi-line
        epkgs.nix-mode
        epkgs.magit
        epkgs.projectile
        epkgs.protobuf-mode
        epkgs.restart-emacs
        epkgs.sbt-mode
        epkgs.scala-mode
        epkgs.shell-pop
        epkgs.smartparens
        epkgs.string-inflection
        epkgs.swiper
        epkgs.systemd
        epkgs.unfill
        epkgs.use-package
        epkgs.which-key
        epkgs.ws-butler
        epkgs.yaml-mode
      ];
      overrides = self: super:
        let
          inherit (pkgs) fetchFromGitHub fetchurl stdenv;
          inherit (stdenv) lib;

          withPatches = pkg: patches:
            lib.overrideDerivation pkg (attrs: { inherit patches; });
        in {
          git-gutter = withPatches super.git-gutter [ ./emacs/patches/git-gutter.patch ];
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
  };

  services = {
    emacs.enable = true;

    gpg-agent = {
      enable = true;
      enableSshSupport = true;
    };
  };
}
