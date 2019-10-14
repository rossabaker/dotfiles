{ config, pkgs, ... }:

let
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
in
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
      ".xsettingsd".source = xsettingsd/.xsettingsd;
    };

    keyboard.options = [
      "ctrl:nocaps"
    ];

    packages = [
      (all-hies.selection { selector = p: { inherit (p) ghc865; }; })
      pkgs.bashInteractive
      pkgs.hasklig
      pkgs.sbt-extras
      pkgs.stack
      pkgs.xsettingsd
    ];
  };

  programs = {
    autorandr = {
      enable = true;

      profiles = {
        "mobile" = {
          fingerprint = {
            eDP-1-1 = "00ffffffffffff0006afeb3200000000251b0104a5221378020925a5564f9b270c50540000000101010101010101010101010101010152d000a0f0703e803020350058c1100000180000000f0000000000000000000000000020000000fe0041554f0a202020202020202020000000fe00423135365a414e30332e32200a000d";
          };
          config = {
            eDP-1-1 = {
              enable = true;
              dpi = 176;
              mode = "3840x2160";
              position = "0x0";
              primary = true;
              rate = "60.00";
            };
          };
        };
      };
    };

    bash = {
      enable = true;
      initExtra = ''
        PS1='\n\[\033[1;36m\][\h:\w]\$\[\033[0m\] '
      '';
      sessionVariables = {
        NIX_PATH = "$HOME/.nix-defexpr/channels\${NIX_PATH:+:}$NIX_PATH";
      };
    };

    direnv = {
      enable = true;
      enableBashIntegration = true;
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
        epkgs.color-theme-sanityinc-tomorrow
        epkgs.company-lsp
        epkgs.company-quickhelp
        epkgs.company-restclient
        epkgs.counsel
        epkgs.counsel-projectile
        epkgs.crux
        epkgs.delight
        epkgs.dhall-mode
        epkgs.direnv
        epkgs.dockerfile-mode
        epkgs.dtrt-indent
        epkgs.electric-operator
        epkgs.ess
        epkgs.expand-region
        epkgs.flycheck
        epkgs.git-gutter
        epkgs.git-link
        epkgs.git-timemachine
        epkgs.gitconfig-mode
        epkgs.gitignore-mode
        epkgs.haskell-mode
        epkgs.hasklig-mode
        epkgs.hydra
        epkgs.ivy
        epkgs.ivy-rich
        epkgs.json-mode
        epkgs.lsp-haskell
        epkgs.lsp-mode
        epkgs.lsp-treemacs
        epkgs.lsp-ui
        epkgs.ivy
        epkgs.list-environment
        epkgs.multi-line
        epkgs.nix-mode
        epkgs.magit
        epkgs.projectile
        epkgs.protobuf-mode
        epkgs.rainbow-delimiters
        epkgs.rainbow-mode
        epkgs.restart-emacs
        epkgs.restclient
        epkgs.sbt-mode
        epkgs.scala-mode
        epkgs.shell-pop
        epkgs.smartparens
        epkgs.stan-mode
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
          sbt-mode = withPatches super.sbt-mode [
            ./emacs/patches/sbt-mode/889efbafc6f831713422884864a1b0adb6e30f38.patch
            ./emacs/patches/sbt-mode/e9aa908d1b80dc2618eab22eeefc68ae82d0026f.patch
          ];
        };
    };

    git = {
      enable = true;
      extraConfig = ''
        [url "git@github.com:"]
          insteadOf = "gh:"
      '';
      ignores = [
        ".bloop"
        ".metals"
      ];
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

  systemd.user.services = {
    xsettingsd = {
      Unit = {
        Description = "X Settings Daemon";
        After = [ "graphical-session-pre.target" ];
        PartOf = [ "graphical-session.target" ];
      };

      Service = {
        ExecStart = "/usr/bin/env xsettingsd";
        ExecStop = "/usr/bin/env pkill xsettingsd";
      };

      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
    };
  };

  xsession = {
    enable = true;

    windowManager = {
      xmonad = {
        enable = true;
        config = xmonad/xmonad.hs;
      };
    };
  };
}
