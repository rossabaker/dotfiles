{ pkgs, ... }:

{
  home.file = {
    ".emacs.d/custom.el".source = ./custom.el;
  };

  programs.emacs = {
    enable = true;
    extraPackages = epkgs: with epkgs; [
      ross-init

      ace-window
      atomic-chrome
      avy
      base16-theme
      bazel-mode
      beacon
      better-defaults
      color-theme-sanityinc-tomorrow
      company-lsp
      company-quickhelp
      company-restclient
      counsel
      counsel-jq
      counsel-projectile
      crux
      delight
      dhall-mode
      direnv
      dockerfile-mode
      dtrt-indent
      dumb-jump
      electric-operator
      emacs-libvterm
      ess
      exec-path-from-shell
      expand-region
      flycheck
      git-gutter
      git-link
      git-timemachine
      gitconfig-mode
      gitignore-mode
      haskell-mode
      hasklig-mode
      hydra
      ivy
      ivy-rich
      json-mode
      lsp-haskell
      lsp-mode
      lsp-treemacs
      lsp-ui
      ivy
      list-environment
      multi-line
      nix-mode
      nix-sandbox
      magit
      page-break-lines
      persistent-scratch
      projectile
      protobuf-mode
      quick-yes
      rainbow-delimiters
      rainbow-mode
      restart-emacs
      restclient
      sbt-mode
      scala-mode
      shell-pop
      smartparens
      spacemacs-theme
      stan-mode
      string-inflection
      swiper
      systemd
      title-capitalization
      try
      unfill
      use-package
      which-key
      ws-butler
      yaml-mode

      pkgs.jq
    ];

    overrides = self: super:
      let
        inherit (pkgs) fetchFromGitHub fetchurl runCommand stdenv;
        inherit (stdenv) lib;

        withPatches = pkg: patches:
          lib.overrideDerivation pkg (attrs: { inherit patches; });
      in {
        ross-init = (runCommand "ross-init" {
          buildInputs = [ self.emacs ];
        } ''
          cp ${./README.org} README.org
          cp ${./init.el} init.el
          ${self.emacs}/bin/emacs --batch -l ob-tangle --eval "(org-babel-tangle-file \"README.org\" \"default.el\")"
          mkdir -p $out/share/emacs/site-lisp
          mv *.el $out/share/emacs/site-lisp
        '');

        git-gutter = withPatches super.git-gutter [ ./patches/git-gutter.patch ];
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
        quick-yes =
          let version = "10";
          in stdenv.mkDerivation {
            inherit version;
            name = "quick-yes-${version}";
            src = ./quick-yes;
            installPhase = ''
                mkdir -p $out/share/emacs/site-lisp
                cp *.el $out/share/emacs/site-lisp/
              '';
          };
        sbt-mode = withPatches super.sbt-mode [
          ./patches/sbt-mode/e9aa908d1b80dc2618eab22eeefc68ae82d0026f.patch
        ];
        title-capitalization =
          let version = "0.1";
          in stdenv.mkDerivation {
            inherit version;
            name = "title-capitalization-${version}";
            src = fetchFromGitHub {
              owner = "novoid";
              repo = "title-capitalization.el";
              rev = "e83d463c500d04adf47b2e444728803121e7b641";
              sha256 = "0y0fhi8sb3chh5pzgn0rp7cy7492bw5yh1dldmpqxcsykjn06aga";
            };
            installPhase = ''
                mkdir -p $out/share/emacs/site-lisp
                cp *.el $out/share/emacs/site-lisp/
              '';
          };
      };
  };
}
