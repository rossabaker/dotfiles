{ pkgs, ... }:

let
  sources = import ../../nix/sources.nix;

  used-packages = import (
    pkgs.runCommand "used-packages" rec {
      emacs = pkgs.emacsWithPackages (epkgs: [ epkgs.use-package ]);
      buildInputs = [ emacs ];
      srcs = [ ./used-packages.el ./init.el ];
    } ''
      mkdir -p $out
      emacs --batch -l ${./used-packages.el} --eval '(ross/used-packages "${./init.el}")' > $out/default.nix
    ''
  );

  # Things we want to ensure are on the exec-path.
  extraSystemPackages = with pkgs; [
    fd
    git
    ripgrep
  ];
in
{
  home.file = {
    ".emacs.d/init.el".source = ./init.el;
  };

  home.packages = [
    pkgs.fd
    pkgs.emacs-all-the-icons-fonts
    pkgs.nodePackages.pyright
    pkgs.pandoc
    (pkgs.python3.withPackages(ps: [ps.tkinter]))
    pkgs.ripgrep
    pkgs.ruby # TODO for yamllint-mode. Feels heavy.
  ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacsUnstable;
    extraPackages = epkgs: (used-packages epkgs) ++ extraSystemPackages;

    overrides = self: super:
      let
        inherit (pkgs) fetchFromGitHub fetchurl stdenv;
        inherit (stdenv) lib;

        withPatches = pkg: patches:
          lib.overrideDerivation pkg (attrs: { inherit patches; });
      in
        {
          goto-line-faster = stdenv.mkDerivation {
            name = "goto-line-faster";
            src = sources."goto-line-faster.el";
            installPhase = ''
              mkdir -p $out/share/emacs/site-lisp
              cp *.el $out/share/emacs/site-lisp/
            '';
          };
          quick-yes =
            let
              version = "10";
            in
              stdenv.mkDerivation {
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
          seq = null; # https://github.com/NixOS/nixpkgs/issues/73346
          title-capitalization = stdenv.mkDerivation {
            name = "title-capitalization";
            src = sources."title-capitalization.el";
            installPhase = ''
              mkdir -p $out/share/emacs/site-lisp
              cp *.el $out/share/emacs/site-lisp/
            '';
          };
          yaml-imenu = withPatches super.yaml-imenu [
            ./patches/yaml-imenu/0001-Replace-loop-with-cl-loop.patch
          ];
        };
  };
}
