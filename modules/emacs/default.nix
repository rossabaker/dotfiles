{ pkgs, ... }:

let
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
    ".emacs.d/custom.el".source = ./custom.el;
    ".emacs.d/init.el".source = ./init.el;
  };

  home.packages = [ pkgs.emacs-all-the-icons-fonts ];

  programs.emacs = {
    enable = true;
    extraPackages = epkgs: (used-packages epkgs) ++ extraSystemPackages;

    overrides = self: super:
      let
        inherit (pkgs) fetchFromGitHub fetchurl stdenv;
        inherit (stdenv) lib;

        withPatches = pkg: patches:
          lib.overrideDerivation pkg (attrs: { inherit patches; });
      in
        {
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
          title-capitalization =
            let
              version = "0.1";
            in
              stdenv.mkDerivation {
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
