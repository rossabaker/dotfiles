{ pkgs, ... }:

let
  sources = import ../../nix/sources.nix;
  doom-emacs = pkgs.callPackage sources.nix-doom-emacs {
    doomPrivateDir = ./doom.d;
  };
in {
  home.packages = [
    pkgs.aspell
    pkgs.aspellDicts.en
    pkgs.aspellDicts.en-computers
    pkgs.direnv
    pkgs.docker
    pkgs.docker-compose
    pkgs.docker-machine
    pkgs.emacs-all-the-icons-fonts
    pkgs.git
    pkgs.fd
    pkgs.languagetool
    pkgs.mdl
    pkgs.pandoc
    pkgs.ripgrep
    pkgs.wordnet
  ];

  programs.emacs = {
    enable = true;
    package = doom-emacs;
  };
}
