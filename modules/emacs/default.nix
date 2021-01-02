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
in
{
  home.file = {
    ".emacs.d/init.el".source = ./init.el;
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacs27;
    extraPackages = epkgs: (used-packages epkgs);
  };
}
