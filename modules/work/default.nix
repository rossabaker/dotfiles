{ pkgs, ... }:

let
  sources = import ../../nix/sources.nix;
  er-nix = import sources.er-nix;
in
{
  home.packages = [
    pkgs.zoom-us
    er-nix.pkgs.okta-aws-login
  ] ++ builtins.attrValues er-nix.tools.hopenpgp-tools;
}
