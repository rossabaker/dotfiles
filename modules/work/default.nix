{ pkgs, ... }:

let
  okta-aws-login = pkgs.callPackage ../../pkgs/okta-aws-login {};
in
{
  home.packages = [
    pkgs.zoom-us
    okta-aws-login
  ];
}
