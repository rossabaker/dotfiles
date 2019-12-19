{ pkgs, ... }:

{
  imports = [
    (import ../../home.nix {
      pkgs = pkgs;
    })
  ];

  home.files = {
    ".config/systemd/user/cros-garcon.service.d" = {
      source = ./config/systemd/cros-garcon.service.d;
      recursive = true;
    };
  };
}
