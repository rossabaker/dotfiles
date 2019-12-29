{ pkgs, ... }:

{
  imports = [
    (import ../../modules/home.nix { inherit pkgs; })
  ];

  home.file = {
    ".config/systemd/user/cros-garcon.service.d" = {
      source = ../../config/systemd/cros-garcon.service.d;
      recursive = true;
    };
  };
}
