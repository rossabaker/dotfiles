{ pkgs, ... }:

{
  imports = [
    (
      import ../../home.nix {
        pkgs = pkgs;
      }
    )
  ];

  home.file = {
    ".config/systemd/user/cros-garcon.service.d" = {
      source = ../../config/systemd/cros-garcon.service.d;
      recursive = true;
    };
  };
}
