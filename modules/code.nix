{ pkgs, ... }:

{
  imports = [
    "${builtins.fetchGit {
      url = "https://github.com/msteen/nixos-vsliveshare.git";
      ref = "06259014e30fe1d2b328e0550a9cbe422981d9a2";
    }}/modules/vsliveshare/home.nix"
  ];

  home.packages = [
    pkgs.gnome3.gnome-keyring
    pkgs.jdk11
    pkgs.vscode
  ];

  services.gnome-keyring.enable = true;

  services.vsliveshare = {
    enable = true;
    extensionsDir = "$HOME/.vscode/extensions";
    nixpkgsPath = builtins.fetchGit {
      url = "https://github.com/NixOS/nixpkgs.git";
      ref = "refs/heads/nixos-20.03";
      rev = "61cc1f0dc07c2f786e0acfd07444548486f4153b";
    };
  };
}
