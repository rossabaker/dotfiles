{ config, pkgs, ... }:

{
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages =
    [ pkgs.vim
    ];

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  environment.darwinConfig = "$HOME/src/nix-config/hosts/$HOSTNAME/configuration.nix";

  environment.shells = [ pkgs.bashInteractive ];

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;

  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.bash.enable = true;
  # programs.zsh.enable = true;
  # programs.fish.enable = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  system.keyboard = {
    enableKeyMapping = true;
    remapCapsLockToControl = true;
  };

  nix.nixPath = [
    { darwin = ./nix-darwin; }
    { nixpkgs = ./nixpkgs; }
    { home-manager = ./home-manager; }
  ];

  nix.trustedUsers = [
    "@admin"
    "rossbaker" # cachix doesn't support groups yet
  ];
}
