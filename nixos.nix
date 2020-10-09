{ pkgs, ... }:

let
  sources = import ./nix/sources.nix;
in
{
  nix = {
    # For nix-direnv
    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
    '';

    nixPath = [
      "nixpkgs=${sources.nixpkgs.url}"
      "home-manager=${sources.home-manager.url}"
    ];
    trustedUsers = [ "root" "ross" ];
  };

  nixpkgs.config.allowUnfree = true;

  networking.networkmanager.enable = true; # Enables wireless support via wpa_supplicant.

  i18n = {
    # consoleKeyMap = ./config/kbd/keys.map;
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "America/Indiana/Indianapolis";

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  services.dbus.packages = [ pkgs.gnome3.dconf ];

  services.fwupd.enable = true;

  services.locate.enable = true;

  services.logind.lidSwitchExternalPower = "ignore";

  services.tlp.enable = true;

  services.upower.enable = true;

  services.xserver.enable = true;
  services.xserver.layout = "us";
  services.xserver.libinput.enable = true;

  services.xserver.displayManager = {
    defaultSession = "xterm";
    sddm.enable = true;
  };
  services.xserver.desktopManager = {
    xterm.enable = true;
  };
  services.xserver.videoDrivers = [ "nvidia" ];

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.ross = {
    isNormalUser = true;
    extraGroups = [
      "audio"
      "docker"
      "wheel"
    ];
  };

  virtualisation.docker.enable = true;
}
