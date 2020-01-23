# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  clamscan-job = pkgs.writeScriptBin "clamscan-job" ''
    #!/usr/bin/env bash
    SCAN_DIR="/"
    LOG_FILE="/var/log/clamav/daily_clamscan.log"
    mkdir -p $(dirname $LOG_FILE)
    echo ">>> BEGIN RUN AT $(date) ---" >> $LOG_FILE
    ${pkgs.clamav}/bin/clamscan -i -r $SCAN_DIR >> $LOG_FILE 2>&1
  '';
in
{
  imports =
    [
      # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ../../nixos.nix
    ];

  boot.kernelPackages = pkgs.linuxPackages_5_4;
  boot.kernelParams = [ "nomodeset" ];

  boot.initrd.luks.devices = {
    root = {
      device = "/dev/disk/by-uuid/cee63b4f-60a1-4290-a51a-de9fdfdf3004";
      preLVM = true;
    };
  };

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "herbert"; # Define your hostname.

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp0s31f6.useDHCP = true;
  networking.interfaces.wlp82s0.useDHCP = true;

  services.clamav = {
    daemon.enable = true;
    updater.enable = true;
  };

  services.cron = {
    enable = true;
    systemCronJobs = [
      "30 1 * * * root ${clamscan-job}/bin/clamscan-job"
    ];
  };

  services.pcscd.enable = true;
  services.udev.packages = [ pkgs.yubikey-personalization ];

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "20.03"; # Did you read the comment?

  users.users."ross".extraGroups = [ "docker" ];

  virtualisation.docker.enable = true;
}
