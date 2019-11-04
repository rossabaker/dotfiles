{ config, pkgs, ... }:

{
  imports = [ ../../darwin.nix ];

  # You should generally set this to the total number of logical cores in your system.
  # $ sysctl -n hw.ncpu
  nix.maxJobs = 16;
  nix.buildCores = 16;
}
