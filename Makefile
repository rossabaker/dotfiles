NIX_CONFIG = $(HOME)/src/nix-config
HOST = $(shell hostname)

switch: os-switch hm-switch

os-switch:
	sudo nixos-rebuild switch -I nixos-config=$(NIX_CONFIG)/hosts/$(HOST)/configuration.nix

hm-switch:
	HOME_MANAGER_CONFIG=$(NIX_CONFIG)/config/home.nix home-manager switch
