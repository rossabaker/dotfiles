NIX_CONFIG = $(HOME)/src/nix-config
HOST = $(shell hostname)

switch: os-switch hm-switch

os-switch:
	sudo nixos-rebuild switch -I nixos-config=$(NIX_CONFIG)/hosts/$(HOST)/configuration.nix -I nixpkgs=nixpkgs

hm-switch:
	HOME_MANAGER_CONFIG=$(NIX_CONFIG)/hosts/$(HOST)/home.nix home-manager -I home-manager=home-manager -I nixpkgs=nixpkgs switch

cachix-install:
	nix-env -iA cachix -f https://cachix.org/api/v1/install
