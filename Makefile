NIX_CONFIG = $(HOME)/src/nix-config
UNAME_S = $(shell uname -s)
HOST = $(shell hostname)

switch: os-switch hm-switch

os-switch:
ifeq ($(UNAME_S),Linux)
	sudo nixos-rebuild switch -I nixos-config=$(NIX_CONFIG)/hosts/$(HOST)/configuration.nix -I nixpkgs=nixpkgs
else ifeq ($(UNAME_S),Darwin)
	darwin-rebuild switch -I darwin-config=darwin.nix -I darwin=nix-darwin -I nixpkgs=nixpkgs
endif

hm-switch:
	HOME_MANAGER_CONFIG=$(NIX_CONFIG)/config/home.nix home-manager -I home-manager=home-manager -I nixpkgs=nixpkgs switch
