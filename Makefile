NIX_CONFIG = $(HOME)/src/nix-config
UNAME_S = $(shell uname -s)
HOST = $(shell hostname)

switch: os-switch hm-switch

os-switch:
ifeq ($(UNAME_S),Linux)
	sudo nixos-rebuild switch -I nixos-config=$(NIX_CONFIG)/hosts/$(HOST)/configuration.nix
else ifeq ($(UNAME_S),Darwin)
	darwin-rebuild switch
endif

hm-switch:
	HOME_MANAGER_CONFIG=$(NIX_CONFIG)/config/home.nix home-manager switch
