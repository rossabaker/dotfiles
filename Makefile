NIX_CONFIG = $(HOME)/src/nix-config

switch:
	HOME_MANAGER_CONFIG=$(NIX_CONFIG)/config/home.nix home-manager switch
