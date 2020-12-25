NIX_CONFIG = $(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
HOST = $(shell hostname)
SHELL = /usr/bin/env bash

NIXPKGS ?= $(shell jq -r .nixpkgs.url < $(NIX_CONFIG)/nix/sources.json)
NIXOS_CONFIG ?= $(NIX_CONFIG)/hosts/$(HOST)/configuration.nix
HOME_MANAGER ?= $(shell jq -r .[\"home-manager\"].url < $(NIX_CONFIG)/nix/sources.json)
HOME_MANAGER_CONFIG ?= $(NIX_CONFIG)/hosts/$(HOST)/home.nix

NIX_PATHS = \
  -I nixpkgs=$(NIXPKGS) \
  -I nixos-config=$(NIXOS_CONFIG) \
  -I home-manager=$(HOME_MANAGER)

switch: os-switch hm-switch

update:
	niv update

os-switch:
	sudo nixos-rebuild switch $(NIX_PATHS)

hm-switch:
	HOME_MANAGER_CONFIG=$(HOME_MANAGER_CONFIG) home-manager switch $(NIX_PATHS)

format:
	fd '\.nix$$' -X nixpkgs-fmt
