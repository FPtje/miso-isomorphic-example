{ pkgs ? import ../nixpkgs.nix }:

let

  common = pkgs.haskellPackages.callCabal2nix "common" ./. {};

in

if pkgs.lib.inNixShell then common.env else common

