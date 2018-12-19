{ pkgs ? import ../nixpkgs.nix }:

let

  _common = pkgs.haskellPackages.callCabal2nix "common" ./. {};

in

if pkgs.lib.inNixShell then _common.env else _common

