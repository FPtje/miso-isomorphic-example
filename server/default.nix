{ pkgs ? import ../nixpkgs.nix }:

let

  _server = pkgs.haskell.packages.ghc.callCabal2nix "server" ./. {
    common = pkgs.haskell.packages.ghc.callCabal2nix "common" ../common {};
  };

in

  if pkgs.lib.inNixShell then _server.env else _server


