{ pkgs ? import ../nixpkgs.nix }:

let

  server = pkgs.haskell.packages.ghc.callCabal2nix "server" ./. {
    common = pkgs.haskell.packages.ghc.callCabal2nix "common" ../common {};
  };

in

  if pkgs.lib.inNixShell then server.env else server


