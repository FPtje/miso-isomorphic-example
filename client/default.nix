{ pkgs ? import ../nixpkgs.nix }:

let

  _client = pkgs.haskell.packages.ghcjs.callCabal2nix "client" ./. {
    common = pkgs.haskell.packages.ghcjs.callCabal2nix "common" ../common {};
  };

  _client_pkg = pkgs.stdenv.mkDerivation {
    name = "client";
    src = ./.;
    installPhase = ''
      mkdir -p $out/static
      ${pkgs.closurecompiler}/bin/closure-compiler ${_client}/bin/client.jsexe/all.js > $out/static/all.js
    '';
  };

in

  if pkgs.lib.inNixShell then _client.env else _client_pkg


