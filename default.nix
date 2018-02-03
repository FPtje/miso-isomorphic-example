{ pkgs ? import <nixpkgs> {} }:
let
  inherit (pkgs) runCommand closurecompiler;
  inherit (pkgs.haskell.packages) ghcjsHEAD ghc802;

  miso-src = pkgs.fetchFromGitHub {
    rev = "adea51505f30853caae76f38faa5e9f192ae8827";
    sha256 = "0x8dik2cx5j11svb091pzy2ycvhbb88534fng2v9bic5yx1a8c72";
    owner = "haskell-miso";
    repo = "miso";
  };

  miso-ghc   = ghc802.callCabal2nix "miso" miso-src {};
  miso-ghcjs = ghcjsHEAD.callCabal2nix "miso" miso-src {};

  server = ghc802.callPackage ./server { miso = miso-ghc; };
  client = ghcjsHEAD.callPackage ./client { miso = miso-ghcjs; };
in
  runCommand "miso-ismorphic-example" { inherit client server; } ''
    mkdir -p $out/{bin,static}
    ${closurecompiler}/bin/closure-compiler ${client}/bin/client.jsexe/all.js > $out/static/all.js
    cp ${server}/bin/* $out/bin
  ''
