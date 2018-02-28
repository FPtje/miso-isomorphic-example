let
  # nixpkgs pinning method from
  # Sneaking Nix at $work – and become a hero, hopefully by zimbatm
  # See http://nixcon2017.org/#program
  fetchTarball-compat = { url, sha256 }@attrs:
    let
     inherit (builtins) lessThan nixVersion fetchTarball;
    in
    if lessThan nixVersion "1.12" then
     fetchTarball { inherit url; }
    else
     fetchTarball attrs;

  nixpkgs-src = fetchTarball-compat {
   url = "https://github.com/NixOS/nixpkgs-channels/archive/c1d9aff56e0ae52ee4705440fe09291a51e91977.tar.gz";
   sha256 = "1dsszs9ds17n006yv4il39640msslmhdh3ckg4l0vfq8y11ljvss";
  };

  pkgs = import (nixpkgs-src) {};

  inherit (pkgs) runCommand closurecompiler;
  inherit (pkgs.haskell.packages) ghcjsHEAD ghc802;

  miso-src = pkgs.fetchFromGitHub {
    rev = "a03f9a1e370d369722d643d1cc93dc0a13fab59b";
    sha256 = "16r64x39x42fp576c5wcvwphanyckmzvzmv7yw8djyg8p4wrqrzj";
    owner = "FPtje";
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
