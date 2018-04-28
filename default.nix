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
   sha256 = "13qfydr4a4by3fnqxcczdq0zr6vsqxbdmj8grwbsk3xjhw4442p9";
  };

  pkgs = import (nixpkgs-src) {};

  inherit (pkgs) runCommand closurecompiler;
  inherit (pkgs.haskell.packages) ghcjsHEAD ghc802;

  miso-src = pkgs.fetchFromGitHub {
    rev = "c756a6771a2da437f874645b1930e12d27127650";
    sha256 = "06ra3imm65fxfw41nnns5hvhwpsr56kn13x27glv7g43vx8ny5y3";
    owner = "dmjio";
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
