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
   url = "https://github.com/NixOS/nixpkgs-channels/archive/3fd87ad0073fd1ef71a8fcd1a1d1a89392c33d0a.tar.gz";
   sha256 = "0n4ffwwfdybphx1iyqz1p7npk8w4n78f8jr5nq8ldnx2amrkfwhl";
  };

  pkgs = import (nixpkgs-src) {};

  inherit (pkgs) runCommand closurecompiler;
  inherit (pkgs.haskell.packages) ghcjs84 ghc844;

  servant-src = pkgs.fetchFromGitHub {
    owner = "haskell-servant";
    repo = "servant";
    rev = "v0.15";
    sha256 = "0n9xn2f61mprnvn9838zbl4dv2ynnl0kxxrcpf5c0igdrks8pqws";
  };

  # Many packages don't build on ghcjs because of a dependency on doctest
  # (which doesn't build), or because of a runtime error during the test run.
  # See: https://github.com/ghcjs/ghcjs/issues/711
  ghcjs = ghcjs84.override {
    overrides = final: previous: with pkgs.haskell.lib; {
      tasty-quickcheck = dontCheck previous.tasty-quickcheck;
      http-types       = dontCheck previous.http-types;
      comonad          = dontCheck previous.comonad;
      semigroupoids    = dontCheck previous.semigroupoids;
      lens             = dontCheck previous.lens;
      servant          = dontCheck (doJailbreak (previous.callCabal2nix "servant" (servant-src + "/servant") {}));
    };
  };

  miso-src = pkgs.fetchFromGitHub {
    rev = "0.21.2.0";
    sha256 = "07k1rlvl9g027fp2khl9kiwla4rcn9sv8v2dzm0rzf149aal93vn";
    owner = "haskell-miso";
    repo = "miso";
  };

  miso-ghc   = ghc844.callCabal2nix "miso" miso-src {};
  miso-ghcjs = ghcjs.callCabal2nix "miso" miso-src {};

  server = ghc844.callPackage ./server { miso = miso-ghc; };
  client = ghcjs.callPackage ./client { miso = miso-ghcjs; };
in
  runCommand "miso-ismorphic-example" { inherit client server; } ''
    mkdir -p $out/{bin,static}
    ${closurecompiler}/bin/closure-compiler ${client}/bin/client.jsexe/all.js > $out/static/all.js
    cp ${server}/bin/* $out/bin
  ''
