let

  pkgs = import ./nixpkgs.nix ;
  server = pkgs.callPackage ./server/default.nix {};
  client = pkgs.callPackage ./client/default.nix {};

in

  pkgs.runCommand "miso-ismorphic-example" { inherit client server; } ''
    mkdir -p $out/{bin,static}
    cp ${server}/bin/* $out/bin/
    cp ${client}/static/* $out/static/
  ''

