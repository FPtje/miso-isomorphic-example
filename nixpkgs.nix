let

  bootstrap = import <nixpkgs> {};

  nixpkgs-src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    rev = "3fd87ad0073fd1ef71a8fcd1a1d1a89392c33d0a";
    sha256 = "0n4ffwwfdybphx1iyqz1p7npk8w4n78f8jr5nq8ldnx2amrkfwhl";
  };

  servant-src = bootstrap.fetchFromGitHub {
    owner = "haskell-servant";
    repo = "servant";
    rev = "v0.15";
    sha256 = "0n9xn2f61mprnvn9838zbl4dv2ynnl0kxxrcpf5c0igdrks8pqws";
  };

  miso-src = bootstrap.fetchFromGitHub {
    owner = "haskell-miso";
    repo = "miso";
    rev = "0.21.2.0";
    sha256 = "07k1rlvl9g027fp2khl9kiwla4rcn9sv8v2dzm0rzf149aal93vn";
  };

  config = { 
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {

          ghc = pkgs.haskell.packages.ghc844.override {
            overrides = self: super: with pkgs.haskell.lib; {
                miso = self.callCabal2nix "miso" miso-src {};
              };
            };

          } // {

          # Many packages don't build on ghcjs because of a dependency on doctest
          # (which doesn't build), or because of a runtime error during the test run.
          # See: https://github.com/ghcjs/ghcjs/issues/711
          ghcjs = pkgs.haskell.packages.ghcjs84.override {
            overrides = self: super: with pkgs.haskell.lib; {
              tasty-quickcheck = dontCheck super.tasty-quickcheck;
              http-types       = dontCheck super.http-types;
              comonad          = dontCheck super.comonad;
              semigroupoids    = dontCheck super.semigroupoids;
              lens             = dontCheck super.lens;

              miso    = self.callCabal2nix "miso" miso-src {};
              servant = dontCheck (doJailbreak (self.callCabal2nix "servant" (servant-src + "/servant") {}));
            };
          };

        };
      };
    };
  };

in

  import nixpkgs-src { inherit config; }

