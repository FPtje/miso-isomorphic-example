let

  bootstrap = import <nixpkgs> {};

  nixpkgs-src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    rev = "725b5499b89fe80d7cfbb00bd3c140a73cbdd97f";
    sha256 = "0xdhv9k0nq8d91qdw66d6ln2jsqc9ij7r24l9jnv4c4bfpl4ayy7";
  };

  config = { 
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {

          ghc = pkgs.haskell.packages.ghc864;

          } // {

          # Many packages don't build on ghcjs because of a dependency on doctest
          # (which doesn't build), or because of a runtime error during the test run.
          # See: https://github.com/ghcjs/ghcjs/issues/711
          ghcjs = pkgs.haskell.packages.ghcjs86.override {
            overrides = self: super: with pkgs.haskell.lib; {
              tasty-quickcheck = dontCheck super.tasty-quickcheck;
              http-types       = dontCheck super.http-types;
              http-media       = dontCheck super.http-media;
              comonad          = dontCheck super.comonad;
              semigroupoids    = dontCheck super.semigroupoids;
              lens             = dontCheck super.lens;
              QuickCheck       = dontCheck super.QuickCheck;

              network = dontCheck (doJailbreak super.network_2_6_3_1);
              servant-client = dontCheck (doJailbreak super.servant-client);
              servant = dontCheck (doJailbreak super.servant);
            };
          };

        };
      };
    };
  };

in

  import nixpkgs-src { inherit config; }

