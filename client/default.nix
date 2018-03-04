{ mkDerivation, aeson, base, lens, containers, miso, servant, stdenv }:
mkDerivation {
  pname = "miso-isomorphic-example";
  version = "0.1.0.0";
  src = ../.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ aeson base containers miso lens servant ];
  description = "Example showing the isomorphic aspect of miso";
  license = stdenv.lib.licenses.unlicense;
}
