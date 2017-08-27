{ mkDerivation, aeson, base, lens, containers, http-types, lucid, miso
, mtl, network-uri, servant, servant-lucid, servant-server, stdenv
, wai, wai-extra, warp
}:
mkDerivation {
  pname = "miso-isomorphic-example";
  version = "0.1.0.0";
  src = ../.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base containers http-types lucid miso mtl network-uri servant
    servant-lucid servant-server wai wai-extra warp lens
  ];
  description = "Example showing the isomorphic aspect of miso";
  license = stdenv.lib.licenses.unfree;
}
