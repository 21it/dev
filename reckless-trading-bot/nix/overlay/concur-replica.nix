{ mkDerivation, aeson, base, byteorder, bytestring, chronos, co-log
, concur-core, containers, fetchgit, free, hpack, http-types, mtl
, network, random, replica, stdenv, text, torsor, transformers, wai
, wai-websockets, warp, websockets
}:
mkDerivation {
  pname = "concur-replica";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/tkachuk-labs/concur-replica";
    sha256 = "058a8bx5kdf5pz39gvy4mmw5xg8jf8z85bjzbip68wqbngb1xy7b";
    rev = "cdbf120bfbe2b74448985417261986a6c572b15e";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base byteorder bytestring chronos co-log concur-core
    containers free http-types network replica text torsor transformers
    wai wai-websockets warp websockets
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base byteorder bytestring chronos co-log concur-core
    containers free http-types mtl network random replica text torsor
    transformers wai wai-websockets warp websockets
  ];
  prePatch = "hpack";
  homepage = "https://github.com/pkamenarsky/concur-replica#readme";
  description = "Replica backend for Concur";
  license = stdenv.lib.licenses.bsd3;
}
