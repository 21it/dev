{ mkDerivation, aeson, async, base, bytestring, chronos
, co-log-core, containers, cryptonite, Diff, fetchgit, file-embed
, hex-text, hpack, http-media, http-types, psqueues, QuickCheck
, quickcheck-instances, resourcet, stdenv, stm, template-haskell
, text, torsor, wai, wai-websockets, websockets
}:
mkDerivation {
  pname = "replica";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/tkachuk-labs/replica";
    sha256 = "1hlfsdfwciyqjc9m1ph756090arjb3dggj9xc8r7anlzc1lsmfl8";
    rev = "e4b0988f7a4053fb7edc73e6267b00bfbfb70178";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    aeson async base bytestring chronos co-log-core containers
    cryptonite Diff file-embed hex-text http-media http-types psqueues
    resourcet stm template-haskell text torsor wai wai-websockets
    websockets
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    aeson async base bytestring chronos co-log-core containers
    cryptonite Diff file-embed hex-text http-media http-types psqueues
    QuickCheck quickcheck-instances resourcet stm template-haskell text
    torsor wai wai-websockets websockets
  ];
  prePatch = "hpack";
  homepage = "https://github.com/https://github.com/pkamenarsky/replica#readme";
  description = "Remote virtual DOM library";
  license = stdenv.lib.licenses.bsd3;
}
