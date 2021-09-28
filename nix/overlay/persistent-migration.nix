{ mkDerivation, base, bytestring, conduit, containers, exceptions
, fetchgit, fgl, hpack, lib, monad-logger, mtl, persistent
, persistent-postgresql, persistent-template, process, QuickCheck
, resource-pool, tasty, tasty-golden, tasty-quickcheck, temporary
, text, time, unordered-containers, yaml
}:
mkDerivation {
  pname = "persistent-migration";
  version = "0.1.0";
  src = fetchgit {
    url = "https://github.com/21it/persistent-migration";
    sha256 = "1v993ckrhgm7v3f1km174r2r93scpxhc5v1ckfhmqay391jf5qzv";
    rev = "8d5d50204eec2d3d4bd9bb1f61e79ece2035ff8c";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base containers fgl mtl persistent text time unordered-containers
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    base bytestring conduit containers exceptions monad-logger mtl
    persistent persistent-postgresql persistent-template process
    QuickCheck resource-pool tasty tasty-golden tasty-quickcheck
    temporary text time yaml
  ];
  prePatch = "hpack";
  homepage = "https://github.com/brandonchinn178/persistent-migration#readme";
  description = "Manual migrations for the persistent library";
  license = lib.licenses.bsd3;
}
