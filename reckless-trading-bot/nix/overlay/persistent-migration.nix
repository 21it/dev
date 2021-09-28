{ mkDerivation, base, bytestring, conduit, containers, exceptions
, fetchgit, fgl, hpack, monad-logger, mtl, persistent
, persistent-postgresql, persistent-template, process, QuickCheck
, resource-pool, stdenv, tasty, tasty-golden, tasty-quickcheck
, temporary, text, time, unordered-containers, yaml
}:
mkDerivation {
  pname = "persistent-migration";
  version = "0.1.0";
  src = fetchgit {
    url = "https://github.com/tkachuk-labs/persistent-migration";
    sha256 = "1ysjkfgaxr8iv9g836ky9792sp7fanp0d09c4sgz1cwn4r6za4xq";
    rev = "3afe6367363efafbc788ea72eb2c0492884d458a";
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
  license = stdenv.lib.licenses.bsd3;
}
