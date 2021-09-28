{ mkDerivation, aeson, async, base, bitfinex-client, bytestring
, containers, envparse, esqueleto, extra, hpack, hspec, katip, lib
, persistent, persistent-postgresql, resource-pool, stm, text, time
, transformers, unbounded-delays, universum, unliftio, witch
}:
mkDerivation {
  pname = "reckless-trading-bot";
  version = "0.1.0.0";
  src = ./..;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async base bitfinex-client bytestring containers envparse
    esqueleto extra katip persistent persistent-postgresql
    resource-pool stm text time transformers unbounded-delays universum
    unliftio witch
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec ];
  prePatch = "hpack";
  homepage = "https://github.com/tkachuk-labs/reckless-trading-bot#readme";
  license = lib.licenses.bsd3;
}
