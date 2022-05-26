{ mkDerivation, aeson, base, base16-bytestring, bytestring
, combinat, containers, cryptonite, envparse, extra, gnuplot, hpack
, hspec, http-client, http-client-tls, http-types, lens-aeson, lib
, memory, persistent, siggy-chardust, singletons, singletons-base
, singletons-th, template-haskell, text, time, transformers, units
, universum, unliftio, unordered-containers, vector, witch
}:
mkDerivation {
  pname = "bitfinex-client";
  version = "0.1.0.0";
  src = ./..;
  libraryHaskellDepends = [
    aeson base base16-bytestring bytestring combinat containers
    cryptonite envparse extra gnuplot http-client http-client-tls
    http-types lens-aeson memory persistent siggy-chardust singletons
    singletons-base singletons-th template-haskell text time
    transformers units universum unliftio unordered-containers vector
    witch
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    aeson base containers hspec time transformers units
  ];
  prePatch = "hpack";
  homepage = "https://github.com/21it/bitfinex-client#readme";
  license = lib.licenses.bsd3;
}
