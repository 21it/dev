{ mkDerivation, aeson, base, base16-bytestring, bytestring
, containers, cryptonite, envparse, hpack, hspec, http-client
, http-client-tls, http-types, lens-aeson, lib, memory
, siggy-chardust, text, time, transformers, universum, unliftio
, unordered-containers, vector, witch
}:
mkDerivation {
  pname = "bitfinex-client";
  version = "0.1.0.0";
  src = ./..;
  libraryHaskellDepends = [
    aeson base base16-bytestring bytestring containers cryptonite
    envparse http-client http-client-tls http-types lens-aeson memory
    siggy-chardust text time transformers universum unliftio
    unordered-containers vector witch
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [ aeson base hspec time transformers ];
  prePatch = "hpack";
  homepage = "https://github.com/tkachuk-labs/bitfinex-client#readme";
  license = lib.licenses.bsd3;
}
