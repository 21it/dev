{ mkDerivation, aeson, async, base, bitfinex-client, bytestring
, containers, envparse, errors, esqueleto, extra
, generic-pretty-instances, GenericPretty, hpack, hspec
, http-client, http-client-tls, katip, lib, monad-logger
, persistent, persistent-migration, persistent-postgresql
, resource-pool, servant-client, singletons, stm
, telegram-bot-simple, template-haskell, time, transformers
, unbounded-delays, units, universum, unliftio, witch
}:
mkDerivation {
  pname = "reckless-trading-bot";
  version = "0.1.0.0";
  src = ./..;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async base bitfinex-client bytestring containers envparse
    errors esqueleto extra generic-pretty-instances GenericPretty
    http-client http-client-tls katip monad-logger persistent
    persistent-migration persistent-postgresql resource-pool
    servant-client singletons stm telegram-bot-simple template-haskell
    time transformers unbounded-delays units universum unliftio witch
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec ];
  prePatch = "hpack";
  homepage = "https://github.com/21it/reckless-trading-bot#readme";
  license = lib.licenses.bsd3;
}
