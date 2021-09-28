{ mkDerivation, base, exceptions, fetchgit, free, hpack, mtl
, natural-transformation, resourcet, stdenv, tasty, tasty-hunit
, transformers, unliftio
}:
mkDerivation {
  pname = "concur-core";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/tkachuk-labs/concur";
    sha256 = "0xj2s57s6r8kh76yjpdsp9vm4g5vwg75fqc29lfw1z2igkb0igyz";
    rev = "5d7366a25c0688b504ca263ee689b4f6618313d1";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/concur-core; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base exceptions free mtl natural-transformation resourcet
    transformers unliftio
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    base exceptions free mtl natural-transformation resourcet tasty
    tasty-hunit transformers unliftio
  ];
  prePatch = "hpack";
  homepage = "https://github.com/ajnsit/concur#readme";
  description = "A UI framework for Haskell. Core framework.";
  license = stdenv.lib.licenses.bsd3;
}
