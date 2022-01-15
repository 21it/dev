{ mkDerivation, base, bytestring, containers, fetchgit, HUnit, lib
, template-haskell, text, time
}:
mkDerivation {
  pname = "witch";
  version = "0.3.4.0";
  src = fetchgit {
    url = "https://github.com/21it/witch.git";
    sha256 = "1v9n9jfyzv6s0jhbx38v32zn9gfhanwca1k331yf7rgvirvk7b9y";
    rev = "4514e5b86ad209d660ee38291ec2614d810411e8";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base bytestring containers template-haskell text time
  ];
  testHaskellDepends = [
    base bytestring containers HUnit text time
  ];
  description = "Convert values from one type into another";
  license = lib.licenses.isc;
}
