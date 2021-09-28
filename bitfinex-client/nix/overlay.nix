{
  vimBackground ? "light",
  vimColorScheme ? "PaperColor"
}:
[(self: super:
    let
      callPackage = self.lib.callPackageWith self.haskellPackages;
      dontCheck = self.haskell.lib.dontCheck;
      doJailbreak = self.haskell.lib.doJailbreak;
    in
      {
        haskell-ide = import (
          fetchTarball "https://github.com/tkachuk-labs/ultimate-haskell-ide/tarball/1812de1b890900f0298dbf768cfed73ab74f41e4"
          ) { inherit vimBackground vimColorScheme; };
        haskellPackages = super.haskell.packages.extend(
          self': super': {
            persistent-migration = dontCheck (
              callPackage ./overlay/persistent-migration.nix {
                stdenv = self.stdenv;
                fetchgit = self.fetchgit;
              });
          }
        );
      }
)]
