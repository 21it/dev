{
  minishell ? true
}:
[(self: super:
    let
      callPackage = self.lib.callPackageWith self.haskellPackages;
      dontCheck = self.haskell.lib.dontCheck;
      doJailbreak = self.haskell.lib.doJailbreak;
      bundle = if minishell
               then ["dhall"]
               else ["dhall" "haskell"];
    in
      {
        ide21 = import (fetchTarball "https://github.com/21it/ultimate-haskell-ide/tarball/86769387b5dc55d74f69b4566e46d966b2a9b166") {inherit bundle;};
        haskellPackages = super.haskell.packages.ghc921.extend(
          self': super': {
            bitfinex-client =
              callPackage ../bitfinex-client/nix/default.nix {

              };
            persistent-migration =
              callPackage ./overlay/persistent-migration.nix {
                stdenv = self.stdenv;
                fetchgit = self.fetchgit;
              };
            witch =
              doJailbreak (callPackage ./overlay/witch.nix {
                lib = self.lib;
                fetchgit = self.fetchgit;
              });
          }
        );
      }
)]
