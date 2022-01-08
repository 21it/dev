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
        ide21 = import (fetchTarball "https://github.com/tkachuk-labs/ultimate-haskell-ide/tarball/78fb42182fb8bdc3b084b9f81d8e0b0df60dc73a") {inherit bundle;};
        haskellPackages = super.haskell.packages.ghc901.extend(
          self': super': {
            bitfinex-client =
              callPackage ../bitfinex-client/nix/default.nix {

              };
            persistent-migration =
              callPackage ./overlay/persistent-migration.nix {
                stdenv = self.stdenv;
                fetchgit = self.fetchgit;
              };
          }
        );
      }
)]
