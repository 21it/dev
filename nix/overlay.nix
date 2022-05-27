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
        ide21 = import (fetchTarball "https://github.com/21it/ultimate-haskell-ide/tarball/ba6c0091debe147b1a23ecdae97efd42312f9b4f") {inherit bundle;};
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
