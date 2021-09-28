let nixpkgs = import ./nixpkgs.nix;
in
{
  pkgs ? import nixpkgs {
    overlays = import ./overlay.nix {
      inherit hexOrganization hexApiKey robotSshKey;
    };
  },
  hexOrganization ? null, # organization account name on hex.pm
  hexApiKey ? null,       # plain text account API key on hex.pm
  robotSshKey ? null      # base64-encoded private id_rsa (for private git)
}:
with pkgs;

let callPackage = lib.callPackageWith haskellPackages;
    pkg = callPackage ./pkg.nix {inherit stdenv;};
    systemDeps = [ protobuf makeWrapper cacert ];
    testDeps = [ postgresql ];
in
  haskell.lib.overrideCabal pkg (drv: {
    setupHaskellDepends =
      if drv ? "setupHaskellDepends"
      then drv.setupHaskellDepends ++ systemDeps
      else systemDeps;
    testSystemDepends =
      if drv ? "testSystemDepends"
      then drv.testSystemDepends ++ testDeps
      else testDeps;
    isExecutable = true;
    enableSharedExecutables = false;
    enableLibraryProfiling = false;
    isLibrary = false;
    doHaddock = false;
    prePatch = "hpack --force";
    preCheck = ''
      source ./nix/export-test-envs.sh;
      sh ./nix/spawn-test-deps.sh;
    '';
    postCheck = ''
      sh ./nix/shutdown-test-deps.sh
    '';
    postFixup = "rm -rf $out/lib $out/nix-support $out/share/doc";
    postInstall = ''
      wrapProgram "$out/bin/reckless-trading-bot-exe" \
        --set SYSTEM_CERTIFICATE_PATH "${cacert}/etc/ssl/certs"
    '';
  })
