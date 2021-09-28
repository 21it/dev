let nixpkgs = import ./nixpkgs.nix;
in
{
  pkgs ? import nixpkgs {
    overlays = import ./overlay.nix {
      inherit hexOrganization hexApiKey robotSshKey vimBackground vimColorScheme;
    };
  },
  hexOrganization ? null, # organization account name on hex.pm
  hexApiKey ? null,       # plain text account API key on hex.pm
  robotSshKey ? null,     # base64-encoded private id_rsa (for private git)
  vimBackground ? "light",
  vimColorScheme ? "PaperColor",
  gitAuthorName,
  gitAuthorEmail,
  bitfinexApiKey ? "TODO",
  bitfinexPrvKey ? "TODO",
}:
with pkgs;

stdenv.mkDerivation {
  name = "reckless-trading-bot-shell";
  buildInputs = [
    /* IDE */
    haskell-ide
    zlib
    /* Apps */
    postgresql
    /* Utils */
    git
    nix-prefetch-scripts
    openssh
    cabal2nix
    protobuf
    cacert
    xxd
  ];

  TERM="xterm-256color";
  LC_ALL="C.UTF-8";
  GIT_SSL_CAINFO="${cacert}/etc/ssl/certs/ca-bundle.crt";
  NIX_SSL_CERT_FILE="${cacert}/etc/ssl/certs/ca-bundle.crt";
  NIX_PATH="/nix/var/nix/profiles/per-user/root/channels";
  HEX_ORGANIZATION=hexOrganization;
  HEX_API_KEY=hexApiKey;
  ROBOT_SSH_KEY=robotSshKey;
  GIT_AUTHOR_NAME=gitAuthorName;
  GIT_AUTHOR_EMAIL=gitAuthorEmail;
  EMAIL = gitAuthorEmail;
  BITFINEX_API_KEY=bitfinexApiKey;
  BITFINEX_PRV_KEY=bitfinexPrvKey;
  shellHook = ''
    source ./nix/export-test-envs.sh
    sh ./nix/spawn-test-deps.sh

    export HOOGLEDB=/root/.hoogle
    if [ "$(ls -A $HOOGLEDB)" ]; then
      echo "hoogle database already exists..."
    else
      echo "building hoogle database..."
      stack --stack-yaml=/app/stack.yaml exec hoogle generate
    fi

    if [ "$(ls -A ~/.cabal)" ]; then
      echo "cabal database already exists..."
    else
      echo "building cabal database..."
      cabal v2-update
    fi
  '';
}
