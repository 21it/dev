#!/bin/sh

set -e

THIS_DIR="$(dirname "$(realpath "$0")")"
USER="${USER:-user}"
NIX_CONF="http2 = false
trusted-users = root $USER
extra-substituters = https://cache.nixos.org https://hydra.iohk.io https://all-hies.cachix.org
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= all-hies.cachix.org-1:JjrzAOEUsD9ZMt8fdFbzo3jNAyEWlPAwdVuHw4RD43k=
"
BITFINEX_API_KEY="${BITFINEX_API_KEY:-TODO}"
BITFINEX_PRV_KEY="${BITFINEX_PRV_KEY:-TODO}"

MINISHELL=false
if [ -z "$*" ]; then
  true
else
  for arg in "$@"
  do
    case $arg in
      -m|--mini|--minishell)
      MINISHELL=true
      shift
      ;;
      *)
      break
      ;;
    esac
  done
fi
NIX_EXTRA_ARGS="$@"

docker run -it --rm \
  -v "$THIS_DIR/..:/app" \
  -v "nix-$USER:/nix" \
  -v "nix-home-$USER:/home/$USER" \
  -w "/app" nixos/nix:2.3.12 \
  sh -c "
  adduser $USER -D &&
  echo \"$NIX_CONF\" >> /etc/nix/nix.conf &&
  (nix-daemon &) &&
  sleep 1 &&
  su $USER -c \"NIX_REMOTE=daemon \
      nix-shell \
      ./nix/shell.nix \
      --pure \
      --show-trace -v \
      --arg minishell $MINISHELL \
      --argstr bitfinexApiKey $BITFINEX_API_KEY \
      --argstr bitfinexPrvKey $BITFINEX_PRV_KEY \
      $NIX_EXTRA_ARGS\"
  "
