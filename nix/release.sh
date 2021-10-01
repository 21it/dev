#!/bin/sh

set -e

build () {
  nix-build "./$1/nix/$2.nix" \
    -v --show-trace \
    --out-link "result-$1-$2" \
    --option http2 false \
    --option sandbox false \
    --option extra-substituters "https://cache.nixos.org https://hydra.iohk.io https://all-hies.cachix.org" \
    --option trusted-public-keys "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= all-hies.cachix.org-1:JjrzAOEUsD9ZMt8fdFbzo3jNAyEWlPAwdVuHw4RD43k="
}

build bitfinex-client default
build reckless-trading-bot default
build reckless-trading-bot docker
