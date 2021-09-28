#!/bin/sh

export VIM_BACKGROUND="${VIM_BACKGROUND:-light}"
export VIM_COLOR_SCHEME="${VIM_COLOR_SCHEME:-PaperColor}"
export BITFINEX_API_KEY="${BITFINEX_API_KEY:-TODO}"
export BITFINEX_PRV_KEY="${BITFINEX_PRV_KEY:-TODO}"

docker run -it --rm \
  -v "$(pwd):/app" \
  -v "nix:/nix" \
  -v "nix-19.09-root:/root" \
  -w "/app" nixos/nix:2.3 \
  nix-shell ./nix/shell.nix --pure \
   -v --show-trace \
   --argstr vimBackground $VIM_BACKGROUND \
   --argstr vimColorScheme $VIM_COLOR_SCHEME \
   --argstr bitfinexApiKey $BITFINEX_API_KEY \
   --argstr bitfinexPrvKey $BITFINEX_PRV_KEY \
   --option http2 false \
   --option sandbox false \
   --option extra-substituters 'https://cache.nixos.org https://hydra.iohk.io https://all-hies.cachix.org' \
   --option trusted-public-keys 'cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= all-hies.cachix.org-1:JjrzAOEUsD9ZMt8fdFbzo3jNAyEWlPAwdVuHw4RD43k='
