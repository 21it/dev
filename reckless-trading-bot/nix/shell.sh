#!/bin/sh

export VIM_BACKGROUND="${VIM_BACKGROUND:-light}"
export VIM_COLOR_SCHEME="${VIM_COLOR_SCHEME:-PaperColor}"
export BITFINEX_API_KEY="${BITFINEX_API_KEY:-TODO}"
export BITFINEX_PRV_KEY="${BITFINEX_PRV_KEY:-TODO}"
echo "starting nixos container"
docker run -it --rm \
  -e NIXPKGS_ALLOW_BROKEN=1 \
  -e ROBOT_SSH_KEY="$ROBOT_SSH_KEY" \
  -v "$(pwd):/app" \
  -v "nix:/nix" \
  -v "nix-19.09-root:/root" \
  -w "/app" nixos/nix:2.3 sh -c "
  ./nix/bootstrap.sh &&
  nix-shell ./nix/shell.nix --pure \
   -I ssh-config-file=/tmp/.ssh/config \
   --argstr hexOrganization $HEX_ORGANIZATION \
   --argstr hexApiKey $HEX_API_KEY \
   --argstr robotSshKey $ROBOT_SSH_KEY \
   --argstr vimBackground $VIM_BACKGROUND \
   --argstr vimColorScheme $VIM_COLOR_SCHEME \
   --argstr gitAuthorName $GIT_AUTHOR_NAME \
   --argstr gitAuthorEmail $GIT_AUTHOR_EMAIL \
   --argstr bitfinexApiKey $BITFINEX_API_KEY \
   --argstr bitfinexPrvKey $BITFINEX_PRV_KEY \
   --option sandbox false \
   -v --show-trace \
   --option extra-substituters 'https://cache.nixos.org https://hydra.iohk.io https://all-hies.cachix.org' \
   --option trusted-public-keys 'cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= all-hies.cachix.org-1:JjrzAOEUsD9ZMt8fdFbzo3jNAyEWlPAwdVuHw4RD43k='
  "
