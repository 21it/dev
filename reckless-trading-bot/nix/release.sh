#!/bin/sh

set -e

./nix/bootstrap.sh

NIXPKGS_ALLOW_BROKEN=1 nix-build ./nix/docker.nix \
  -I ssh-config-file=/tmp/.ssh/config \
  --argstr hexOrganization tkachuk-labs \
  --argstr hexApiKey $HEX_API_KEY \
  --argstr robotSshKey $ROBOT_SSH_KEY \
  --option sandbox false \
  -v --show-trace \
  --option extra-substituters "https://cache.nixos.org https://hydra.iohk.io https://all-hies.cachix.org" \
  --option trusted-public-keys "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= all-hies.cachix.org-1:JjrzAOEUsD9ZMt8fdFbzo3jNAyEWlPAwdVuHw4RD43k="
