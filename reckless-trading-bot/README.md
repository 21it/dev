# reckless-trading-bot

Interactive web chat based on `concur-replica` (Haskell live view).

## Development

Sometimes projects are using dependencies from private repositories. Docker and Nix can get access to private repositories through environment variables:

```shell
vi ~/.profile

# hex.pm access to private Erlang/Elixir dependencies
export HEX_API_KEY="SECRET"
# hex.pm organization
export HEX_ORGANIZATION="tkachuk-labs"
# ssh access to private git dependencies
export ROBOT_SSH_KEY="$(cat ~/.ssh/id_rsa | base64 --wrap=0)"
# git user info
export GIT_AUTHOR_NAME="tkachuk-labs"
export GIT_AUTHOR_EMAIL="tkachuk.labs@gmail.com"
# optional appearance parameters
export VIM_BACKGROUND="light" # or "dark"
export VIM_COLOR_SCHEME="PaperColor" # or "jellybeans"
```

And then:

```shell
# start nix-shell
./nix/shell.sh

# run tests in nix-shell
stack test

# develop in nix-shell
vi .
```
