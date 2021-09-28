# bitfinex-client

Bitfinex client API library for Haskell

## Development

```shell
vi ~/.profile

# bitfinex user data
export BITFINEX_API_KEY="SECRET"
export BITFINEX_PRV_KEY="SECRET"
# optional appearance parameters
export VIM_BACKGROUND="light" # or "dark"
export VIM_COLOR_SCHEME="PaperColor" # or "jellybeans"
```

And then:

```shell
# start nix-shell
./nix/shell.sh

# run tests in nix-shell
stack test --fast

# develop in nix-shell
vi .
```
