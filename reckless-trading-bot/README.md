# reckless-trading-bot

Bitfinex trading bot.

## Development

Configure Bitfinex user data:

```shell
vi ~/.profile

export BITFINEX_API_KEY="SECRET"
export BITFINEX_PRV_KEY="SECRET"
```

Spawn nix-shell:

```shell
./nix/shell.sh
```

Develop in nix-shell:

```shell
vi .
```

Run tests in nix-shell:

```shell
stack test --fast --file-watch reckless-trading-bot
```

Run development daemon:

```shell
ghcid
```
