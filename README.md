# 21it

Executive Order 6102 was a presidential directive which forbade the hoarding of gold in the United States – citizens were instructed to deliver their gold to the Federal Reserve within a month of the signing of the order in April 1933. Violating the order carried a punishment of 10 years in prison, a $10,000 fine or both. The $10,000 fine is equivalent to almost $200,000 today due inflation of the US Dollar. Less than 100 years ago, in the United States of America, it was illegal to hoard gold. It was illegal to have a shiny rock in your home. I find that fact astonishing. (6102, June 4, 2020, SLP178)

## Configuration

Configure Bitfinex user data:

```sh
vi ~/.profile

export BITFINEX_API_KEY="SECRET"
export BITFINEX_PRV_KEY="SECRET"
```

## Quickstart

Requirements:

- Running [Docker/Swarm](https://docs.docker.com/engine/swarm/swarm-tutorial/create-swarm/)
- On Mac you also need `brew install coreutils`

Run the following command to spawn `reckless-trading-bot` using prebuilt binaries from github:

```sh
./nix/ds-setup.sh --prebuilt
```

To build `reckless-trading-bot` from source, make sure Docker have access to reasonable amount of resources (at least 8GB of memory, reasonable storage and CPU capacity). Initial compilation will take a lot of time, CPU, memory, bandwidth and storage, but it's needed to be done only once. Run the following command to build `reckless-trading-bot` from source and run it:

```sh
./nix/ds-setup.sh
```

## Development

Spawn nix-shell:

```sh
./nix/shell.sh
```

Everything following is supposed to be run from inside of spawned nix-shell. Run IDE:

```sh
vi .
```

Run tests:

```sh
stack test --fast --file-watch bitfinex-client
```

Run specific test:

```sh
stack test --fast --file-watch --test-arguments="-m platformStatus" bitfinex-client
```

Run development daemon:

```sh
ghcid
```
