# Scenario Planning Game

A multiplayer economic strategy game where players act as governments, corporations, or institutions in a shared world economy. Each round, player actions and the simulated behavior of the rest of the economy are fed together into a deterministic engine that resolves the new world state.

## Status

Design stage. The specification is in [`docs/`](docs/); start with [`01_overview.md`](docs/01_overview.md).

## Development

Requires Nix with direnv (`use flake`). The dev shell installs the pinned PureScript toolchain.

```sh
just build   # compile
just test    # run tests
just run     # run Main
```
