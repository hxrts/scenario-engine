# Scenario Planning Game

A multiplayer economic strategy game where players act as governments, corporations, or institutions in a shared world economy. Each round, player actions and the simulated behavior of the rest of the economy are fed together into a deterministic engine that resolves the new world state.

## Status

Implemented against the specification in [`docs/`](docs/); start with [`01_overview.md`](docs/01_overview.md). The reference scenario is *The Fractured World*.

## Development

Requires Nix with direnv (`use flake`). The dev shell installs the pinned PureScript toolchain.

```sh
just build                                                  # compile
just check                                                  # format, dependency direction, build, tests
just all                                                    # check + dashboard smoke test
just run run fractured-world --preset Conservative          # simulate a game
just run session fractured-world --humans US                # live session (AI seats via pi)
just serve --db runs.db                                     # dashboard and JSON API on :8787
```
