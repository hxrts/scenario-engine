# Scenario Planning Game

A multiplayer economic strategy game where players act as governments, corporations, or institutions in a shared world economy. Each round, player actions and the simulated behavior of the rest of the economy are fed together into a deterministic engine that resolves the new world state.

Implemented against the specification in [`docs/`](docs/), start with [`01_overview.md`](docs/01_overview.md). *Meridian* is the reference scenario.

## Development

The nix flake's dev shell installs a pinned PureScript toolchain.

```sh
just build                                    # compile
just check                                    # format, dependency direction, build, tests
just all                                      # check + dashboard smoke test
just sim meridian --preset Conservative       # simulate a game
just session meridian --humans US             # live session (AI seats via pi)
just serve                                    # dashboard and JSON API on :8787
```

The dashboard smoke test (`just dashboard-test`) also needs the Playwright browsers, installed once: `cd apps/dashboard && npx playwright install`.
