# Scenario Planning Game Engine

A multiplayer strategy game in which actors play out a global political economy scenario. Each player has a distinct set of tools they use to advance their own agenda. A pure, deterministic engine settles double-entry books each round. The reference scenario, *Pacific*, models the US and China as two complementary national regimes. Players occupy seats as either US or Chinese heads of state, or business leaders of a major firm.

The game is written in PureScript. It has a CLI, a JSON API server, and a dashboard for comparing simulated runs and facilitating live sessions.

The [`docs/`](docs/) include overview, kernel, premise, run, *Pacific*, simulator, the software architecture pages.

## Get Started

```sh
nix develop                                     # enter nix dev shell
just sim pacific --preset Restrict --rounds 6   # play a game from the CLI
just serve --db runs.db                         # dashboard and API on :8787 (admin password in .env)
```
