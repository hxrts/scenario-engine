# AGENTS.md

Guidance for AI coding agents working in this repository.

## Status

The system is implemented. `docs/` is the specification and the source of truth. Read the relevant spec before changing behaviour, and record design changes in the specs. Items marked **Planned** in the specs are not implemented.

## Commands

- Toolchain: the Nix flake (`use flake` via direnv) provides node 22 and just. The dev shell runs `npm ci` into `node_modules/.bin`, which pins `purescript` 0.15.16, `spago` 1.0.5, `purs-tidy` 0.11.1 and `shadcn` 4.23.0.
- `just build`: compile everything. `just test`: every package's suite. `just test-pkg core`: one package.
- Single test: `spago test -p sp-core -- --example "<test name>"`.
- `just fmt`: purs-tidy. `just check`: fmt-check, deps, build, test. `just all`: `check` plus the dashboard Playwright smoke test (`just dashboard-test`, which needs `cd apps/dashboard && npx playwright install` once).
- `just sim meridian --preset Conservative --rounds 5`: play a game from the CLI.
- `just session meridian --humans US --model anthropic/claude-opus-5-5`: live facilitator console. Non-human seats are AI agents through the `pi` binary and make billed model calls.
- `just live-interpret`: one billed model call that checks the joint pass against the real model. It needs `ALLOW_BILLED_LIVE=1`, and no suite runs it.
- `just serve --db runs.db`: build the dashboard and serve it with the JSON API on :8787. Ports are in `apps/dashboard/ports.mjs`.
- Recipes starting with `_` are internal steps. They are hidden from `just --list` but still runnable. `just _scenarios` compiles the DSL; `test`, `sim`, `session`, `serve`, and `all` run it first.

## Where things live

- Package layout, dependency rules, and key interfaces: `docs/07_architecture.md` (§3 and §4). `scripts/check-deps` enforces the dependency table.
- Engine: `packages/core/src/Core/Engine/Commit.purs` (`resolveRound`). Round runner: `packages/effects/src/Effects/Round.purs`. Effect rows: `packages/effects/src/Effects/Signatures.purs`. Handlers: `packages/handlers/src`; the in-memory test handler is in `packages/handlers/test`.
- Invariants: `packages/core/src/Core/Invariants.purs`. `check` is shared by the tests, harness, adapters, and CLI. It checks INV-02, 04, 05, 06, and 10. The engine does not call it, so a failing round is reported, not blocked.
- Numerics: `packages/core/src/Core/Num.purs` (`Q`) is the only implementation of economic quantities. Use `Q.parse`, `Q.format`, `Q.divMaybe`, `Q.divFloor`, and `Q.proRata`. Display goes through `Q.displayQuantity`, `Q.displayPrice`, or `Q.displayRatio`. Never use `Number`, `parseFloat`, or `toFixed` for economic values.
- Randomness: draws come from `Core.Draw.draw`. The player seed is drawn once from the OS in `packages/adapters/src/Adapters/Seed.purs`.
- Scenarios: authored in the DSL under `packages/scenarios/src/DSL/` and `src/Scenarios/`. `packages/scenarios` does no IO. `adapters/Scenarios` reads the scenario directory, and `apps/cli`'s `Compile` writes the compiled files. Compiled JSON is gitignored and never hand-edited. `capstone/transcript.json` is a committed recording. Fixture expected values are literals from the worked examples in `docs/05_test_instance.md`, never computed.
- Dashboard: `apps/dashboard` is PureScript `react-basic-hooks` over shadcn/ui components generated into `apps/dashboard/ui/` (`just _ui`; not committed). View models are in `packages/harness/src/Harness/Views/`. Keep business logic out of the frontend.
- `work/` is gitignored scratch space for implementation notes. It is not part of the spec.

## Spec map (`docs/`)

Read them in order. Each layer builds on the one before it.

1. `01_overview.md`: concept and design principles
2. `02_type_system.md`: glossary (§2.5), types and rules, the canonical round sequence (§8.1), effects (§8.4), invariants `INV-xx` (§9), numerics (§9.1)
3. `03_gameboard.md`: how a scenario is configured
4. `04_instantiated_game.md`: initialization and live round execution
5. `05_test_instance.md`: *Meridian*, the reference scenario, and fixtures A–G
6. `06_simulator.md`: simulation harness, run store, dashboard, mandatory tests
7. `07_architecture.md`: package layout, dependency direction, key interfaces
8. `08_coverage.md`: feature-to-test audit and capstone run (not normative)

Spec conventions:
- Cite invariants as `INV-xx` and Type System sections by number. IDs 18, 20, and 32–35 are retired.
- Define terms only in the Type System glossary (§2.5). The other docs reuse them.
- Never add open-question sections. Record a decision where it belongs.

## Architecture (big picture)

- **Two parts.** The pure engine, `resolveRound`, is the only code that changes the economy. The round runner runs Propose and then Sign for every Policy and hands the signed Proposals to the engine. The engine checks authority and signatures and balances the books. It never authors or rewrites an Intent.
- **Purity.** All impurity goes through algebraic effects. Handlers choose the context and contain no game logic.
- **Representation lags one round.** The engine stores an adjustment factor in round r, and the Aggregate's Policy applies it in round r+1.
- **Replay.** Replay under the Replay handler must reproduce a run exactly.
- **Layers.** The dependency chain and the module map are in `docs/07_architecture.md` §3.

## Tests

Fixtures A–G are golden tests against `resolveRound` (`packages/scenarios/test`). The mandatory tests are in `docs/06_simulator.md` §14. The coverage audit is `docs/08_coverage.md`.
