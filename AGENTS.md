# AGENTS.md

Guidance for AI coding agents working in this repository.

## Status

The system is implemented. `docs/` is the specification and the source of truth. Read the relevant spec before changing behaviour, and record design changes in the specs. Items marked **Planned** in the specs are not implemented.

## Commands

- Toolchain: the Nix flake (`use flake` via direnv) provides node 22 and just. The dev shell runs `npm ci` into `node_modules/.bin`, which pins `purescript` 0.15.16, `spago` 1.0.5, `purs-tidy` 0.11.1 and `shadcn` 4.23.0.
- `just build`: compile everything. `just test`: every package's suite. `just test-pkg core`: one package.
- Single test: `spago test -p sp-core -- --example "<test name>"`.
- `just fmt`: purs-tidy. `just check`: fmt-check, build, deps, layout, impurity, derived, test, docs. `just all`: `check` plus the dashboard Playwright smoke test (`just dashboard-test`, which needs `cd apps/dashboard && npx playwright install` once).
- `just sim meridian --preset Conservative --rounds 5`: play a game from the CLI.
- `just session meridian --humans US`: live facilitator console. Non-human seats are AI agents through the `pi` binary and make billed model calls. The model defaults to `openrouter/deepseek/deepseek-v4-flash-latest` (`Adapters.Pi`); `--model` overrides it.
- `just live-interpret`: one joint pass against the real model, which makes billed calls. It needs `ALLOW_BILLED_LIVE=1`, and no suite runs it.
- `just serve --db runs.db`: build the dashboard and serve it with the JSON API on :8787. Ports are in `apps/dashboard/ports.mjs`.
- Recipes starting with `_` are internal steps. They are hidden from `just --list` but still runnable. `just _settings` compiles the setting DSL (`Compile`) and regenerates the capstone recording (`packages/settings/capstone/transcript.json`), with no model call; `test`, `test-pkg`, `sim`, `session`, `serve`, `dashboard-test`, `live-interpret`, and `_docs` run it first.

## Where things live

- Package layout, dependency rules, and key interfaces: `docs/07_architecture.md` (§3 and §4). `scripts/check/deps.js` enforces the dependency table and the import rules; `scripts/check/layout.js` enforces the concept rules that imports cannot express; `scripts/check/impurity.js` enforces the three layers for impure work (docs/07 §1); `scripts/check/derived.js` finds manual implementations of what should be derived from one description, and `just check` fails on any hit outside `scripts/check/derived.allow` (`--list` shows the hits); `scripts/check/docs.js` compares the marked tables of docs/05 with the compiled setting.
- Engine: `packages/core/src/Core/Engine/Commit.purs` (`resolveRound`). Round runner: `packages/effects/src/Effects/Round.purs`. Effect rows: `packages/effects/src/Effects/Signatures.purs`. Handlers: `packages/handlers/src`; the in-memory test handler is in `packages/handlers/test`. Operations are declared in `packages/effects/src/Effects/Signatures.purs` (`OBSERVE` and `STORE` for the Run Store; the lifecycle constructors are in `Effects/Store.purs`), and the `API` row is in `Effects/Api.purs`. Handlers interpret them over typed backends: `Handlers.Llm.*` (`Backend`, `Call`, `Joint`, `Narrate`, `Speak`, `Wire`, `Prompt`; backend `call` is raw text), `Handlers.Store` and `Handlers.SqlStore` (OBSERVE, STORE, and RECORD; the SQL schema, statements and decoding, over `Handlers.Sql`), `Handlers.Play` (PLAY), `Handlers.Session` (LIVE), `Handlers.Cache` (CACHE), `Handlers.System` (CLOCK, ENTROPY, PROCESS, FILES, CONSOLE), `Handlers.Api.Route` and `Handlers.Api.Client` (the API's routes and browser-safe client), and `Handlers.Recording` (the capstone's exchange recording). `packages/adapters/src/Adapters/` holds only raw primitives: `Adapters.Pi` (one `pi` process), `Adapters.SQLite` (one statement, query or transaction; the `SqlDevice`), `Adapters.Clock` (wall clock), `Adapters.Random` (`drawOsSeed`), `Adapters.Process` (identity, liveness), `Adapters.Digest` (`sha256`, the evidence digest), `Adapters.Console` (the terminal: `readLine`, `writeLine`, `closeInput`), `Adapters.Exit` (print a failure and exit 1), and `Adapters.Filesystem` (`fileOps`, `fileDevice`, `lineSink`). Test stubs are in `packages/handlers/test/Test/Handlers/Stub.purs`.
- Invariants: `packages/core/src/Core/Invariants.purs` (`check`, which checks INV-01, 02, 03, 04, 05, 06, 08, 09, and 10 for one round). `packages/checks/src/Checks/Run.purs` (`checkRun`) applies it across a run; the test suites and testkit use it, and the CLI and `Handlers.Store` call `checkAfter` (`check` against the world the round was played from) per round. The engine does not call it, so a failing round is reported, not blocked.
- Numerics: `packages/core/src/Core/Num.purs` (`Q`) is the only implementation of economic quantities. Use `Q.parse`, `Q.format`, `Q.divMaybe`, `Q.divFloor`, and `Q.proRata`. Display goes through `Q.displayQuantity` (quantities and prices) or `Q.displayRatio`. Never use `Number`, `parseFloat`, or `toFixed` for economic values.
- Randomness: draws come from `Core.Draw.draw`. The player seed is resolved once by `Effects.System.resolveSeed`, over the `ENTROPY` row.
- Settings: authored in the DSL under `packages/schema/src/Schema/Build/` and `packages/settings/src/Settings/`. `packages/settings` does no IO. `adapters/Filesystem` is the file device, `Settings.Tree` defines the setting file tree over it, and `apps/cli`'s `Cli.Compile` writes the compiled files. Compiled JSON is gitignored and never hand-edited. `packages/settings/capstone/transcript.json` is generated by `just _settings` (`apps/cli`'s `Cli.Transcript`), gitignored like the rest of the compiled files, and never hand-edited; it records the capstone's joint-pass exchanges with no model call. Fixture expected values are literals from the worked examples in `docs/05_test_instance.md`, never computed.
- Dashboard: `apps/dashboard` is PureScript `react-basic-hooks` over shadcn/ui components generated into `apps/dashboard/ui/` (`just _ui`; not committed). View models are in `packages/views/src/Views/`. Keep business logic out of the frontend.
- `work/` is gitignored scratch space for implementation notes. It is not part of the spec.

## Spec map (`docs/`)

Read them in order. Each layer builds on the one before it.

1. `01_overview.md`: concept and design principles
2. `02_type_system.md`: glossary (§2.5), types and rules, the canonical round sequence (§8.1), effects (§8.4), invariants `INV-xx` (§9), numerics (§9.1)
3. `03_gameboard.md`: how a setting is configured
4. `04_instantiated_game.md`: initialization and live round execution
5. `05_test_instance.md`: *Meridian*, the reference setting, and its golden fixtures
6. `06_simulator.md`: simulator, run store, dashboard, mandatory tests
7. `07_architecture.md`: package layout, dependency direction, key interfaces

Spec conventions:
- Cite invariants as `INV-xx` and Type System sections by number. IDs 18, 20, and 32–35 are retired.
- Define each term once, in the doc of the layer that owns it: type-system and engine terms in the Type System glossary (`02` §2.5), scenario-planning terms in `01` §1, authoring terms in `03`, assembly and runtime terms in `04`, store and analysis terms in `06`, and implementation terms in `07`. Each of `03`, `04`, `06` and `07` has a `Terms` table, and the scenario-planning terms are in `01` §1. The other docs reuse them.
- Never add open-question sections. Record a decision where it belongs.

## Architecture (big picture)

- **Two parts.** The pure engine, `resolveRound`, is the only code that changes the economy. The round runner runs Propose and then Sign for every Policy and hands the signed Proposals to the engine. The engine checks authority and signatures and balances the books. It never authors or rewrites an Intent.
- **Purity.** Every impure act is an operation of an effect row, interpreted by a handler over an adapter device (docs/07 §1). Only composition roots (`Server`, `Server.Stack`, `Main`, `Compile`, `Transcript`, `Cli.Stack`, test suites) import adapters. Handlers contain no game logic.
- **Capacity is a use of a Resource** (glossary, `docs/02_type_system.md` §2.5). `Core` has no such concept, and `scripts/check/derived.js` bans the word in `packages/core/src`; the word belongs to the DSL and the UI.
- **Representation lags one round.** The engine stores an adjustment factor in round r, and the Aggregate's Policy applies it in round r+1.
- **Replay.** Replay under the Replay handler must reproduce a run exactly.
- **Layers.** The dependency chain is in `docs/07_architecture.md` §3. The module list is `find packages apps -name '*.purs'`.

## Tests

The setting fixtures are golden tests against `resolveRound` (`packages/settings/test/Test/Settings/Golden.purs`, which runs every fixture in the DSL's list). The mandatory tests are in `docs/06_simulator.md` §14 and `packages/handlers/test/Test/Handlers/Simulate.purs`.
