# AGENTS.md

Guidance for AI coding agents working in this repository.

## Status

The system is implemented. `docs/` is the specification and the source of truth. Read the relevant spec before changing behaviour, and record design changes in the specs.

## Commands

- Toolchain: the Nix flake (`use flake` via direnv) provides node 22 and just. The dev shell runs `npm ci` into `node_modules/.bin`, which pins `purescript` 0.15.16, `spago` 1.0.5, `purs-tidy` 0.11.1 and `shadcn` 4.23.0.
- `just build`: compile everything. `just test`: every package's suite. `just test-pkg core`: one package.
- Single test: `spago test -p sp-core -- --example "<test name>"`.
- `just fmt`: purs-tidy. `just check`: fmt-check, build, deps, layout, impurity, derived, test, docs. `just all`: `check` plus the dashboard Playwright smoke test (`just dashboard-test`, which needs `cd apps/dashboard && npx playwright install` once).
- `just sim pacific --preset Restrict --rounds 6`: play a game from the CLI; `--preset` defaults to the composition's own default.
- `just session pacific --humans US`: live facilitator console. Non-human seats are AI agents through the `pi` binary and make billed model calls. The model defaults to `openrouter/deepseek/deepseek-v4-flash-latest` (`Adapters.Pi`); `--model` overrides it.
- `just live-interpret`: one joint pass against the real model, which makes billed calls. It needs `ALLOW_BILLED_LIVE=1`, and no suite runs it.
- `just openapi pacific [--server URL] [--out DIR]`: write `transcription.json` and `narrative.json` for one Premise to `openapi/<premise>`, each reference of a kind the Premise names listed as an `enum` of those names.
- `just serve --db runs.db`: build the dashboard and serve it with the JSON API on :8787. The landing page is at `/`, the dashboard at `/admin` behind `ADMIN_PASSWORD` from `.env` (`.env.example`; `--env` names another file), and `/api` is private unless a route says Open (`docs/07_simulator.md` §8.3). Ports are in `apps/dashboard/ports.mjs`.
- Recipes starting with `_` are internal steps. They are hidden from `just --list` but still runnable. `just _premises` compiles the Premise DSL to compositions and Premises (`Compile`), after clearing the compiled directories, and writes the external services' OpenAPI documents to `openapi/` (`apps/server/src/OpenApi.purs`, gitignored); `test`, `test-pkg`, `sim`, `session`, `serve`, `dashboard-test`, `live-interpret`, and `_docs` run it first.

## Where things live

- Package layout, dependency rules, and key interfaces: `docs/08_architecture.md` (§3 and §4). `scripts/check/deps.js` enforces the dependency table and the import rules; `scripts/check/layout.js` enforces the concept rules that imports cannot express; `scripts/check/impurity.js` enforces the three layers for impure work (docs/08 §1); `scripts/check/derived.js` finds manual implementations of what should be derived from one description, and `just check` fails on any hit outside `scripts/check/derived.allow` (`--list` shows the hits); `scripts/check/docs.js` compares the marked tables of docs/06 with the compiled composition.
- Kernel: `packages/core/src/Core/Kernel/` (`Types`, `Post`, the only writer of the ledger, `Resolve`, `Consume`, `Law`, `Ration`, `Clear`, `Route`). Authored vocabulary and the settlement sequence: `packages/laws` (`Laws.Types`, `Laws.World`, `Laws.Engine.*`), which depends only on core. Engine: `packages/laws/src/Laws/Engine/Commit.purs` (`resolveRound`). Round runner: `packages/effects/src/Effects/Round.purs`. Effect rows: `packages/effects/src/Effects/Signatures.purs`. Handlers: `packages/handlers/src`; the in-memory test handler is in `packages/handlers/test`. Operations are declared in `packages/effects/src/Effects/Signatures.purs` (`OBSERVE` and `STORE` for the Run Store; the lifecycle constructors are in `Effects/Store.purs`), and the `API` row is in `Effects/Api.purs`. Handlers interpret them over typed backends: `Handlers.Llm.*` (`Backend`, `Call`, `Joint`, `Speak`, `Wire`, `Prompt`; backend `call` is raw text), `Handlers.Store` and `Handlers.SqlStore` (OBSERVE, STORE, and RECORD; the SQL schema, statements and decoding, over `Handlers.Sql`), `Handlers.Play` (PLAY), `Handlers.Session` (LIVE), `Handlers.Sessions` (SESSIONS, the live sessions a server hosts), `Handlers.Cache` (CACHE), `Handlers.System` (CLOCK, ENTROPY, PROCESS, FILES, CONSOLE), `Handlers.Api.Route` and `Handlers.Api.Client` (the API's routes and browser-safe client), and `Handlers.Recording` (the recording of model turns). `packages/adapters/src/Adapters/` holds only raw primitives: `Adapters.Pi` (one `pi` process), `Adapters.SQLite` (one statement, query or transaction; the `SqlDevice`), `Adapters.Clock` (wall clock), `Adapters.Random` (`drawOsSeed`), `Adapters.Process` (identity, liveness), `Adapters.Digest` (`sha256`, the evidence digest), `Adapters.Console` (the terminal: `readLine`, `writeLine`, `closeInput`), `Adapters.Exit` (print a failure and exit 1), and `Adapters.Filesystem` (`fileOps`, `fileDevice`, `lineSink`). Test stubs are in `packages/handlers/test/Test/Handlers/Stub.purs`.
- Invariants: `packages/laws/src/Laws/Invariants.purs` (`check`, which checks INV-01, 02, 03, 04, 05, 06, 07, 08, 09, 10, and 37 for one round; `checkAfter` adds INV-38, external consistency, from `Laws.External`). `packages/checks/src/Checks/Run.purs` (`checkRun`) applies it across a run; the test suites and testkit use it, and the CLI and `Handlers.Store` call `checkAfter` (`check` against the world the round was played from) per round. The engine does not call it, so a failing round is reported, not blocked.
- Numerics: `packages/core/src/Core/Num.purs` (`Q`) is the only implementation of economic quantities. Use `Q.parse`, `Q.format`, `Q.divMaybe`, `Q.divFloor`, and `Q.proRata`. Display goes through `Q.displayQuantity` (quantities and prices) or `Q.displayRatio`. Never use `Number`, `parseFloat`, or `toFixed` for economic values.
- Randomness: draws come from `Core.Draw.draw`. The player seed is resolved once by `Effects.System.resolveSeed`, over the `ENTROPY` row.
- Premises and PremiseCompositions: authored in the DSL under `packages/schema/src/Schema/Build/` and `packages/premises/src/Premises/`; `Premises.Compose` merges an ordered list of Premises (conflicts refused, naming both Premises) into what `Schema.Composition`'s `PremiseComposition` decodes. A composition compiles to `compositions/<id>/<version>/` and one `premises/<id>/<version>.json` per Premise. `packages/premises` does no IO. `adapters/Filesystem` is the file device, `Premises.Tree` defines the compiled file tree over it, and `apps/cli`'s `Cli.Compile` writes the compiled files. Compiled JSON is gitignored and never hand-edited. Fixture expected values are literals from the worked examples in `docs/05_pacific.md`, never computed. The compiled compositions are listed in `Premises.Catalog`: `pacific` (`Premises.Pacific`). A new composition is a module beside it, added to `Premises.Catalog` (and its fixtures to `Premises.Fixtures`); the server and the dashboard offer every composition the catalog lists.
- Dashboard: `apps/dashboard` is PureScript `react-basic-hooks` over shadcn/ui components generated into `apps/dashboard/ui/` (`just _ui`; not committed). View models are in `packages/views/src/Views/`. Keep business logic out of the frontend. The dashboard is served at `/admin`; each Actor's page is `/balancesheet/:run/:token`. Speech transcription and the newspaper are another repository's work: this one exposes their API (`docs/07_simulator.md` §8.2).
- `work/` is gitignored scratch space for implementation notes. It is not part of the spec.

## Spec map (`docs/`)

Read them in order. Each layer builds on the one before it.

1. `01_overview.md`: concept, the anchor Terms, and design principles
2. `02_kernel.md`: what any World can be made of (the kernel and types), the canonical round sequence (§8.1), effects (§8.4), invariants `INV-xx` (§9), numerics (§9.1)
3. `03_premise.md`: how Premises are authored and composed, and the vocabulary that compiles to the kernel
4. `04_run.md`: how a PremiseComposition is assembled and realized as a Run, Round by Round
5. `05_pacific.md`: *Pacific*, the reference PremiseComposition (two countries, each a state and a firm, with two currencies, industrial competence, export controls, financial repression and dollar reserves, in a circular flow), its feedback loops, experiments and golden scenarios
6. `07_simulator.md`: simulator, run store, dashboard, mandatory tests
7. `08_architecture.md`: package layout, dependency direction, key interfaces

Spec conventions:
- Cite invariants as `INV-xx` and `02` sections by number. IDs 12, 15, 16, 18, 20, 22, and 32–35 are retired.
- The 17 anchors are defined once, in the Terms section of `01`, and nowhere else. Name a new concept from an anchor with a qualifier (candidate Transaction, settled Flow, round-end World), or explain it in the section that uses it. Only `08` has a further table, of implementation terms for people working on the code. Do not bold a term as a definition in prose.
- Never add open-question sections. Record a decision where it belongs.

## Architecture (big picture)

- **Two parts.** The pure engine, `resolveRound` (the settlement sequence in `packages/laws`, over the kernel in `core`), is the only code that changes the economy; every settled Flow posts through `Core.Kernel.Post`. The round runner runs Propose and then Sign for every Strategy and hands the signed candidate Transactions to the engine. The engine checks authority and signatures and balances the books. It never authors or rewrites an Intent.
- **Purity.** Every impure act is an operation of an effect row, interpreted by a handler over an adapter device (docs/08 §1). Only composition roots (`Server`, `Server.Stack`, `OpenApi`, `Main`, `Compile`, `Transcript`, `Cli.Stack`, test suites) import adapters. Handlers contain no game logic.
- **Capacity is a use of a Resource** (`docs/03_premise.md` §5.1). `Core` has no such concept; the word belongs to the DSL and the UI. `scripts/check/layout.js` keeps the authored vocabulary (trade, transform, joint, procedure, levy, depreciation, intent, proposal) out of `packages/core/src`, and only `Core.Kernel.Post` writes the ledger.
- **Representation lags one round.** The engine stores an adjustment factor in round r, and the Aggregate's Strategy applies it in round r+1.
- **Replay.** Replay under the Replay handler must reproduce a run exactly.
- **Layers.** The dependency chain is in `docs/08_architecture.md` §3. The module list is `find packages apps -name '*.purs'`.

## Tests

The premise fixtures are golden tests against `resolveRound` (`packages/premises/test/Test/Premises/Golden.purs`, which runs every fixture in the DSL's list). The mandatory tests are in `docs/07_simulator.md` §14 and `packages/handlers/test/Test/Handlers/Simulate.purs`. Pacific's golden scenarios play whole Runs and are in `packages/handlers/test/Test/Handlers/Pacific.purs`; its circuits, accounting, feedback loops and experiments in `packages/handlers/test/Test/Handlers/Loops.purs`.
