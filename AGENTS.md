# AGENTS.md

This file provides guidance to AI coding agents working with code in this repository.

## Status

The system is implemented: engine, scenario, Policies, harness, Run Store, read API, dashboard, and live play. `docs/` holds the specification and is the source of truth; read the relevant spec before changing behaviour, and record design changes in the specs.

## Environment and commands

- Toolchain: Nix flake (`use flake` via direnv) provides node 22 and just. `purescript` 0.15.16, `spago` 1.0.5, `purs-tidy` 0.11.1 and `shadcn` 4.23.0 are pinned exactly in `package.json` and installed with `npm ci` from `package-lock.json` into `node_modules/.bin` by the dev shell.
- The repo is a spago workspace: `packages/{core,board,policy,scenarios,effects,handlers,adapters,harness}` and `apps/{cli,server,dashboard}`. Each package is named `sp-<dir>`.
- `just build`: compile everything. `just test`: every package's suite. `just test-pkg core`: one package.
- Single test: `spago test -p sp-core -- --example "<test name>"` (spec-node filter)
- `just fmt`: purs-tidy.
- `just check`: fmt-check, deps, build, test. `just all`: `check` plus the dashboard's Playwright smoke test (`just dashboard-test`). Recipes starting with `_` are internal steps, hidden from `just --list` but still runnable.
- `just sim fractured-world --preset Conservative --rounds 5`: play a game from the CLI. `just session fractured-world --humans US --model anthropic/claude-opus-5-5`: live facilitator console (typed input; other seats are AI agents via the `pi` binary, which makes billed model calls).
- `just serve --db runs.db`: build the dashboard and serve it with the JSON API on :8787. Dashboard ports (API 8787, e2e 8799) are set in `apps/dashboard/ports.mjs`; `just dashboard-test` needs `cd apps/dashboard && npx playwright install` once.
- Scenarios and presets are authored in the PureScript DSL (`packages/scenarios/src/DSL/` and `src/Scenarios/`); `packages/scenarios` does no IO, so reading the directory is `adapters/Scenarios` and writing the compiled files is `apps/cli`'s `Compile`. So are the golden fixtures (`src/Scenarios/Fixtures.purs`): they share the scenario's world pieces, but every expected value stays a literal from the worked examples in `docs/05_test_instance.md`, never computed. `just _scenarios` compiles all of them to `fractured-world/v0/*.json`, `presets.json` and `fixtures/*.json`, which are gitignored build outputs; `test`, `sim`, `session`, `serve` and `all` compile them first. Never hand-edit those JSON files.
- The dashboard (`apps/dashboard`) is PureScript `react-basic-hooks` over vendored shadcn/ui in `apps/dashboard/ui/` (FFI bindings in `Dashboard.UI`, `Dashboard.Api` and `Dashboard.Theme`). View models are computed in `packages/harness/src/Harness/Views/` (one module per view group, shared shapes in `Views/Common`); keep business logic out of the frontend.

`work/` is gitignored scratch space for implementation-process docs such as `work/1.md` (the work plan) and `work/deviations.md`. It is not part of the spec.

## Spec map (`docs/`)

Read them in order. Each layer references the one before it, and none restates it.

1. `01_overview.md`: concept and principles
2. `02_type_system.md`: the authoritative source for the glossary (§2.5), Intents and Proposals (§3), Policies (§4), markets and facilities (§5), Representation (§7), the canonical round sequence (§8.1), effects and handlers (§8.4), the master invariant table `INV-xx` (§9), and numerics and randomness (§9.1)
3. `03_gameboard.md`: how a scenario is configured
4. `04_instantiated_game.md`: initialization and live round execution
5. `05_test_instance.md`: *The Fractured World*, the reference scenario, with fixtures A–G (exact expected balances)
6. `06_simulator.md`: simulation harness, run store, dashboard, mandatory tests
7. `07_architecture.md`: package layout, dependency direction, and the key interfaces

Spec conventions:
- Cite invariants by `INV-xx` and Type System sections by number. Those numbers are stable, and IDs 18, 20 and 32–35 are retired.
- Define terms only in the Type System glossary; the other docs reuse them.
- Never add open-question sections. Record a decision in the place where it belongs.

## Architecture (big picture)

- **Two parts.** The pure **engine** (`resolveRound :: WorldState -> Array SignedProposal -> Either EngineError RoundResult`) is the only code that changes the economy. A **round runner** in the Policy layer runs Propose and then Sign for every Policy (seats, NPCs, Aggregate baselines) and hands the signed Proposals to the engine.
- **The engine is an accountant.** It checks authority and signatures and balances the books; negotiation and agreement happen out of band. Intents are its only input, and it never authors or rewrites one.
- **Proposals and signatures.** Each Intent is carried by a Proposal. A deal needs every named signer and bundles only Transfers and Transforms, settling atomically. A collective procedure is a weighted-signature rule on an Aggregate's own Proposal: it executes in the same round if the signed weight exceeds the threshold. A Proposal whose signature rule isn't met lapses; nothing ever blocks a round.
- **Representation lags one round.** The engine stores an adjustment factor in round r, and the Aggregate's Policy applies it in round r+1.
- **One bundled round update.** Clearing is uniform and there is no ordering within a round. Actions are funded from opening holdings through escrow, and receipts are credited at commit.
- **Purity.** All impurity goes through algebraic effects (`Run` row: `SEAT_INPUT`, `RECORD`, plus `INTERPRET`, `NARRATE`, `CLOCK` for live play). Handlers (Live, Sim, Replay, Test) choose the context and contain no game logic.
- **Randomness.** Draws are keyed and pure (`draw :: Seed -> DrawKey -> UnitInterval`); no effect is needed. Each game instance has exactly one game seed for in-world draws. The simulator adds a separate player seed for seat-policy randomness, and the two streams never share draws.
- **Numerics.** All economic quantities use fixed-point arithmetic, 4 decimal places, rounded half-to-even, with no floats. The rule set is in `docs/02_type_system.md` §9.1, and `packages/core/src/Core/Num.purs` (`Q`) is the only implementation: use `Q.parse`, `Q.format`, `Q.divMaybe`/`Q.divFloor` and `Q.proRata`, never `Number`, `parseFloat` or `toFixed`.
- **Planned layout** (`07_architecture.md`): a spago workspace in which dependencies point one way only: `core ← board, policy ← effects ← handlers ← adapters, harness ← apps`. Packages keep modules in namespace folders; no facade modules. `scenarios/` holds data (JSON), and its pure decoding and DSL compilation; it has no IO. `adapters/Scenarios` reads the scenario directory and `apps/cli` (module `Compile`) writes the compiled files. `core` imports no effects or IO; its JSON codecs sit beside each type. Each invariant is a single pure predicate in `core/Invariants`, shared by the tests, the harness and live play.
- **Tests.** Fixtures A–G become golden tests against `resolveRound`. Replay under the Replay handler must reproduce a run exactly.
