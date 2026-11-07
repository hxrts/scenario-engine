# Scenario Planning Game — Technical Architecture

**Version:** 0.3  
**Scope:** Package layout, dependency direction, and the key interfaces between packages  
**Implementation language:** PureScript

---

## 1. Principles

- **Pure core, effects at the edge.** The engine is a pure function ([INV-36](02_type_system.md#inv-36)). Impure work is an algebraic effect (Type System [§8.4](02_type_system.md#84-effects-and-handlers)) interpreted by a handler chosen at the entry point.
- **One-way dependencies.** A package depends only on the packages in its row of the table in [§3](#3-codebase-layout). Packages at the same level never import each other.
- **Narrow seams.** Packages meet at the small interfaces in [§4](#4-key-interfaces). Code behind an interface can change without touching its callers.
- **Scenarios are data.** Gameboards, instantiations, fixtures, and presets are declarative data, authored in a typed PureScript DSL that compiles to JSON. The engine reads only the compiled files. Fixture expected values are literals, never computed.
- **Small.** One concept per module. No frameworks inside the core.

## 2. Runtime Shape

```text
   entry point (cli run | cli session | server | tests)
                  │ chooses handlers
                  ▼
   ┌─────────────── round runner (effectful) ───────────────┐
   │  prepare/observe ──► Propose ──► Sign ──► resolveRound │
   │      (pure)        SEAT_INPUT  SEAT_INPUT    (pure)    │
   └───────────────────────────────┬────────────────────────┘
                                   │ RECORD
                                   ▼
                     Run Store ──► server (read API) ──► dashboard
```

Each round is one call to `runRound`. Seats propose and sign through `SEAT_INPUT`, and the pure engine resolves the signed Proposals. The engine checks authority and signatures and keeps the books balanced. It never authors or rewrites an Intent. Handlers choose where seat inputs come from and where records go. Live play also uses `INTERPRET`, `NARRATE`, and `CLOCK` ([§4](#4-key-interfaces)). The round rules are in [02](02_type_system.md) and [04](04_instantiated_game.md). The Run Store, read API, and dashboard are in [06](06_simulator.md).

## 3. Codebase Layout

The project is a spago workspace. Each package is named `sp-<dir>` and compiles on its own. `just check` runs `scripts/check-deps`, which fails if a package depends on a package outside this table, or if a pure package (`core`, `board`, `policy`) depends on an effect, `aff`, `run`, or `node-*` library. Test sections are not checked, so test suites may import more (for example, the handler and harness tests use `sp-scenarios` and `sp-adapters`).

| Package | May depend on |
|---|---|
| `core` | nothing in the workspace |
| `board`, `policy` | `core` |
| `scenarios` | `core`, `board`, `policy` |
| `effects` | `core`, `board`, `policy` |
| `handlers` | `core`, `board`, `policy`, `effects` |
| `adapters`, `harness` | `core`, `board`, `policy`, `effects`, `handlers` |
| `apps/*` | anything |

`scenarios` is kept free of IO by convention, not by the checker.

Module map, for finding code:

```text
packages/
  core/        Core.*: pure types and engine
    Num            fixed-point quantities Q and display kinds
    Types, World   Entity, Resource, Intent, Proposal, Market, …; WorldState, RoundRecord, RoundResult, Manifest
    Draw           keyed PRNG
    Market, Query, Represent, Invariants
    Engine.*       Prepare, Screen, Fund, Route, Settle, Circulate, Commit (resolveRound)
  board/       Board.*: Gameboard schema, Board.Decode.*, validation, instantiation, Tags, Fixture
  policy/      Policy.Program (decision language), Policy.Interpret (propose and sign)
  scenarios/   DSL.*, Scenarios.* (Meridian, Presets, Fixtures, Capstone, Compile, Load)
  effects/     Effects.Signatures (effect rows), Effects.Environment, Effects.Round (runRound, runGame)
  handlers/    Handlers.Seats, Sim, Replay, Live; the in-memory test handler is in test/
  adapters/    Adapters.Llm (one `pi` process per call), Store (SQLite: record writer and RunStoreRead reader),
               Scenarios (reads SCENARIOS_DIR), Seed (the only place a player seed is drawn from the OS)
  harness/     Harness.Simulate, Analysis, Overrides, Records, Options, Capstone; Harness.Views.* (view models)
apps/
  cli/         Main (run, session), Session, Compile
  server/      Server, Server.Routes, Read, Launch, Derive, Errors; Http
  dashboard/   Dashboard.* (react-basic-hooks); FFI in UI.js, Api.js, Theme.js
test/          packages/<pkg>/test (Test.<Pkg>.Main); apps/{cli,server,dashboard}/test; apps/dashboard/e2e (Playwright)
```

Compiled scenario files (`packages/scenarios/meridian/`, `presets.json`, `fixtures/`, and `capstone/run.json`) are gitignored build output. `capstone/transcript.json` is a committed recording. The shadcn/ui components in `apps/dashboard/ui` are generated, not committed.

## 4. Key Interfaces

These are the seams. Every other piece is an internal detail of a package. Signatures are as the code declares them.

**Engine** (`core`, `Core.Engine.Commit`). Pure, and the only operation that changes the economy:

```purescript
resolveRound :: WorldState -> Array SignedProposal -> Either EngineError RoundResult
-- RoundResult = { world :: WorldState, record :: RoundRecord }
```

**Instantiation** (`board`). Pure:

```purescript
validateGameboard :: Gameboard -> InstantiationConfig -> Either (Array ValidationError) Gameboard
instantiate       :: Gameboard -> InstantiationConfig -> Either (Array ValidationError) InitialWorld
```

**Policy** (`policy`). A Program is pure and reduces to two functions over an environment. A seat, human or AI agent, satisfies the same shape through `SEAT_INPUT`:

```purescript
propose :: Program -> Env -> Array Draft
sign    :: Program -> Env -> Array ProposalId     -- chooses among view.toSign
-- Draft = { intents :: Array Intent, signers :: Array EntityId, procedure :: Maybe CollectiveProcedureId }
-- Env   = { view :: View, params :: Object Q, home :: Maybe MarketId, random :: Int -> Q }
```

`View` (Type System [§4](02_type_system.md#4-policies)) is everything a Policy sees. Its `toSign` field lists the Proposals awaiting that Policy's signature.

**Effects** (`effects`). The only impure vocabulary in the system:

```purescript
type SEAT_INPUT r = (seatInput :: SeatInputF | r)   -- Propose EntityId View | Sign EntityId View
type RECORD r     = (record :: RecordF | r)         -- Record RoundResult
type INTERPRET r  = (interpret :: InterpretF | r)   -- live play: transcript → Interpretation
type NARRATE r    = (narrate :: NarrateF | r)       -- live play and reports
type CLOCK r      = (clock :: ClockF | r)           -- live play: propose and sign deadlines

runRound :: InWorld -> WorldState -> Run (SEAT_INPUT + RECORD + r) (Either EngineError RoundResult)
```

Randomness needs no effect. Every draw is the pure `Core.Draw.draw` on the game seed or the player seed, and the handler supplies the player seed.

**Simulation** (`harness`, `Harness.Simulate`). The harness runs a spec through `runRound` with a chosen handler. It reads no entropy: the seed is resolved before the call (`Adapters.Seed`).

```purescript
simulate     :: Context -> SimulationSpec -> Either String Run
simulateWith :: Context -> SimulationSpec -> (Manifest -> Effect (RoundResult -> Effect Unit)) -> Effect (Either String (Either EngineError WorldState))
-- Run = { manifest :: Manifest, playerSeed :: Seed, records :: Array RoundResult, final :: Either EngineError WorldState }
```

`simulateWith` takes the per-round persister, so the server writes each round as it completes. Objectives are a pure view over the records and never touch the engine (Gameboard [§10.5](03_gameboard.md#105-evaluation)):

```purescript
evaluateObjectives :: Gameboard -> Array RoundResult -> Array ObjectiveResult
```

**Run Store** (`adapters/Store`). Writes go through `createRun`, the per-round persister (a `RECORD` target), `finish`, `failRun`, `failStaleRuns`, `setArchived`, `setSpec`, and `deleteRun`. Round records and snapshots are insert-only. Reads go through `reader :: Db -> RunStoreRead`:

```purescript
type RunStoreRead =
  { runs      :: Effect (Array RunInfo)
  , summaries :: Effect (Array RunSummary)  -- run list: no records or snapshots
  , manifest  :: String -> Effect (Maybe { spec :: String, manifest :: String })
  , rounds    :: String -> Effect (Array StoredRound)
  , metric    :: String -> String -> String -> Effect (Array { round :: Int, value :: String }) }  -- run, metric, version
```

The schema, migrations, and endpoints are in [06 §8](06_simulator.md#8-persistence).

**Views** (`harness/Views/*`). Every dashboard element is a pure projection of a stored run to one of these generic shapes, computed server-side. The dashboard renders them and computes nothing:

```purescript
type Table     = { title :: String, columns :: Array String, rows :: Array (Array String) }
type Chart     = { title :: String, unit :: String, kind :: Kind, series :: Array { name :: String, points :: Array { round :: Int, value :: String } } }
type Bars      = { title :: String, unit :: String, kind :: Kind, categories :: Array String, series :: Array { name :: String, values :: Array String } }
type Statistic = { name :: String, question :: String, definition :: String, formula :: String }
```

`Kind` is the display kind ([06 §9](06_simulator.md#9-pages)). Which views exist, and their routes, are in [06](06_simulator.md).

**Data formats.** Two JSON formats cover overlapping types and are not interchangeable:

- The **snapshot format** is the engine's own state as core's codecs write it: `WorldState`, round records, and manifests. Runs store it, and replay reads it back.
- The **scenario format** is the authored configuration that `board` and `policy` decode: Gameboard sections, `instantiation.json`, seat-policy presets, and fixtures. Its tags are `kind:argument` strings (constants in `Board.Tags`), and its tagged unions are objects with exactly one key.

Each format's codec lives in the package that owns its type.

## 5. Coupling Rules

- `core` imports no effect library and no IO. Its JSON codecs are the `EncodeJson`/`DecodeJson` instances declared beside each type, because PureScript forbids orphan instances. The engine never calls them.
- Policies never touch World State. They receive a `View` and return Proposals and signatures.
- Handlers translate effects only. Economic rules live only in `core`.
- Views are computed in `harness/Views/*`. The dashboard holds no business logic.
- Replacing an adapter (the SQLite store, the LLM provider) changes nothing in `core`, `board`, `policy`, `effects`, or `handlers`.
- Scenarios change by editing data. Needing new engine code for a scenario is a design finding to resolve in `core` ([05 §1](05_test_instance.md#1-purpose)).
- Per-round assertions are pure predicates in `core/Invariants` (`check`), shared by the tests, the harness, and live play. Which assertions exist is in [06 §7.1](06_simulator.md#71-required-assertions).

## 6. Testing

Each package has its own suite, and the apps have suites for the CLI, server, and dashboard. The dashboard's Playwright specs run against a server on the E2E port. The required tests, including golden fixtures, replay, and persistence, are in [06 §14](06_simulator.md#14-mandatory-tests). Interpretation tests are separate from economic tests ([06 §7.4](06_simulator.md#74-ai-interpretation-tests)). Commands to run them are in `AGENTS.md`.
