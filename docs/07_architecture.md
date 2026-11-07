# Technical Architecture

**Version:** 0.3  
**Scope:** Package layout, dependency direction, and the key interfaces between packages  
**Implementation language:** PureScript

---

## 1. Principles

- **Pure core, effects at the edge.** The engine is a pure function ([INV-36](02_type_system.md#inv-36)). Impure work is an algebraic effect (Type System [§8.4](02_type_system.md#84-effects-and-handlers)) interpreted by a handler chosen at the entry point.
- **One-way dependencies.** A package depends only on the packages in its row of the table in [§3](#3-codebase-layout). Packages at the same level never import each other.
- **Narrow seams.** Packages meet at the small interfaces in [§4](#4-key-interfaces). Code behind an interface can change without touching its callers.
- **Three layers for impure work.** Each concern has one home:
  - **Adapters expose devices.** An adapter wraps one device (the filesystem, SQLite, a process, the clock, the OS seed) as raw primitives: text in, text out, or one statement. It knows nothing of games, runs, prompts or scenarios.
  - **Effects define the program's interface to a device.** A module in `effects` declares the operations a program may request, with their typed requests and replies. It contains no implementation.
  - **Handlers hold the program logic.** A handler interprets an effect's operations over a device primitive. Prompt building, reply decoding, fallbacks, run lifecycle, and recording belong here. Handlers never import adapters; the composition root wires the two.
  - A new device is a new adapter; a new program need is a new operation and its handler. Domain rules are not in any of the three; they stay in `core`, `schema`, `board`, `policy` and `premises`.
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

Each round is one call to `runRound`. Seats propose and sign through `SEAT_INPUT`, and the pure engine resolves the signed Proposals. The engine checks authority and signatures and keeps the books balanced. It never authors or rewrites an Intent. Handlers choose where seat inputs come from and where records go. Every round is also narrated (`NARRATE`), and live play adds `INTERPRET` ([§4](#4-key-interfaces)). The live handler waits out each human deadline window itself. The round rules are in [02](02_type_system.md) and [04](04_instantiated_game.md). The Run Store, read API, and dashboard are in [06](06_simulator.md).

## 3. Codebase Layout

The project is a spago workspace. Each package is named `sp-<dir>` and compiles on its own. `just check` runs `scripts/check-deps`, which fails if a package depends on a package outside this table, or if a pure package (`core`, `board`, `policy`) depends on an effect, `aff`, `run`, or `node-*` library. Test sections may import more, except that the simulator's tests may not import `analysis`, which depends on the simulator (a cycle).

| Package | May depend on |
|---|---|
| `core` | nothing in the workspace |
| `schema` | `core` |
| `board`, `policy` | `core`, `schema` |
| `game` | `core`, `schema`, `board`, `policy` |
| `premises` | `core`, `schema`, `board`, `policy` (until `game` takes assembly, design 4 Phase 4) |
| `effects` | `core`, `schema`, `board`, `policy` |
| `handlers` | `core`, `schema`, `board`, `policy`, `effects` |
| `adapters`, `simulator` | `core`, `schema`, `board`, `policy`, `effects`, `handlers` |
| `analysis` | `core`, `schema`, `board`, `simulator` |
| `apps/*` | anything |

`premises` is kept free of IO by convention, not by the checker. `schema` holds the premise format (its types, decoders, tags, and the authoring builders in `Schema.Build`); `board` holds realization only and imports no JSON library.

Module map, for finding code:

```text
packages/
  core/        Core.*: pure types and engine
    Num            fixed-point quantities Q and display kinds
    Types, World   Entity, Resource, Intent, Proposal, Market, …; WorldState, RoundRecord, RoundResult, Manifest
    Draw           keyed PRNG
    Market, Query (incl. `homeMarket`, capacity and facility-pool helpers, Intent selectors), Authority (`authorized`, `authorizedIn`, `executor`, `governs`), Proposal (numbering, signing eligibility, signature assembly; the joint pass's `Transcript`, `JointProposal`, and the reconciliation rule `jointProposals`), Represent, Invariants
    Strategy       the Strategy language's types (`Strategy`, `Clause`, `Cond`, `Expr`, `Template`, `SignRule`), decoded by `Policy.Strategy`
    Engine.*       Prepare, Screen, Fund, Route, Settle, Circulate, Commit (resolveRound)
  schema/      Schema.Gameboard (the Gameboard and Instantiation Configuration types), Schema.Tags (the premise format's tag grammar and the decision language's tags), Schema.Decode.* (World composes the section decoders; Common holds the shared readers; Strategy decodes Policies), Schema.Build.* (the authoring builders, JSON-emitting until their typed rewrite)
  checks/      Checks.Round (`observe`: a round's `Observed`; `check`, `checkRound`: an `Expect` against it, field by field), Checks.Run (`checkRun`: every round's invariants), Checks.Objectives (`evaluateObjectives`, `Outcome`); pure, over core and schema
  board/       Board.Gameboard.* (Validate, Instantiate, Reconcile), Board.Error (ValidationError, RefKind, Difference)
  policy/      Policy.Strategy (Strategy queries: the names a Strategy uses), Policy.InWorld (in-world Strategies, `acts`), Policy.Interpret (propose, sign, `envFor`)
  premises/    Premises.* (Meridian and its submodules, Names, Presets, Fixtures, Literal (a literal premise as a Game, its bindings from the rounds), Capstone, Compile, Load, Tree: the premise file tree, pure); a premise is authored, a scenario is one run
  effects/     Effects.Signatures (the operations and their request and reply records, STORE's read-model records included), Effects.Store (STORE constructors and
               the `finish` lifecycle program), Effects.Round (runRound, runGame): the algebra and the round program only
  handlers/    Handlers.Seats, Sim, Replay (the round's seat handlers); Handlers.Live (live play: the human prompts and
               deadline windows, the joint pass, narration, the record write); Handlers.Llm (the model handler: requests,
               the joint pass, fallbacks, evidence; `Llm.js` is the one sha256), Handlers.Llm.Wire (the prompt text, the payloads and
               reply decoding: the one place prompts live), Handlers.Recording (typed exchanges, the capstone recording), Handlers.Store (STORE and RECORD
               over a StoreBackend; `validRunId`, the run identity rule); test/Test/Handlers/Stub holds the stub backend and
               the recording replay
  adapters/    raw primitives only: Adapters.Pi (one `pi` process per call), Adapters.SQLite (SQLite statements and the JSON
               text of stored values; `sqliteBackend`), Adapters.Clock (wall clock, `delayMs`), Adapters.Random (the only OS
               seed), Adapters.Filesystem (file reads, directory listings, environment variables, `appendLine`; `fileOps`),
               Adapters.Console (the terminal: `readLine`, `writeLine`, `closeInput`; Console.js is the readline interface)
  simulator/   Simulator.Simulate (`startOf`, `presetSpec`, `seats`, simulation orchestration), Overrides, Recipe (a run's
               stored spec, and the derive rules), Records, Options, Capstone
  analysis/    Analysis.Agency (differences, counterfactuals, agency); Analysis.Views.* (view models, including `Views.Metrics`, the versioned metric definitions)
apps/
  cli/         Main (run, session), Session, Compile, Transcript (the capstone recording)
  server/      Server, Server.Routes, Read, Launch, Derive, Errors; Http
  dashboard/   Dashboard.* (react-basic-hooks); FFI in UI.js, Api.js, Theme.js
test/          packages/<pkg>/test (Test.<Pkg>.Main); apps/{cli,server,dashboard}/test; apps/dashboard/e2e (Playwright)
```

Compiled premise files (`packages/premises/meridian/`, `presets.json`, `fixtures/`, and `capstone/run.json`) are gitignored build output. `capstone/transcript.json` is generated by `just _scenarios` (`apps/cli`'s `Transcript`) and gitignored: the capstone's joint-pass recording, with no model call. The shadcn/ui components in `apps/dashboard/ui` are generated, not committed.

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

**Policy** (`policy`). A Strategy is pure and reduces to two functions over an environment. A seat, human or AI agent, satisfies the same shape through `SEAT_INPUT`:

```purescript
propose :: Strategy -> Env -> Array Draft
sign    :: Strategy -> Env -> Array ProposalId     -- chooses among view.toSign
-- Draft = { intents :: Array Intent, signers :: Array EntityId, procedure :: Maybe CollectiveProcedureId }
-- Env   = { view :: View, params :: Object Q, home :: Maybe MarketId, random :: Int -> Q }
```

`View` (Type System [§4](02_type_system.md#4-policies)) is everything a Policy sees. Its `toSign` field lists the Proposals awaiting that Policy's signature.

**Dashboard client** (`apps/dashboard`, `Dashboard.Client`). One typed function per request, and the only place the endpoint paths are written (with `Dashboard.Api`, the low-level requests and the load hook). The pages hold no paths:

```purescript
overview :: String -> Aff (Either String RunOverview)   -- GET /api/runs/:id/overview
runs     :: Boolean -> Aff (Either String RunList)      -- GET /api/runs, with archived runs when True
```

**Effects** (`effects`). The only impure vocabulary in the system:

```purescript
type SEAT_INPUT r = (seatInput :: SeatInputF | r)   -- Propose EntityId View | Sign EntityId View | Speak EntityId View
type RECORD r     = (record :: RecordF | r)         -- Record RoundResult (Array Transcript): the round and the transcripts its joint pass read
type INTERPRET r  = (interpret :: InterpretF | r)   -- Interpret RoundIndex (Array Transcript): the joint pass (live play)
type NARRATE r    = (narrate :: NarrateF | r)       -- Narrate RoundResult: the front page, requested after each round (live play answers with text)
type STORE r      = (store :: StoreF | r)           -- the Run Store: run writes and reads (§4 Run Store)

runRound :: InWorld -> WorldState -> Run (SEAT_INPUT + INTERPRET + RECORD + NARRATE + r) (Either EngineError RoundResult)
```

The request and reply types of each operation are declared beside it in `effects`: a joint pass answers `Either String (Array JointProposal)` (`Core.Proposal`), a narration answers `String`, and a seat's transcript is `Maybe String`. The operations are the whole model interface, so no port record duplicates them.

The live handler's devices are a line of input, one window's wait, a line of output, the model's calls, and the record write. `Handlers.Live.runLive` takes them as a record, and the composition root (`apps/cli`, `Session`) supplies `Adapters.Console`, `Adapters.Clock`, and the model backend. A human's propose and sign windows are the live handler's own clock, not an effect.

Randomness needs no effect. Every draw is the pure `Core.Draw.draw` on the game seed or the player seed, and the handler supplies the player seed.

**Simulation** (`simulator`, `Simulator.Simulate`). The simulator runs a spec through `runRound` with a chosen handler. It reads no entropy: the seed is resolved before the call (`Adapters.Random`).

```purescript
simulate     :: Context -> SimulationSpec -> Either SimError Run
simulateWith :: Context -> SimulationSpec -> (Manifest -> Effect (RoundResult -> Effect Unit)) -> Effect (Either SimError (Either EngineError WorldState))
-- Run = { manifest :: Manifest, playerSeed :: Seed, records :: Array RoundResult, final :: Either EngineError WorldState }
```

`simulateWith` takes the per-round persister, so the server writes each round as it completes. Objectives are a pure view over the records and never touch the engine (Gameboard [§10.5](03_gameboard.md#105-evaluation)):

```purescript
evaluateObjectives :: Gameboard -> Array RoundResult -> Array ObjectiveResult
```

**Handlers and backends.** A handler interprets operations over a typed backend, a record of raw functions that its adapter implements. The logic (prompts, decoding, fallbacks, the joint pass, the run lifecycle) is in the handler, so a backend carries only primitives. Prompt text is part of the model handler's contract and lives only in `Handlers.Llm.Wire`; adapters never hold prompts. Economic values such as metrics are computed in `analysis`, and the composition root passes them to the handler, which stores them: a model call that takes text and returns text, or one store statement that takes typed values. Handlers never import adapters. The composition root (`apps/cli`, `apps/server`) builds each backend once from the adapter's primitives and passes it to the interpreter.

```purescript
type Call = String -> String -> Aff { text :: String, raw :: String }  -- Handlers.Llm: system prompt and payload in, reply text out
type Backend = { model :: String, call :: Call, evidence :: Evidence -> Effect Unit, exchange :: Exchange -> Effect Unit }
type StoreBackend = { createRun :: NewRun -> Effect Unit, appendRound :: AppendRound -> Effect Unit, readRounds :: String -> Effect (Array StoredRound), ... }  -- Handlers.Store: one primitive per STORE operation, and the round write
```

`Adapters.Pi.pi` implements `Call`: one `pi` process per call, with its raw event stream saved. `Adapters.SQLite.sqliteBackend` implements `StoreBackend`: the SQL, and the JSON text of each stored value in both directions. The stub backend (`Test.Handlers.Stub`) answers typed replies in tests, and the recording replay answers from `capstone/transcript.json`; neither is production code.

**Run Store** (`effects/Signatures` `STORE`, `effects/Store` constructors, `handlers/Store`, `adapters/SQLite`). The run lifecycle writes (`createRun`, `setStatus`, `failRun`, `failRunning`, `setSpec`, `setArchived`, `deleteRun`) and the reads the server answers from (`listRuns`, `listSummaries`, `readManifest`, `readRounds`, `readMetric`) are operations. A program is written as `Run (STORE + r)` and run with `runStore backend`. `finish` (`Effects.Store`) is the lifecycle's last step: completed, or failed with the error as its reason. The reads are new operations because `RECORD` is write-only; each round's write stays `RECORD` (`persister`, `recorder`), which also stores the round's transcripts. Round records and snapshots are insert-only.

```purescript
data StoreF a = CreateRun NewRun (Unit -> a) | FailRunning String (Int -> a) | ... | ListRuns (Array RunInfo -> a) | ReadRounds String (Array StoredRound -> a) | ...
```

The startup policy (a run still `running` when the server starts is failed with reason `interrupted`) is `Server.Launch.failInterrupted`; the adapter supplies only the SQL. The schema, migrations, and endpoints are in [06 §8](06_simulator.md#8-persistence).

**Views** (`analysis/Views/*`). Every dashboard element is a pure projection of a stored run to one of these generic shapes, computed server-side. The dashboard renders them and computes nothing:

```purescript
type Table     = { title :: String, columns :: Array String, rows :: Array (Array String) }
type Chart     = { title :: String, unit :: String, kind :: Kind, series :: Array { name :: String, points :: Array { round :: Int, value :: String } } }
type Bars      = { title :: String, unit :: String, kind :: Kind, categories :: Array String, series :: Array { name :: String, values :: Array String } }
type Statistic = { name :: String, question :: String, definition :: String, formula :: String }
```

`Kind` is the display kind ([06 §9](06_simulator.md#9-pages)). Which views exist, and their routes, are in [06](06_simulator.md).

**Data formats.** Two JSON formats cover overlapping types and are not interchangeable:

- The **snapshot format** is the engine's own state as core's codecs write it: `WorldState`, round records, and manifests. Runs store it, and replay reads it back.
- The **premise format** is the authored configuration that `schema` decodes: Gameboard sections, `instantiation.json`, seat-policy presets, and fixtures. Its tags are `kind:argument` strings (constants in `Schema.Tags`), and its tagged unions are objects with exactly one key.

Each format's codec lives in the package that owns its type.

## 5. Coupling Rules

- `core` imports no effect library and no IO. Its JSON codecs are the `EncodeJson`/`DecodeJson` instances declared beside each type, because PureScript forbids orphan instances. The engine never calls them.
- Policies never touch World State. They receive a `View` and return Proposals and signatures.
- Handlers interpret operations over typed backends. Economic rules live only in `core`. Raw I/O primitives live only in `adapters`.
- Views are computed in `analysis/Views/*`. The dashboard holds no business logic.
- Replacing an adapter (the SQLite store, the LLM provider) changes nothing in `core`, `board`, `policy`, `effects`, or `handlers`: the new adapter implements the same primitives.
- Scenarios change by editing data. Needing new engine code for a scenario is a design finding to resolve in `core` ([05 §1](05_test_instance.md#1-purpose)).
- Per-round assertions are pure predicates in `core/Invariants` (`check`), shared by the tests, the simulator, and live play. Which assertions exist is in [06 §7.1](06_simulator.md#71-required-assertions).

## 6. Testing

Each package has its own suite, and the apps have suites for the CLI, server, and dashboard. The dashboard's Playwright specs run against a server on the E2E port. The required tests, including golden fixtures, replay, and persistence, are in [06 §14](06_simulator.md#14-mandatory-tests). Interpretation tests are separate from economic tests ([06 §7.4](06_simulator.md#74-ai-interpretation-tests)). Commands to run them are in `AGENTS.md`.
