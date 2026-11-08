# Technical Architecture

**Version:** 0.4  
**Scope:** Package layout, dependency direction, and the key interfaces between packages  
**Implementation language:** PureScript

---

## 1. Principles

- **Pure core, effects at the edge.** The engine is a pure function ([INV-36](02_type_system.md#inv-36)). Impure work is an algebraic effect (Type System [§8.4](02_type_system.md#84-effects-and-handlers)) interpreted by a handler chosen at the entry point.
- **One-way dependencies.** A package depends only on the packages in its row of the table in [§3](#3-codebase-layout). Packages at the same level never import each other.
- **Narrow seams.** Packages meet at the small interfaces in [§4](#4-key-interfaces). Code behind an interface can change without touching its callers.
- **Three layers for impure work.** Each concern has one home:
  - **Adapters expose devices.** An adapter wraps one device (the filesystem, SQLite, a process, the clock, the OS seed) as raw primitives: text in, text out, or one statement. It knows nothing of games, runs, prompts or premises.
  - **Effects define the program's interface to a device.** A module in `effects` declares the operations a program may request, with their typed requests and replies. It contains no implementation.
  - **Handlers hold the program logic.** A handler interprets an effect's operations over a device primitive. Prompt building, reply decoding, fallbacks, run lifecycle, and recording belong here. Handlers never import adapters; the composition root wires the two.
  - A new device is a new adapter; a new program need is a new operation and its handler. Domain rules are not in any of the three; they stay in `core`, `schema`, `board`, `policy`, `checks`, `game`, `views`, `experiments`, and `premises`.
- **Premises are data.** Gameboards, instantiation configurations, fixtures, and presets are declarative data, authored in a typed PureScript DSL (`schema`'s builders) that compiles to JSON. The engine reads only the compiled files. Fixture expected values are literals, never computed.
- **Small.** One concept per module. No frameworks inside the core.

## 2. Runtime Shape

```text
   premise ──► game.assemble ──► Game  (manifest, world, in-world Strategies, rounds, one SeatBinding per seat)
                                  │
          ┌───────────────────────┴────────────────────────┐
   simulate (pure fold, player seed)               runLive (Aff: human windows, agents)
          │                                                │
          └──────────── runRound (effectful) ──────────────┘
           SEAT_INPUT, INTERPRET, RECORD, NARRATE; pure resolveRound inside
                                  │ RECORD, STORE
                                  ▼
   Run Store ◄── OBSERVE ── server (Implementation) ◄── API ── Handlers.Api.Route ◄── dashboard (Handlers.Api.Client)
```

A premise is assembled once into a `Game` ([§4](#4-key-interfaces)), and every runner plays that same `Game`: the simulator folds it with the player seed, and live play runs it through `runLive`. Each round is one `runRound` call. Seats propose and sign through `SEAT_INPUT`; the pure engine resolves the signed Proposals and never authors or rewrites an Intent. Handlers choose where seat inputs come from and where records go. The round rules are in [02](02_type_system.md) and [04](04_instantiated_game.md). The Run Store, the API, and the dashboard are in [06](06_simulator.md).

## 3. Codebase Layout

The project is a spago workspace. Each package is named `sp-<dir>` and compiles on its own. `just check` runs `scripts/check-deps`, which holds the `allowed` table below and checks the `import` lines of each package's `src/`. It fails on a dependency outside the table, on a pure package that depends on an effect, `aff`, `run`, or `node-*` library, and on an import its layer may not make. `scripts/check-layout` holds the concept rules that imports cannot express, and fails on a `.js` module anywhere but `adapters` and the apps: FFI is a device.

| Package | May depend on | Owns | Must not own |
|---|---|---|---|
| `core` | nothing | The engine (`resolveRound`); World types; `IntentF`/`ProposalF`; the Strategy language and `InWorld`; `Shape`, its codecs, and the engine's `Described` instances; `Num`, `Draw`, `Authority`, `Proposal`, `Invariants.check` | Premise knowledge, authoring JSON, operations, runs, a second codec for a described type |
| `schema` | `core` | Premise-format shapes (Gameboard sections, instantiation config, objectives, presets, `RoundF`: `Observed`, `Expect`), and the authoring builders (`Schema.Build.*`) | Realization logic, validation errors, operations, behaviour |
| `board` | `core`, `schema` | World realization: `instantiate`, `validate`, `reconcile`; `ValidationError`, `Difference` | JSON, fixtures, seat binding, policy concepts |
| `policy` | `core`, `schema` | The Strategy interpreter: pure `propose` and `sign` over an `Env`; `inWorldOf` | `run` or any effect library; codecs; World State access |
| `game` | `core`, `schema`, `board`, `policy` | `Premise`, `assemble`, `Game`, `SeatBinding`, `Override`, `Recipe` (the stored spec and derive rules), `GameError`. The only domain package where `board` and `policy` meet | Effects, round execution, persistence, the player seed, premise content |
| `premises` | `core`, `schema`, `game` | Meridian and its presets, the fixtures (`Premises.Fixtures.*`), the capstone's authored DSL, the premise file tree and loader; all as typed values | IO, compiling, decoders, computed expected values |
| `checks` | `core`, `schema` | `Expect` against `Observed` (`observe`, `check`, `checkRound`); `checkRun`; objective evaluation (`evaluateObjectives`, `ObjectiveResult`) | Effects, running games |
| `effects` | `core`, `schema`, `board`, `policy` | The operation rows (`SEAT_INPUT`, `INTERPRET`, `NARRATE`, `RECORD`, `OBSERVE`, `STORE`, `API`) and the programs over them (`runRound`, `runGame`, `finish`) | Interpreters, handler records, domain rules, prompt text |
| `handlers` | `core`, `schema`, `board`, `policy`, `game`, `effects` | `Seats`, `Live` (`runLive`), `Llm`, `Store`, `Recording`, `Replay`; `Api.Route` and `Api.Client` | Adapter imports; `node-*` in `Api.Client`'s closure; `views`, `simulator`, or `experiments` imports; the API's operation semantics |
| `adapters` | `core`, `schema`, `board`, `policy`, `effects`, `handlers` (backend record types only) | Raw devices: `Pi`, `SQLite`, `Clock`, `Random`, `Digest`, `Console`, `Filesystem`; every FFI (`.js`) module outside the apps | Games, runs, prompts, premises, any handler function |
| `simulator` | `core`, `game`, `effects`, `handlers` | The fold over a `Game` (`Simulator.Simulate`) and the round records (`Simulator.Records`) | `board` or `policy` imports; premises, views, experiments |
| `views` | `core`, `schema`, `checks` | Pure projections of a stored run to `Table`, `Chart`, `Bars`, `Statistic` | Effects; `simulator`, `game`, `experiments`, or `premises` imports; objective evaluation of their own |
| `experiments` | `core`, `schema`, `checks`, `views`, `game`, `simulator` | Counterfactuals, agency, and seed batches (`Experiments.Agency`), over the pure `simulate` | Effects; persistence; `board` or `policy` imports |
| `testkit` | `core`, `schema`, `premises`, `game`, `adapters`, `simulator`, `checks` | The shared fixture set and capstone run for test suites | Anything a `src` module imports |
| `flags` (`apps/flags`) | nothing | The `--name value` parser (`parseFlags`, `flagOr`) that `cli` and `server` read their arguments with | Any workspace or effect import |
| `apps/cli`, `apps/server`, `apps/dashboard` | anything | Composition roots. `cli`: `run`, `session`, `Compile`, `Transcript`. `server`: `Http`, and the `API` operation semantics (`Server.Implementation`). `dashboard`: renders view models and calls the API | Instantiation, seat binding, economic logic; path strings and decoders in the dashboard |

Two deliberate exceptions to the layering: `testkit` depends on `adapters`, because it reads the premise files through the filesystem device. `handlers` does not list `checks`, so a handler never reaches the checkers.

Module map: every `src` module, one line per package, from `find packages apps -name '*.purs'` (test modules, `Test.<Pkg>.*` under each `test/`, are not listed):

```text
packages/adapters:   Adapters.Clock, Adapters.Console, Adapters.Digest, Adapters.Filesystem, Adapters.Pi, Adapters.Random, Adapters.SQLite
packages/board:      Board.Error, Board.Gameboard.Instantiate, Board.Gameboard.Reconcile, Board.Gameboard.Validate
packages/checks:     Checks.Objectives, Checks.Round, Checks.Run
packages/core:       Core.Authority, Core.Draw, Core.Engine.Circulate, Core.Engine.Commit, Core.Engine.Fund, Core.Engine.Prepare, Core.Engine.Route, Core.Engine.Screen, Core.Engine.Settle, Core.Invariants, Core.Map, Core.Market, Core.Num, Core.Proposal, Core.Query, Core.Represent, Core.Run, Core.Shape, Core.Strategy, Core.Types, Core.World
packages/effects:    Effects.Api, Effects.Observe, Effects.Round, Effects.Signatures, Effects.Store
packages/experiments: Experiments.Agency
packages/game:       Game.Assemble, Game.Bindings, Game.Error, Game.Override, Game.Premise, Game.Recipe, Game.Types
packages/handlers:   Handlers.Api.Client, Handlers.Api.Route, Handlers.Live, Handlers.Llm, Handlers.Llm.Wire, Handlers.Recording, Handlers.Replay, Handlers.Seats, Handlers.Store
packages/policy:     Policy.Interpret, Policy.InWorld, Policy.Strategy
packages/premises:   Premises.Capstone, Premises.Fixtures, Premises.Fixtures.Circulation, Premises.Fixtures.Common, Premises.Fixtures.ConflictingActions, Premises.Fixtures.GermanyEu, Premises.Fixtures.MarketExchange, Premises.Fixtures.NpcResponse, Premises.Fixtures.Reservation, Premises.Fixtures.SignedProposals, Premises.Fixtures.SmeRepresentation, Premises.Literal, Premises.Load, Premises.Meridian, Premises.Meridian.Entities, Premises.Meridian.Entities.Aggregates, Premises.Meridian.Entities.Capabilities, Premises.Meridian.Entities.Seats, Premises.Meridian.Holdings, Premises.Meridian.Parameters, Premises.Meridian.Policies, Premises.Names, Premises.Presets, Premises.Tree
packages/schema:     Schema.Build.Base, Schema.Build.Decision, Schema.Build.Fixture, Schema.Build.Intent, Schema.Build.Quantity, Schema.Build.World, Schema.Decode.Common, Schema.Gameboard, Schema.Premise.Actor, Schema.Premise.Fixture, Schema.Premise.Gameboard, Schema.Premise.Tags, Schema.Premise.Intent, Schema.Premise.Strategy, Schema.Premise.World, Schema.Round
packages/simulator:  Simulator.Records, Simulator.Simulate
packages/testkit:    Testkit.Capstone, Testkit.Fixture, Testkit.Gameboard, Testkit.Json, Testkit.Meridian
packages/views:      Views.Actions, Views.Agency, Views.Balance, Views.Balances, Views.Common, Views.Compare, Views.Difference, Views.Drivers, Views.Entity, Views.Glossary, Views.Markets, Views.Metrics, Views.Overview, Views.Representation, Views.Standings
apps/cli:            Compile, Compile.Render, Exit, Main, Options, Session, Transcript
apps/flags:          Flags (shared by apps/cli and apps/server)
apps/dashboard:      Dashboard.Api, Dashboard.Chart, Dashboard.Client, Dashboard.Components, Dashboard.DataTable, Dashboard.Entity, Dashboard.Glossary, Dashboard.Launch, Dashboard.Layout, Dashboard.Main, Dashboard.Palette, Dashboard.Results, Dashboard.Route, Dashboard.RunActions, Dashboard.RunForms, Dashboard.Runs, Dashboard.Theme, Dashboard.UI
apps/server:         Http, Server, Server.Cache, Server.Derive, Server.Errors, Server.Implementation, Server.Launch, Server.Read
```

Compiled premise files (`packages/premises/meridian/`, `packages/premises/presets.json`, `packages/premises/fixtures/`, and `packages/premises/capstone/run.json`) are gitignored build output. `packages/premises/capstone/transcript.json` is generated by `just _premises` (`apps/cli`'s `Transcript`) and is gitignored with the rest of `packages/premises/capstone/`: the capstone's joint-pass recording, with no model call. The shadcn/ui components in `apps/dashboard/ui` are generated, not committed.

## 4. Key Interfaces

These are the seams. Every other piece is an internal detail of a package. Signatures are as the code declares them.

**Premise and Game** (`game`). A premise is assembled once, purely, into the `Game` every runner plays. This is the only place a Gameboard is instantiated and its seats are bound:

```purescript
assemble :: Premise -> Bindings -> Array Override -> Either GameError Game
-- Premise  = GameboardPremise parts | LiteralPremise { world, rounds }
-- Bindings = Map EntityId SeatBinding
-- SeatBinding = Strategy | Script (Array ScriptRound) | Agent | Human
-- Game.Bindings: strategyBindings; scriptBindings (from rounds' inputs, sorted by submissionOrder)
```

**Instantiation and validation** (`board`). Pure:

```purescript
validateGameboard :: Gameboard -> InstantiationConfig -> Either (Array ValidationError) Gameboard
instantiate       :: Gameboard -> InstantiationConfig -> Either (Array ValidationError) InitialWorld
```

**Engine** (`core`, `Core.Engine.Commit`). Pure, and the only operation that changes the economy:

```purescript
resolveRound :: WorldState -> Array SignedProposal -> Either EngineError RoundResult
-- RoundResult = { world :: WorldState, record :: RoundRecord }
```

**Policy** (`policy`). A Strategy is pure and reduces to two functions over an environment. A seat, human or AI agent, answers the same steps through `SEAT_INPUT`:

```purescript
propose :: Strategy -> Env -> Array ProposalBody
sign    :: Strategy -> Env -> Array ProposalId     -- chooses among view.toSign
```

`View` (Type System [§4](02_type_system.md#4-policies)) is everything a Policy sees. Its `toSign` field lists the Proposals awaiting that Policy's signature, and its `markets` field the Markets the Entity has access to, which the Strategy language's Market selectors (`MarketRef`) pick from.

**Shape** (`core`, `Core.Shape`). A type that crosses a described boundary is described once, by a `Shape`. The same combinator calls give its codec for each format, and its prose. `Described` instances sit beside their types:

```purescript
data Format = Premise | Snapshot          -- authored premise files; stored runs and model replies
codec :: forall a. Format -> Shape a -> JsonCodec a
class Described a where shape :: Shape a
```

**Effects** (`effects`). The impure vocabulary of the system. Each row is declared in `Effects.Signatures`, or `Effects.Api` for `API`, with no implementation:

```purescript
type SEAT_INPUT r = (seatInput :: SeatInputF | r)   -- Propose, Sign, Speak: a seat's Proposals, signatures, transcript
type INTERPRET r  = (interpret :: InterpretF | r)   -- the round's transcripts to Intents (live play)
type NARRATE r    = (narrate :: NarrateF | r)       -- the front page, requested after each round
type RECORD r     = (record :: RecordF | r)         -- the round and the transcripts its joint pass read
type OBSERVE r    = (observe :: ObserveF | r)       -- Run Store reads: a read program has only this row
type STORE r      = (store :: StoreF | r)           -- Run Store lifecycle writes
type API r        = (apiCall :: ApiF | r)           -- every HTTP route: Call ApiRequest (Reply -> a)

runRound :: InWorld -> WorldState -> Run (SEAT_INPUT + INTERPRET + RECORD + NARRATE + r) (Either EngineError RoundResult)
```

`InWorld` is defined in `core` (`Core.Strategy`). The request and reply types of each operation are declared beside it. Randomness needs no effect: every draw is the pure `Core.Draw.draw` on the game seed or the player seed.

**Live play** (`handlers`, `Handlers.Live`). `runLive` plays a `Game` in `Aff`. Its devices are supplied by the composition root (`apps/cli`, `Session`): the player seed, a line of input, one window's wait, output, the model calls, and the record write. `Human` and `Agent` seats' transcripts are answered by the live handler, a layer over `Handlers.Seats.handleSeats`, which answers everything else:

```purescript
runLive :: Live -> Game -> Aff (Either EngineError WorldState)
```

**Simulation** (`simulator`, `Simulator.Simulate`). The fold over a `Game`. It reads no entropy: the player seed is resolved before the call (`Adapters.Random`).

```purescript
simulate   :: Seed -> Game -> Either GameError SimRun
simulateWith :: Seed -> Game -> (Manifest -> Effect RecordSink) -> Effect (Either GameError (Either EngineError WorldState))
resumeFrom :: Seed -> Game -> Checkpoint -> Either GameError SimRun
resumeWith :: Seed -> Game -> Checkpoint -> (Manifest -> Effect RecordSink) -> Effect (Either GameError (Either EngineError WorldState))
-- SimRun = { manifest :: Manifest, playerSeed :: Seed, records :: Array RoundResult, final :: Either EngineError WorldState }
```

`simulateWith` and `resumeWith` take the per-round sink, so the server writes each round as it completes. `resumeFrom` is the pure form.

**Checks** (`checks`). Pure predicates over records, shared by the golden tests, the capstone, live play, views, and experiments:

```purescript
checkRun           :: WorldState -> Array RoundResult -> Array Failure
checkRound         :: Expect -> RoundResult -> Array Failure
observe            :: RoundResult -> Observed
evaluateObjectives :: Gameboard -> Array RoundResult -> Array ObjectiveResult
```

Objectives are a view over the records and never touch the engine (Gameboard [§10.5](03_gameboard.md#105-evaluation)).

**API** (`effects` row `API`, `handlers` routing and client, `apps/server` semantics). Each route is one operation of the `API` row, and the route table is [06 §8.1](06_simulator.md#81-read-api-and-run-operations). A request is an `ApiRequest`, and a reply is a status and a body:

```purescript
-- handlers/Api/Route: the one route table; parse and wire map between HTTP and ApiRequest
route  :: Implementation -> Request -> Effect Reply      -- Implementation = ApiRequest -> Effect Reply
-- handlers/Api/Client: answers each operation over a fetch device, text in and text out
runApi :: forall a. Fetch -> Run (API + ()) a -> Aff a
```

`Implementation` is supplied by `apps/server` (`Server.Implementation.implement`). Its reads come from `OBSERVE` and `views`, and its launch, derive, and resume from `game` and `simulator`. The dashboard's `Dashboard.Client` runs its operations through `Handlers.Api.Client` over its `fetch` device. The browser closure of `Handlers.Api.Client` imports no Node module.

**Handlers and backends.** A handler interprets operations over a typed backend, a record of raw functions that its adapter implements. The logic (prompts, decoding, fallbacks, the joint pass, the run lifecycle) is in the handler, so a backend carries only primitives. Prompt text lives only in `Handlers.Llm.Wire`; adapters never hold prompts. Handlers never import adapters. The composition root (`apps/cli`, `apps/server`) builds each backend once from the adapter's primitives and passes it to the interpreter.

```purescript
type Call = String -> String -> Aff { text :: String, raw :: String }  -- Handlers.Llm: system prompt and payload in, reply text out
type Backend = { model :: String, call :: Call, digest :: String -> String, timeoutMs :: Int, evidence :: Evidence -> Effect Unit, exchange :: Exchange -> Effect Unit }  -- Handlers.Llm: the call, the digest device, and the evidence and recording sinks
type StoreBackend = { createRun :: NewRun -> Effect Unit, appendRound :: AppendRound -> Effect Unit, readRounds :: String -> Effect (Array StoredRound), ... }  -- Handlers.Store: one primitive per operation
```

`Adapters.Pi.pi` implements `Call`: one `pi` process per call, with its raw event stream saved. `Adapters.Digest.sha256` is the `digest`, which the handler applies to each call's template and payload for its evidence. `Adapters.SQLite` implements `StoreBackend`: the SQL, and the JSON text of each stored value in both directions. The stub backend (`Test.Handlers.Stub`) answers typed replies in tests, and the recording replay answers from `packages/premises/capstone/transcript.json`; neither is production code.

**Round records and run rows.** `Core.Run` holds `RunStatus` and `DeriveKind`; the store's text is converted only at the `Adapters.SQLite` boundary. A round record's events include `OrderRemoved` for one order removed at settlement and `Tariffed` for a tariff paid. The `runs` table's `owner` column names the role, process, and start time that created a row, and server startup fails only `running` rows of the `server` role or with no owner. `Core.Map` holds the shared Map helpers; `Game.Bindings` builds seat bindings; `Shape.parse` is the one text-to-value entry point for a described type.

**Run Store** (`effects` rows `RECORD`, `STORE`, `OBSERVE`; `handlers/Store`; `adapters/SQLite`). Round writes are `RECORD` (`recorder`), which also stores the round's transcripts. Lifecycle writes are `STORE` (`CreateRun`, `SetStatus`, `FailRunning`, `SetSpec`, `SetArchived`, `DeleteRun`), and a run's last step is `finish`. A status is `Core.Run.RunStatus` everywhere but the store's text, which `Adapters.SQLite` converts; a derivation's kind is `Core.Run.DeriveKind`. Reads are `OBSERVE` (`ListRuns`, `ListSummaries`, `ReadManifest`, `ReadRounds`), so a read program cannot write. The startup policy is `Server.Launch.failInterrupted`. The schema, migrations, and endpoints are in [06 §8](06_simulator.md#8-persistence).

**Views** (`views/Views.*`). Every dashboard element is a pure projection of a stored run to one of these generic shapes, computed server-side. The dashboard renders them and computes nothing:

```purescript
type Table     = { title :: String, columns :: Array String, rows :: Array (Array String) }
type Chart     = { title :: String, unit :: String, kind :: Kind, series :: Array { name :: String, points :: Array { round :: Int, value :: String } } }
type Bars      = { title :: String, unit :: String, kind :: Kind, categories :: Array String, series :: Array { name :: String, values :: Array String } }
type Statistic = { name :: String, question :: String, definition :: String, formula :: String }
```

`Kind` is the display kind ([06 §9](06_simulator.md#9-pages)). Which views exist, and their routes, are in [06](06_simulator.md).

**Data formats.** Two JSON formats cover overlapping types and are not interchangeable (`Core.Shape.Format`):

- The **snapshot format** is the engine's own state as core's codecs write it: `WorldState`, round records, and manifests. Runs store it, and replay reads it back.
- The **premise format** is the authored configuration: Gameboard sections, `instantiation.json`, seat presets, and fixtures. Its tagged unions are objects with exactly one key, and it is described in `schema`.

## 5. Coupling Rules

- Only `core` changes the economy.
- Each described type is described once: `core` for engine types, `schema` for premise-format types, and `effects` for operation payloads (plain records today, see [02 §8.4](02_type_system.md#84-effects-and-handlers)). Its codec, prose, and `EncodeJson`/`DecodeJson` come from that one description. Each operation is declared once, in `effects`. Action lists, routes, and decoders come from those declarations.
- `core` imports no effect library and no IO. Its JSON codecs are the `EncodeJson`/`DecodeJson` instances declared beside each type, because PureScript forbids orphan instances. The engine never calls them.
- Capabilities are rows. A read program has only `OBSERVE`, and a `ProposalBody` carries no author.
- Policies never touch World State. They receive a `View` and return Proposals and signatures.
- Economic rules live only in `core`. Views are computed in `views`, and the dashboard holds no business logic.
- Interfaces are derived; behaviour (the Strategy interpreter, agent framing, human facilitation, view projections, `API` semantics) is hand-written.
- Replacing an adapter (the SQLite store, the LLM provider) changes nothing in `core`, `board`, `policy`, `effects`, or `handlers`: the new adapter implements the same primitives.
- Needing new engine code for a premise is a design finding to resolve in `core` ([05 §1](05_test_instance.md#1-purpose)).
- Per-round checks are pure predicates in `checks` and `core/Invariants` (`check`), shared by the tests, the simulator, and live play. Which assertions exist is in [06 §7.1](06_simulator.md#71-required-assertions).

## 6. Testing

Each package has its own suite, and the apps have suites for the CLI, server, and dashboard. The dashboard's Playwright specs run against a server on the E2E port. The shared fixtures are `testkit`. The required tests, including golden fixtures, replay, and persistence, are in [06 §14](06_simulator.md#14-mandatory-tests). Interpretation tests are separate from economic tests ([06 §7.4](06_simulator.md#74-ai-interpretation-tests)). Commands to run them are in `AGENTS.md`.
