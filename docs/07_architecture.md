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
  - **Adapters expose devices.** An adapter wraps one device (the filesystem, SQLite, a process, the clock, the OS seed) as raw primitives: text in, text out, or one statement. It knows nothing of games, runs, prompts or settings.
  - **Effects define the program's interface to a device.** A module in `effects` declares the operations a program may request, with their typed requests and replies. It contains no implementation.
  - **Handlers hold the program logic.** A handler interprets an effect's operations over a device primitive. Prompt building, reply decoding, fallbacks, run lifecycle, and recording belong here. Handlers never import adapters; the composition root wires the two.
  - A new device is a new adapter; a new program need is a new operation and its handler. Domain rules are not in any of the three; they stay in `core`, `schema`, `board`, `policy`, `checks`, `game`, `views`, `experiments`, and `settings`.
- **Settings are data.** Gameboards, instantiation configurations, fixtures, and presets are declarative data, authored in a typed PureScript DSL (`schema`'s builders) that compiles to JSON. The engine reads only the compiled files. Fixture expected values are literals, never computed.
- **Small.** One concept per module. No frameworks inside the core.

## 2. Runtime Shape

```text
   setting ──► game.assemble ──► Game  (manifest, world, in-world Strategies, rounds, one SeatBinding per seat)
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

A setting is assembled once into a `Game` ([§4](#4-key-interfaces)), and every runner plays that same `Game`: the simulator folds it with the player seed, and live play runs it through `runLive`. Each round is one `runRound` call. Seats propose and sign through `SEAT_INPUT`; the pure engine resolves the signed Proposals and never authors or rewrites an Intent. Handlers choose where seat inputs come from and where records go. The round rules are in [02](02_type_system.md) and [04](04_instantiated_game.md). The Run Store, the API, and the dashboard are in [06](06_simulator.md).

## 3. Codebase Layout

The project is a spago workspace. Each package is named `sp-<dir>` and compiles on its own. `just check` runs `scripts/check-deps.js`, which holds the `allowed` table below and checks the `import` lines of each package's `src/`. It fails on a dependency outside the table, on a pure package that depends on an effect, `aff`, `run`, or `node-*` library, and on an import its layer may not make. `scripts/check-layout.js` holds the concept rules that imports cannot express, and fails on a `.js` module anywhere but `adapters` and the apps: FFI is a device. `scripts/check-docs.js` compares the tables of docs/05 marked `<!-- check-docs: name -->` with the compiled setting. `scripts/check-derived.js` reports manual implementations of what should be derived from one description, under one rule id each (hand projections, hand codecs, Event matches, repeated names). `just check` fails if a rule's count in a file differs from `scripts/check-derived.baseline`, so new code cannot add a hit and a removal is recorded. Exceptions that are correct to keep are in `scripts/check-derived.allow`, each with a reason. Each rule has a positive and a negative sample in `scripts/check-derived-samples`, run by the same recipe.

| Package | May depend on | Owns | Must not own |
|---|---|---|---|
| `core` | nothing | The engine; World, Intent, and Proposal types; the Strategy language; the description algebra (`Shape`); numerics, draws, authority, and per-round invariants | Setting knowledge, authoring JSON, operations, runs, a second codec for a described type |
| `schema` | `core` | The setting format, the authoring DSL, the run vocabulary, and the joint pass's values | Realization logic, validation errors, operations, behaviour |
| `board` | `core`, `schema` | World realization and validation, and their errors | JSON, fixtures, seat binding, policy concepts |
| `policy` | `core`, `schema` | The pure Strategy interpreter | `run` or any effect library; codecs; World State access |
| `game` | `core`, `schema`, `board`, `policy` | Assembling a setting into a `Game`; seat bindings, overrides, and stored run specs. The only domain package where `board` and `policy` meet | Effects, round execution, persistence, the player seed, setting content |
| `settings` | `core`, `schema`, `game` | Authored settings, presets, fixtures, and the setting file tree, as typed values. `Settings.Kit` holds the generic definitions, `Settings.Compose` the operations on a Setting, and `Settings.Catalog` the list the compiler reads ([03 §12a](03_gameboard.md#12a-how-settings-are-made)) | IO, compiling, decoders, computed expected values |
| `checks` | `core`, `schema` | Checking expectations, runs, and objectives against records | Effects, running games |
| `effects` | `core`, `schema`, `board`, `policy` | The operation rows and the programs over them | Interpreters, handler records, domain rules, prompt text |
| `handlers` | `core`, `schema`, `board`, `policy`, `game`, `effects` | The interpreters of those operations: seats, live play, model calls, the Run Store, recording, replay, and API routing and client; and `Handlers.Simulate`, the fold over a `Game` and its round records, which imports no `board` or `policy` module | Adapter imports; `node-*` in `Api.Client`'s closure; `views`, `simulator`, or `experiments` imports; the API's operation semantics |
| `adapters` | `core`, `schema`, `board`, `policy`, `effects`, `handlers` (backend record types only) | Raw devices, and every FFI (`.js`) module outside the apps | Games, runs, prompts, settings, any handler function |
| `views` | `core`, `schema`, `checks` | Pure projections of a stored run to generic tables, charts, bars, and statistics | Effects; `handlers`, `game`, `experiments`, or `settings` imports; objective evaluation of their own |
| `experiments` | `core`, `schema`, `checks`, `views`, `game`, `handlers` (the pure `Handlers.Simulate` only) | Counterfactuals, agency, and seed batches, over the pure fold | Effects; persistence; `board` or `policy` imports |
| `testkit` | `core`, `schema`, `settings`, `game`, `adapters`, `handlers`, `checks` | The shared fixture set and capstone run for test suites | Anything a `src` module imports |
| `flags` (`apps/flags`) | nothing | The command-line flag parser that `cli` and `server` read their arguments with | Any workspace or effect import |
| `apps/cli`, `apps/server`, `apps/dashboard` | anything | Composition roots. `cli` runs games and compiles settings; `server` serves and implements the API; `dashboard` renders view models and calls the API | Instantiation, seat binding, economic logic; path strings and decoders in the dashboard |

Two deliberate exceptions to the layering: `testkit` depends on `adapters`, because it reads the setting files through the filesystem device. `handlers` does not list `checks`, so a handler never reaches the checkers.

Module map, by layer. This names what each layer is for, not its files; the table above gives each package's contents, and `find packages apps -name '*.purs'` gives the current modules:

```text
pure domain     core                    the engine and the types it reads
                schema                  the setting format, the authoring DSL, the run vocabulary
                board, policy           realizing a world; interpreting a Strategy
                game                    assembling a setting into a playable Game
                settings                authored content and fixtures, as typed values
                checks, views           predicates and projections over records
impure seams    effects                 the operations a program may request
                handlers                the program logic over those operations
                adapters                raw devices
runners         experiments             folds over a Game (the fold itself is handlers' Handlers.Simulate)
entry points    apps/cli, apps/server, apps/dashboard (apps/flags parses their arguments)
test support    testkit
```

A module is one concept, named `<Package>.<Concept>`. `Core.Engine.*` holds the engine's round phases, `Schema.Build.*` the authoring builders, and `Schema.Setting.*` the setting-format shapes.

Compiled setting files (`packages/settings/meridian/`, `packages/settings/presets.json`, and `packages/settings/fixtures/`) are gitignored build output. `packages/settings/capstone/transcript.json` is generated by `just _settings` (`apps/cli`'s `Transcript`) and is gitignored with the rest of `packages/settings/capstone/`: the capstone's joint-pass recording, with no model call. The shadcn/ui components in `apps/dashboard/ui` are generated, not committed.

## 4. Key Interfaces

These are the seams. Every other piece is an internal detail of a package. Signatures are as the code declares them.

**Setting and Game** (`game`). A setting is assembled once, purely, into the `Game` every runner plays. This is the only place a Gameboard is instantiated and its seats are bound:

```purescript
assemble :: Setting -> Bindings -> Array Override -> Either GameError Game
-- Setting  = GameboardSetting parts | LiteralSetting { world, rounds }
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

**Shape** (`core`, `Core.Shape`). A type that crosses a described boundary is described once, by a `Shape`. The same combinator calls give its codec for each format, and its prose. A record's Shape can be derived from a Shape per field (`recordShape`, over `RowToList`); a sum's from its `Generic` representation (`genericSum`, `genericSettingSum`, `genericTypedSum`, `genericEnum`), with one description per constructor, so a missing constructor is a compile error and writing a value cannot fall through. Hand-written `settingSum` lists remain only for guarded or merged cases (a setting Intent, a Strategy's Intent and Proposal templates). `Described` instances sit beside their types:

```purescript
data Format = Authored | Snapshot          -- authored setting files; stored runs and model replies
codec :: forall a. Format -> Shape a -> JsonCodec a
```

**Event slices** (`core`, `Core.Slice`). A selector (`event @"Coupled"`, or a projection over legs and causes such as `exchanges`, `conversions`, `shortfalls`) names what to read; the kind and field names are checked at compile time against `Event`. A `Slice` is a value: its kinds, field filters, the field read, and its aggregate. `runSlice` evaluates it and `describeSlice` renders the same value as formula text, so a Statistic's or Glossary entry's formula is generated from its calculation. A computation that treats several kinds differently is a total fold, `foldEvents`, one handler per kind; a kind with no effect is `ignore`d, so a new kind fails to compile.

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

**Replay** (`handlers`, `Handlers.Replay`). `runReplay` answers seats from a run's recorded Proposals and signatures (`docs/04 §11.1`). The test suites use it, and the server reaches it through `Handlers.Simulate.replayGame` to duplicate or rerun a live session.

**Live play** (`handlers`, `Handlers.Live`). `runLive` plays a `Game` in `Aff`. Its devices are supplied by the composition root (`apps/cli`, `Session`): the player seed, a line of input, one window's wait, output, the model calls, and the record write. `Human` and `Agent` seats' transcripts are answered by the live handler, a layer over `Handlers.Seats.handleSeats`, which answers everything else:

```purescript
runLive :: Live -> Game -> Aff (Either EngineError WorldState)
```

**Simulation** (`handlers`, `Handlers.Simulate`). The fold over a `Game`. It reads no entropy: the player seed is resolved before the call (`Adapters.Random`).

```purescript
simulate   :: Seed -> Game -> Either GameError SimRun
-- SimRun = { manifest :: Manifest, playerSeed :: Seed, records :: Array RoundResult, final :: Either EngineError WorldState }
```

Variants take a per-round sink, so the server writes each round as it completes, or a checkpoint to resume from.

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
-- handlers/Api/Route: the one endpoint table; parse and wire are folds over it, between HTTP and ApiRequest
route  :: Implementation -> Request -> Effect Reply      -- Implementation = ApiRequest -> Effect Reply
-- handlers/Api/Client: answers each operation over a fetch device, text in and text out
runApi :: forall a. Fetch -> Run (API + ()) a -> Aff a
```

`Implementation` is supplied by `apps/server` (`Server.Implementation.implement`). Its reads come from `OBSERVE` and `views`, and its launch, derive, and resume from `game` and `Handlers.Simulate`. The dashboard's `Dashboard.Client` runs its operations through `Handlers.Api.Client` over its `fetch` device. The browser closure of `Handlers.Api.Client` imports no Node module.

**Handlers and backends.** A handler interprets operations over a typed backend, a record of raw functions that its adapter implements. The logic (prompts, decoding, fallbacks, the joint pass, the run lifecycle) is in the handler, so a backend carries only primitives. Prompt text lives only in the model handler; adapters never hold prompts. Handlers never import adapters. The composition root (`apps/cli`, `apps/server`) builds each backend once from the adapter's primitives and passes it to the interpreter.

Each adapter implements one backend's primitives: a model call, a digest, the store's SQL and JSON text. Test stubs and the capstone's recording replay answer the same interfaces, and neither is production code.

**Primitive flows** (`core`). Every settlement is one event of legs with a structural cause, so the record states what moved and what produced it. The types are in the [Type System](02_type_system.md).

**Setting-neutral Core.** A setting declares its own kinds of Rule, its conditions on flows, and its recurring flows in the DSL. Core names none of them, and does nothing when a setting declares none. The `banned-word` rule of `check-derived` keeps scenario language out of `core`.

**Run rows.** Run status and derivation kind are one vocabulary in `schema`; the store's text is converted only at the adapter boundary. Server startup fails `running` rows it can tell were interrupted.

**Run Store** (`effects` rows `RECORD`, `STORE`, `OBSERVE`; its handler and adapter). Round writes are `RECORD`, lifecycle writes are `STORE`, and reads are `OBSERVE`, so a read program cannot write. The operations are in [02 §8.4](02_type_system.md#84-effects-and-handlers); the schema and endpoints are in [06 §8](06_simulator.md#8-persistence).

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
- The **setting format** is the authored configuration: Gameboard sections, `instantiation.json`, seat presets, and fixtures. Its tagged unions are objects with exactly one key, and it is described in `schema`.

## 5. Coupling Rules

- Only `core` changes the economy.
- Each described type is described once: `core` for engine types, `schema` for setting-format types, and `effects` for operation payloads (plain records today, see [02 §8.4](02_type_system.md#84-effects-and-handlers)). Its codec, prose, and `EncodeJson`/`DecodeJson` come from that one description. Each operation is declared once, in `effects`. Action lists, routes, and decoders come from those declarations.
- `core` imports no effect library and no IO. Its JSON codecs are the `EncodeJson`/`DecodeJson` instances declared beside each type, because PureScript forbids orphan instances. The engine never calls them.
- Capabilities are rows. A read program has only `OBSERVE`, and a `ProposalBody` carries no author.
- Policies never touch World State. They receive a `View` and return Proposals and signatures.
- Economic rules live only in `core`. Views are computed in `views`, and the dashboard holds no business logic.
- Interfaces are derived; behaviour (the Strategy interpreter, agent framing, human facilitation, view projections, `API` semantics) is hand-written.
- Replacing an adapter (the SQLite store, the LLM provider) changes nothing in `core`, `board`, `policy`, `effects`, or `handlers`: the new adapter implements the same primitives.
- Needing new engine code for a setting is a design finding to resolve in `core` ([05 §1](05_test_instance.md#1-purpose)).
- Per-round checks are pure predicates in `checks` and `core/Invariants` (`check`), shared by the tests, the simulator, and live play. Which assertions exist is in [06 §7.1](06_simulator.md#71-required-assertions).

## 6. Testing

Each package has its own suite, and the apps have suites for the CLI, server, and dashboard. The dashboard's Playwright specs run against a server on the E2E port. The shared fixtures are `testkit`. The required tests, including golden fixtures, replay, and persistence, are in [06 §14](06_simulator.md#14-mandatory-tests). Interpretation tests are separate from economic tests ([06 §7.4](06_simulator.md#74-ai-interpretation-tests)). Commands to run them are in `AGENTS.md`.
