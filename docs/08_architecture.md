# Architecture Guidelines

**Version:** 0.5  
**Scope:** Guidelines for building the system: implementation principles, package responsibilities, dependency rules and their checks, the seams between packages, and when to add a package or module  
**Implementation language:** PureScript

---

## Implementation terms

These terms are for people working on the code. The anchors are defined in [01 Terms](01_overview.md#terms).

| Term | Definition |
|---|---|
| **Layer** | One spec's share of the design: the kernel ([02](02_kernel.md)), the Premise ([03](03_premise.md)), the Run ([04](04_run.md)), the Simulator and Dashboard ([07](07_simulator.md)), or this doc. |
| **`PreparedRun`** | A Run ready to play, which `assemble` builds and every runner plays: the initial World, the Run manifest, the in-world Strategies (with the routine each non-playable Actor runs after its Strategy), the rounds to play, and one `SeatBinding` per seat. `assembleCast` builds it from a cast ([04 §2](04_run.md#2-instantiation-configuration)). |
| **Round runner** | The effectful code that runs Propose and Sign for every Strategy, then calls the pure `resolveRound` ([§2](#2-runtime-shape)). |
| **Kernel** | The modules under `packages/core/src/Core/Kernel`: the types of [02 §3](02_kernel.md#3-the-kernel), posting, resolution, consumption, Laws, rationing, clearing and routing. |
| **Settlement sequence** | The per-mechanism order of `resolveRound` in `packages/laws`, which calls the kernel in stated phases ([§4](#4-key-interfaces)). |
| **Compile step** | The code in `packages/laws` that writes each settled mechanism as kernel values, with the static refusals decided first ([03 §14](03_premise.md#14-compilation-to-the-kernel)). |
| **Shape** | A typed description of a type that crosses a described boundary. One Shape gives the type's codec for each JSON format and its prose. |

## System Wiring

What runs, and what it connects to. Arrows point from the caller to what it calls. The handlers, engine and adapters run inside the server or CLI process.

```text
 Audio transcription       Newspaper narrative       Browser: landing page,
 (separate repository)     (separate repository)     balance sheets, admin dashboard
           │                         │                         │
           │ session status,         │ round report            │ dashboard API,
           │ a seat's text           │                         │ admin session cookie
           │ (open routes)           │ (open route)            │
           └─────────────────────────┼─────────────────────────┘
                                     │ HTTPS
                                     ▼
                                   nginx
                                     │
                                     ▼
 ┌─ apps/server ───────────────────────┐      ┌─ apps/cli ──────────────┐
 │ HTTP ──► Gate ──► route table       │      │ sim, session, compile   │◄── facilitator
 │ OpenAPI documents, hosted sessions  │      │ facilitator console     │    at a terminal
 └──────────────────┬──────────────────┘      └────────────┬────────────┘
                    └──────────────────┬───────────────────┘
                                       ▼
                       effect rows and their handlers
                       (round runner, Run Store, model calls)
                          │                         │
                          ▼                         ▼
                  pure engine: laws, core       adapters
                  (resolveRound)                    │
                                                    │
        ┌─────────────┬───────────────┬─────────────┼─────────────┐
        ▼             ▼               ▼             ▼             ▼
     Adapters.     Adapters.       Adapters.     Adapters.    Adapters.Clock
     Pi            db              Filesystem    Console      Random, Process
        │             │               │             │         Digest, Exit
        ▼             ▼               ▼             ▼             ▼
     pi process    runs.db         Premise files,  terminal   operating
        │          (Run Store)     (CLI only)       system
        ▼                          evidence,
     Interence                     .env,
     provider                       openapi/
```

The two external services see only the open routes, whose contract is the OpenAPI documents (07 §8.2). Every other route needs the admin session. Model calls leave the system only through the `pi` process.

## 1. Principles

These govern every package. They are what a change is checked against.

- **Pure core, effects at the edge.** The engine is a pure function ([INV-36](02_kernel.md#inv-36)). Impure work is an algebraic effect ([02 §8.4](02_kernel.md#84-effects-and-handlers)) interpreted by a handler chosen at the entry point.
- **Three layers for impure work.** Each concern has one home.
  - **Adapters expose devices.** An adapter wraps one device (the filesystem, SQLite, a process, the clock, the OS seed) as raw primitives: text in, text out, or one statement. It knows nothing of Runs, prompts or Premises.
  - **Effects declare the program's interface to a device.** An effect module declares the operations a program may request, with their typed requests and replies. It contains no implementation. Each program's type lists the rows it may request, so a read program cannot write. For example, `OBSERVE` is the only row a Run Store read has.
  - **Handlers hold the program logic.** A handler interprets an effect's operations over a device primitive. Prompt building, reply decoding, fallbacks, run lifecycle and recording belong here. Handlers never import adapters; the composition root wires the two.
  - A new device is a new adapter. A new program need is a new operation with its handler. Domain rules are in none of the three: they stay in the domain packages.
- **One-way dependencies.** A package depends only on the packages in its row of the table in [§3](#3-codebase-layout). Packages at the same level never import each other.
- **Narrow seams.** Packages meet at the small interfaces in [§4](#4-key-interfaces). Code behind an interface can change without touching its callers. Replacing an adapter (the SQLite store, the model provider) changes nothing in the domain packages, effects or handlers: the new adapter implements the same primitives.
- **Each described type is described once.** A type that crosses a boundary has one Shape, and its codecs and prose are derived from it. Hand-written instances, projections and name lists that repeat a description are defects.
- **Interfaces are derived; behaviour is hand-written.** Codecs, action lists, routes and decoders come from declarations. The `StrategyRules` interpreter, agent framing, human facilitation, view projections and API semantics are written by hand.
- **Economics live in one place.** Only the settlement sequence in `laws`, over the kernel in `core`, changes the economy. Every settled Flow posts through `Core.Kernel.Post`. Apps compose and hold no economic logic.
- **Premises are data.** Premises, instantiation configurations, fixtures and presets are authored in a typed DSL and compiled to data. The engine reads only the compiled data. Fixture expected values are literals, never computed. The formats are owned by [02](02_kernel.md) (the `Snapshot` format of round-end Worlds) and [03](03_premise.md) (the Premise format).
- **Small.** One concept per module. No frameworks inside the core.

## 2. Runtime Shape

The runtime is one pure engine and one round runner that calls it. A PremiseComposition is assembled once into a `PreparedRun`, and every runner plays that same `PreparedRun`: the simulator folds it with the player seed, and live play runs it with human and agent seats.

```text
   composition ──► assemble ──► PreparedRun
                              │
          ┌───────────────────┴───────────────────┐
   simulate (pure fold)                     runLive (Aff)
          └──────────── runRound (effectful) ─────┘
           seat input; pure resolveRound inside
                              │ record, store
                              ▼
   Run Store ◄── server ◄── API ◄── dashboard
```

Each round is one `runRound` call. Seats propose and sign through the seat-input effect. The pure engine resolves the signed candidate Transactions and never authors or rewrites an Intent. Handlers choose where seat input comes from and where records go. The round rules are in [02](02_kernel.md) and [04](04_run.md). The Run Store, API and dashboard are in [07](07_simulator.md).

## 3. Codebase Layout

The project is a spago workspace. Each package is named `sp-<dir>` and compiles on its own. The table is the rule a builder follows: a package owns what its row says, must not own what its row excludes, and imports only from its row's dependencies. Modules are listed by `find packages apps -name '*.purs'`; they are not repeated here.

| Package | May depend on | Owns | Must not own |
|---|---|---|---|
| `core` | nothing | The kernel; the description algebra; numerics, draws, the cost curve, and the id, quantity and curve types | Any authored vocabulary (Intent, candidate Transaction, Rule, Transformation and the like); Premise knowledge, authoring formats, operations, runs, a second codec for a described type |
| `laws` | `core` | The authored vocabulary and the settlement sequence: World and Events, Intent and candidate Transaction types, the `StrategyRules` language, the compile step, `resolveRound`, authority, Representation and per-round invariants | Premise knowledge, authoring formats, operations, runs |
| `schema` | `core`, `laws` | The Premise format and the compiled PremiseComposition type, the authoring DSL (including `Schema.Build.Agreement`), the run vocabulary, and the joint pass's values | Realization logic, validation errors, operations, behaviour |
| `world` | `core`, `laws`, `schema` | World realization and validation, and their errors. It is separate from `assembly` so that the package boundary keeps what the world is apart from how seats decide | JSON, fixtures, seat bindings, strategy concepts |
| `strategy` | `core`, `laws`, `schema` | The pure `StrategyRules` interpreter | Effect libraries, codecs, World access |
| `assembly` | `core`, `laws`, `schema`, `world`, `strategy` | Assembling a PremiseComposition into a `PreparedRun`; seat bindings, overrides and stored run specs. The only domain package where `world` and `strategy` meet | Effects, round execution, persistence, the player seed, Premise content |
| `premises` | `core`, `laws`, `schema`, `assembly` | Authored Premises and PremiseCompositions, `compose` and its refusals, presets, fixtures and the file tree of compiled compositions and Premises, as typed values ([03 §12a](03_premise.md#12a-how-premises-are-made)) | IO, compiling, decoders, computed expected values |
| `checks` | `core`, `laws`, `schema` | Checking expectations, runs and objectives against records | Effects, playing Runs |
| `effects` | `core`, `laws`, `schema`, `world`, `strategy`, `assembly` | The operation rows and the programs over them | Interpreters, handler records, domain rules, prompt text |
| `handlers` | `core`, `laws`, `schema`, `world`, `strategy`, `assembly`, `effects`, `views` (`Handlers.Api.Contract` only) | The interpreters of the operations: seats, live play, model calls, the Run Store, recording, replay, the API's contract, routing and client, and the pure simulation fold | Adapter imports; `experiments` or `premises` imports; the API's operation semantics; a browser-facing client importing a Node module |
| `adapters` | `handlers` (the device types of the SQL handler only) | Raw devices, and every FFI module outside the apps | Runs, prompts, Premises, any handler function |
| `views` | `core`, `laws`, `schema`, `checks` | Pure projections of a stored run to generic tables, charts and statistics | Effects; `handlers`, `assembly`, `experiments` or `premises` imports; objective evaluation of their own |
| `experiments` | `core`, `laws`, `schema`, `checks`, `views`, `assembly`, `handlers` (the pure simulation fold only) | Agency and seed batches, over the pure fold | Effects; persistence; `world` or `strategy` imports |
| `testkit` | `core`, `laws`, `schema`, `premises`, `assembly`, `adapters`, `handlers`, `checks`, `views`, `effects` | The shared fixture set and composition loading for test suites | Anything a `src` module imports |
| `flags` | nothing | The command-line flag parser that the entry points read their arguments with | Any workspace or effect import |
| `apps/cli`, `apps/server`, `apps/dashboard` | anything | Programs and their composition roots. `cli` plays Runs and compiles PremiseCompositions; `server` serves and implements the API; `dashboard` renders view models and calls the API | Instantiation, seat binding, economic logic; path strings and decoders in the dashboard |

Two deliberate exceptions to the layering. `testkit` may import `adapters`, because it reads the compiled files through the filesystem device. `handlers` does not import `checks`, so a handler never reaches the checkers.

**Where a new concept goes.** A concept that fits a package's row becomes a module in that package, named `<Package>.<Concept>`. A new package is added only when the build must enforce a boundary that module rules cannot: a concept that must not see something another package sees. A new package also needs a row in this table. A new device is a new adapter, and a new program need is an operation with its handler ([§1](#1-principles)).

Compiled composition and Premise files are gitignored build output, generated by the compiler and never hand-edited.

## 4. Key Interfaces

These are the seams. Every other piece is an internal detail of a package. Signatures are as the code declares them.

**PremiseComposition and PreparedRun** (`assembly`). A composition is assembled once, purely, into the `PreparedRun` every runner plays. This is the only place a composition is instantiated and its seats are bound:

```purescript
assemble :: Origin -> Bindings -> Array Override -> Either AssemblyError PreparedRun
-- Origin = FromComposition CompositionParts | FromLiteral { world, rounds }
-- Bindings = Map ActorId SeatBinding
-- SeatBinding = StrategyRules | Script (Array ScriptRound) | Agent (Maybe model) | Human | Composed { routine, player }
```

**Instantiation and validation** (`world`). Pure and atomic: it returns the initial World or every error.

```purescript
instantiate :: PremiseComposition -> InstantiationConfig -> Either (Array ValidationError) InitialWorld
```

**Engine** (`laws`, `Laws.Engine.Commit`). Pure, and the only operation that changes the economy. It is the settlement sequence, and there is deliberately no single kernel call for a whole round ([02 §3.5](02_kernel.md#35-resolution)):

```purescript
resolveRound :: WorldState -> Array SignedCandidateTransaction -> Either EngineError RoundResult
-- RoundResult = { world :: WorldState, record :: RoundRecord }
```

**Kernel** (`core`). Resolution runs a set of transactions through the removal procedure, and `Post` is the only writer of the ledger. The compile step (`Laws.Engine.Compile`) writes each mechanism as kernel values and calls them:

```purescript
resolve :: Kernel cap query -> Array (Transaction cap law query reason cause) -> Either ResolveError (Resolved law reason)
post    :: Opening -> Short -> { from :: Endpoint, to :: Endpoint, resource :: ResourceId, owed :: Q } -> Ledger -> Posted
spawned :: RoundIndex -> Map String (Template cap law query reason cause) -> Array (Transaction cap law query reason cause) -> Array (Consumed law reason) -> Either ConsumeError (Array (Pending cap law query reason cause))
```

**Round runner** (`effects`). Every Strategy, seat or in-world, is evaluated through the same code in live play and in simulation, and then `resolveRound` is called. The round's effect row is the union of the rows it may request:

```purescript
runRound :: forall r. InWorld -> WorldState -> Run (SEAT_INPUT + INTERPRET + RECORD + r) (Either EngineError RoundResult)
```

Randomness needs no effect. Every draw is a pure function of the game seed or the player seed. The player seed is resolved once, before the call.

**Strategy** (`strategy`). `StrategyRules` are pure and reduce to two functions over an environment. A seat, human or AI agent, answers the same two steps:

```purescript
propose :: StrategyRules -> Env -> Array CandidateTransactionBody
sign    :: StrategyRules -> Env -> Array CandidateTransactionId     -- chooses among the candidate Transactions awaiting signature
```

The environment is the strategy's `View` ([02 §4](02_kernel.md#4-strategies)). A Strategy never touches the World.

**Simulation** (`handlers`). The fold over a `PreparedRun`. It reads no entropy, and it is the only pure runner.

```purescript
simulate :: Seed -> PreparedRun -> Either AssemblyError SimRun
```

**API** (`effects` row `API`, `handlers`, `apps/server`). Each HTTP route is one operation of the `API` row. A request is an `ApiRequest`, and a reply is a status and a body. The row and the route table are in [07 §8](07_simulator.md#8-persistence); the server's implementation of the operations is in `apps/server`, and the dashboard calls them through the browser-safe client. `Handlers.Api.Contract` states what each operation answers: its reply Shape, format, success status and the row of refusal labels it may make, one entry per constructor. A handler returns its reply value or one of those refusals, `dispatch` encodes it, and a refusal the entry does not declare does not compile. Each row declares its `Access`, `Admin` unless the table says `Open`; `Gate.gated` in `apps/server` checks it before the operation runs, against the password in the server's `.env` ([07 §8.3](07_simulator.md#83-public-and-admin-surfaces)).

```purescript
type API r = (apiCall :: ApiF | r)   -- every route: Call ApiRequest (Reply -> a)
```

**Run Store** (`effects` rows `RECORD`, `STORE`, `OBSERVE`). Round writes are `RECORD`, lifecycle writes are `STORE`, and reads are `OBSERVE`. The operations are in [02 §8.4](02_kernel.md#84-effects-and-handlers) and the store in [07 §8](07_simulator.md#8-persistence).

## 5. Enforcement

Checks run under `just check`, and each fails the build on a violation. Each check has a positive and a negative sample, so a rule can be shown to fire.

- **Dependency table.** Each package's imports stay in its row of [§3](#3-codebase-layout). Pure packages import no effect, async or Node library.
- **Concept rules.** Rules that imports cannot express: foreign code (`.js`) lives only in adapters and apps; `core` names no authored vocabulary; only `Core.Kernel.Post` writes the ledger or builds a settled Flow.
- **Impurity.** Only composition roots import adapters, and adapters use no domain words or domain imports.
- **Derived rule.** Flags hand-written projections, codecs, event matches and repeated names where one description should generate them. Each exception is listed with its reason.
- **Docs vocabulary.** The docs keep the anchors of [01](01_overview.md) and no retired word (`scripts/check/docs.js`).
- **Invariants.** Per-round checks are pure predicates shared by the tests, the simulator and live play. Which assertions exist is in [07 §7.1](07_simulator.md#71-required-assertions).

Tests live in each package's own test directory and in the apps' suites. The `core` suite covers the kernel only; the vocabulary's tests are in `packages/laws/test`. The mandatory tests are in [07 §14](07_simulator.md#14-mandatory-tests).

## 6. One Description, Many Interfaces

Most of what a person or a program touches in this system is produced from a small number of typed descriptions. A Premise is written once, and PremiseCompositions are reshaped by composition, derivation and filtering. A type is described once, and its codecs and prose come from that description. An operation is declared once, and the server and the client both come from that declaration. Each path below produces with a component that is correct by construction.

```text
 Objects are written plainly. An arrowhead points at the object produced, and the
 word on the shaft names what produces it.

 RESHAPING A COMPOSITION

                    DSL builders
                          │ write
          ┌───────────────┴───────────────┐
          ▼                               ▼
 generic definitions                  Premises
          │                               │
          └───────────────┬───────────────┘
                          │ compose (ordered; conflicts refused)
                          ▼
                 PremiseComposition ── derive (dials, patches) ──► variant composition
                          │ filter or keep
          ┌───────────────┼───────────────┐
          ▼               ▼               ▼
 filtered composition  whole composition  fixture ◄── add ── seat answers, expectations
          │               │               │
          └───────────────┼───────────────┘
                          │ compile
                          ▼
              composition and Premise files
                          │ load, compose, checkComposition
                          ▼
              validated PremiseComposition
                          │ assemble (seat bindings, overrides)
                          ▼
                     PreparedRun
                          │ simulate (player seed), runLive
                          ▼
                    round records
                          │ store
                          ▼
                      Run Store
                          │ project
                          ▼
                     view models

 ONE SHAPE PER TYPE

 Shape                ── Authored codec ─────► Premise and composition files, fixtures
 Shape                ── Snapshot codec ─────► stored Runs, model replies
 Shape                ── description ────────► prose, a model's action list, JSON Schema, OpenAPI

 ONE STRATEGY LANGUAGE

 preset               ── bind ───────────────► StrategyRules
 StrategyRules        ── propose ────────────► candidate Transactions
 StrategyRules        ── sign ───────────────► signatures
 StrategyRules        ── strategyRefs ───────► names to check
 StrategyRules        ── Authored codec ─────► Premise files

 ONE RUN RECIPE

 composition, overrides ── resolve ──────────► variant composition

 ONE RECORD OF A ROUND

 RoundF f p           ── instantiate ────────► Authored, Expect, Observed
 Authored             ── toRound ────────────► Expect
 round records        ── observe ────────────► Observed
 Expect, Observed     ── check ──────────────► failures

 ONE EVENT TYPE

 Event                ── select ─────────────► Slice
 Slice                ── runSlice ───────────► number, test assertion
 Slice                ── describeSlice ──────► formula text

 ONE COLUMN LIST

 view columns         ── glossary ───────────► dashboard glossary
 view columns         ── compile ────────────► dashboard test column list

 ONE ACTOR'S PAGE

 stored Run           ── balanceSheetAt ─────► one Actor's balance sheet
 stored Run           ── linksOf ────────────► each Actor's path

 ONE OPERATION TABLE

 ApiRequest           ── table ──────────────► endpoints
 endpoints            ── parse ──────────────► apps/server
 endpoints            ── wire ───────────────► Handlers.Api.Client
 endpoints, contract  ── document ───────────► OpenAPI documents
 contract             ── dispatch ───────────► apps/server
 contract             ── send ───────────────► Handlers.Api.Client
 Handlers.Api.Client  ── fetch ──────────────► apps/dashboard
```

**Reshaping a composition.** Composition, derivation and filtering are read-only over the authored source, and each fails loudly rather than doing nothing: two Premises defining one thing differently, a name no Premise defines, a patch that names nothing, or a kept thing that refers to a dropped one ([03 §12a](03_premise.md#12a-how-premises-are-made)).

Typed names make a builder accept only the right kind of id, so naming an Exchange where an Actor belongs fails to compile. A single walk over a World's references serves both validation and instantiation, so a new kind of reference is handled in one place.

**The names a model may use come from Shapes.** A Shape marks an id with the kind of thing it names (`refersTo`). The joint pass walks its reply's description for those kinds and offers each one's names from the World being played (`Handlers.Llm.Joint.jointRequest`), so a Premise supplies nothing and cannot leave a kind out, and a new kind does not compile until `namesOfKind` answers it. A reply naming something the World lacks gets a correction; one still wrong after the corrections stands, and the engine refuses it on the record. An id only the engine assigns, a Rule's, is marked as such.

**Formats come from Shapes.** A `Shape` carries a codec for each JSON format and a description. The Premise files, the stored Runs, the prose and what a model is told it may do are therefore the same statement of the type ([§1](#1-principles)).

**Strategies are a language with several readers.** `StrategyRules` is a value, not code. The interpreter runs it through `propose` and `sign`, a query reads every name it refers to for the load-time check, and a Shape stores it. A preset is one shared set of rules that each Premise binds to its own names and thresholds, so results stay comparable across Premises.

**Runs are reshaped as compositions are.** Overrides change values within a composition's structure. Each Run records its recipe, so what produced it can always be read back.

**Fixtures and tests get their own authoring language.** A fixture is not a hand-built World. It is a filter of the PremiseComposition it tests, plus what each seat does and what should happen. It plays through `simulate` as a `PreparedRun` like any other, so its expectations are checked against the records the engine writes. `RoundF f p` is one record type filled in three ways, so what a fixture may assert is exactly what a round can produce. A new field fails to compile until the checker compares it.

**Assertions come from the event type.** A selector such as `` event @"Rejected" `` names an `Event` constructor, and field selectors are checked against that constructor at compile time, so a test cannot assert on something an event does not carry. A `Slice` is a value, which is why one definition gives both the number and the formula text a view shows beside it.

**The static suite.** `Premises.Suite` holds each composition's simulated runs as data (one per preset, and one agency run per seat with an alternative) and `planSuite`, which, from their cache keys and the stored runs' keys, says which to play, how many to reuse and which to delete. It does no IO. `Cli.Suite` is the program over the store and the files that reads the compiled compositions and the engine's compiled output, keys every entry with the digest it is given, and plays the plan. `Main`, a composition root, passes `Adapters.Digest.sha256`.

**Builders for economic objects.** A composition organized by economic object builds each object's Premise from a builder over a configuration record, and composes their parts: for Pacific, `Premises.Pacific.Regime` (`regime`, over `RegimeConfig` from `configOf`) makes both national regimes, `Premises.Pacific.Clearing` the clearing system, `Premises.Pacific.Country` holds each country's names, `Premises.Pacific.Parts` joins what each object contributes, and `Premises.Pacific.Dials` holds every number. The views stay generic: `Views.Indicators` (substitute gap, net external position, domestic absorption, debt service, viability margin), `Views.Regimes` (each country's capability, demand and finance) and `Views.Clearing` (goods, capital and Rules across borders) read a country as an Actor group and a good as a Resource no Exchange quotes, so none names Pacific. The border accounts are in `Laws.External`, which INV-38 checks.

**The views describe their own columns.** The dashboard's glossary is built from the columns the views use, and the compiler writes the same list for the dashboard tests, so neither can drift from the views.

**Clients come from the operations.** `ApiRequest` is the list of operations. The endpoint table has one row per constructor, so a missing route fails to compile. The server parses with that table and the browser client writes with it, so client and server cannot disagree about a path, a method or a body. `Handlers.Api.Contract` has one entry per constructor, stating its reply Shape and refusals, read by the server's `dispatch`, the client's `send`, and the OpenAPI documents for the external services ([07 §8.2](07_simulator.md#82-external-services)), which take their paths and methods from the table and their statuses by reflecting the entry's row of refusals. An entry's reply Shape is found from its reply type (`Views.Keys.ViewShape`); `Created` states a 201 in the Authored format. `Views.Keys.ViewReplies` does the same per View: one label per `View` constructor, read by the server's `answerView` and the client's `viewKey`. Only an open route can be published (`Access`).

**A balance sheet is scoped by construction.** `Views.BalanceSheet` is a pure projection of a stored Run for one Actor, and names another Actor only where this one was party to a Transaction with it ([07 §9.4](07_simulator.md#94-balancesheet-pages)).
