# Architecture Guidelines

**Version:** 0.5  
**Scope:** Guidelines for building the system: implementation principles, package responsibilities, dependency rules and their checks, the seams between packages, and when to add a package or module  
**Implementation language:** PureScript

---

## Terms

The implementation terms of this doc. The type-system terms are in the [Type System glossary](02_type_system.md#25-glossary).

| Term | Definition |
|---|---|
| **Layer** | One spec's share of the design: the Type System ([02](02_type_system.md)), Gameboard ([03](03_gameboard.md)), Instantiated Game ([04](04_instantiated_game.md)), Simulator and Dashboard ([06](06_simulator.md)), or this Architecture doc. |
| **Round runner** | The effectful code that runs Propose and Sign for every Policy, then calls the pure `resolveRound` ([§2](#2-runtime-shape)). |
| **Kernel** | The modules under `packages/core/src/Core/Kernel`: the types of [02 §3](02_type_system.md#3-the-kernel), posting, resolution, consumption, laws, rationing, clearing and routing. |
| **Settlement sequence** | The per-mechanism order of `resolveRound` in `packages/rules`, which calls the kernel in stated phases ([§4](#4-key-interfaces)). |
| **Compile step** | The code in `packages/rules` that writes each settled mechanism as kernel values, with the static refusals decided first ([03 §14](03_gameboard.md#14-compilation-to-the-kernel)). |
| **Shape** | A typed description of a type that crosses a described boundary. One Shape gives the type's codec for each JSON format and its prose. |

## 1. Principles

These govern every package. They are what a change is checked against.

- **Pure core, effects at the edge.** The engine is a pure function ([INV-36](02_type_system.md#inv-36)). Impure work is an algebraic effect ([02 §8.4](02_type_system.md#84-effects-and-handlers)) interpreted by a handler chosen at the entry point.
- **Three layers for impure work.** Each concern has one home.
  - **Adapters expose devices.** An adapter wraps one device (the filesystem, SQLite, a process, the clock, the OS seed) as raw primitives: text in, text out, or one statement. It knows nothing of games, runs, prompts or settings.
  - **Effects declare the program's interface to a device.** An effect module declares the operations a program may request, with their typed requests and replies. It contains no implementation. Each program's type lists the rows it may request, so a read program cannot write. For example, `OBSERVE` is the only row a Run Store read has.
  - **Handlers hold the program logic.** A handler interprets an effect's operations over a device primitive. Prompt building, reply decoding, fallbacks, run lifecycle and recording belong here. Handlers never import adapters; the composition root wires the two.
  - A new device is a new adapter. A new program need is a new operation with its handler. Domain rules are in none of the three: they stay in the domain packages.
- **One-way dependencies.** A package depends only on the packages in its row of the table in [§3](#3-codebase-layout). Packages at the same level never import each other.
- **Narrow seams.** Packages meet at the small interfaces in [§4](#4-key-interfaces). Code behind an interface can change without touching its callers. Replacing an adapter (the SQLite store, the model provider) changes nothing in the domain packages, effects or handlers: the new adapter implements the same primitives.
- **Each described type is described once.** A type that crosses a boundary has one Shape, and its codecs and prose are derived from it. Hand-written instances, projections and name lists that repeat a description are defects.
- **Interfaces are derived; behaviour is hand-written.** Codecs, action lists, routes and decoders come from declarations. The Strategy interpreter, agent framing, human facilitation, view projections and API semantics are written by hand.
- **Economics live in one place.** Only the settlement sequence in `rules`, over the kernel in `core`, changes the economy. Every leg posts through `Core.Kernel.Post`. Apps compose and hold no economic logic.
- **Settings are data.** Gameboards, instantiation configurations, fixtures and presets are authored in a typed DSL and compiled to data. The engine reads only the compiled data. Fixture expected values are literals, never computed. The formats are owned by [02](02_type_system.md) (the snapshot format) and [03](03_gameboard.md) (the setting format).
- **Small.** One concept per module. No frameworks inside the core.

## 2. Runtime Shape

The runtime is one pure engine and one round runner that calls it. A setting is assembled once into a `Game`, and every runner plays that same `Game`: the simulator folds it with the player seed, and live play runs it with human and agent seats.

```text
   setting ──► assemble ──► Game
                              │
          ┌───────────────────┴───────────────────┐
   simulate (pure fold)                     runLive (Aff)
          └──────────── runRound (effectful) ─────┘
           seat input; pure resolveRound inside
                              │ record, store
                              ▼
   Run Store ◄── server ◄── API ◄── dashboard
```

Each round is one `runRound` call. Seats propose and sign through the seat-input effect. The pure engine resolves the signed Proposals and never authors or rewrites an Intent. Handlers choose where seat input comes from and where records go. The round rules are in [02](02_type_system.md) and [04](04_instantiated_game.md). The Run Store, API and dashboard are in [06](06_simulator.md).

## 3. Codebase Layout

The project is a spago workspace. Each package is named `sp-<dir>` and compiles on its own. The table is the rule a builder follows: a package owns what its row says, must not own what its row excludes, and imports only from its row's dependencies. Modules are listed by `find packages apps -name '*.purs'`; they are not repeated here.

| Package | May depend on | Owns | Must not own |
|---|---|---|---|
| `core` | nothing | The kernel; the description algebra; numerics, draws, the cost curve, and the id, quantity and curve types | Any authored vocabulary (Intent, Proposal, Rule, Transformation and the like); setting knowledge, authoring formats, operations, runs, a second codec for a described type |
| `rules` | `core` | The authored vocabulary and the settlement sequence: World State and Events, Intent and Proposal types, the Strategy language, the compile step, `resolveRound`, authority, Representation and per-round invariants | Setting knowledge, authoring formats, operations, runs |
| `schema` | `core`, `rules` | The setting format, the authoring DSL (including `Schema.Build.Agreement`), the run vocabulary, and the joint pass's values | Realization logic, validation errors, operations, behaviour |
| `board` | `core`, `rules`, `schema` | World realization and validation, and their errors. It is separate from `game` so that the package boundary keeps what the world is apart from how seats decide | JSON, fixtures, seat bindings, policy concepts |
| `policy` | `core`, `rules`, `schema` | The pure Strategy interpreter | Effect libraries, codecs, World State access |
| `game` | `core`, `rules`, `schema`, `board`, `policy` | Assembling a setting into a `Game`; seat bindings, overrides and stored run specs. The only domain package where `board` and `policy` meet | Effects, round execution, persistence, the player seed, setting content |
| `settings` | `core`, `rules`, `schema`, `game` | Authored settings, presets, fixtures and the setting file tree, as typed values ([03 §12a](03_gameboard.md#12a-how-settings-are-made)) | IO, compiling, decoders, computed expected values |
| `checks` | `core`, `rules`, `schema` | Checking expectations, runs and objectives against records | Effects, running games |
| `effects` | `core`, `rules`, `schema`, `board`, `policy`, `game` | The operation rows and the programs over them | Interpreters, handler records, domain rules, prompt text |
| `handlers` | `core`, `rules`, `schema`, `board`, `policy`, `game`, `effects` | The interpreters of the operations: seats, live play, model calls, the Run Store, recording, replay, the API's routing and client, and the pure simulation fold | Adapter imports; `views`, `experiments` or `settings` imports; the API's operation semantics; a browser-facing client importing a Node module |
| `adapters` | `handlers` (the device types of the SQL handler only) | Raw devices, and every FFI module outside the apps | Games, runs, prompts, settings, any handler function |
| `views` | `core`, `rules`, `schema`, `checks` | Pure projections of a stored run to generic tables, charts and statistics | Effects; `handlers`, `game`, `experiments` or `settings` imports; objective evaluation of their own |
| `experiments` | `core`, `rules`, `schema`, `checks`, `views`, `game`, `handlers` (the pure simulation fold only) | Counterfactuals, agency and seed batches, over the pure fold | Effects; persistence; `board` or `policy` imports |
| `testkit` | `core`, `rules`, `schema`, `settings`, `game`, `adapters`, `handlers`, `checks`, `views`, `effects` | The shared fixture set and capstone run for test suites | Anything a `src` module imports |
| `flags` | nothing | The command-line flag parser that the entry points read their arguments with | Any workspace or effect import |
| `apps/cli`, `apps/server`, `apps/dashboard` | anything | Programs and their composition roots. `cli` runs games and compiles settings; `server` serves and implements the API; `dashboard` renders view models and calls the API | Instantiation, seat binding, economic logic; path strings and decoders in the dashboard |

Two deliberate exceptions to the layering. `testkit` may import `adapters`, because it reads the setting files through the filesystem device. `handlers` does not import `checks`, so a handler never reaches the checkers.

**Where a new concept goes.** A concept that fits a package's row becomes a module in that package, named `<Package>.<Concept>`. A new package is added only when the build must enforce a boundary that module rules cannot: a concept that must not see something another package sees. A new package also needs a row in this table. A new device is a new adapter, and a new program need is an operation with its handler ([§1](#1-principles)).

Compiled setting files are gitignored build output, generated by the compiler and never hand-edited.

## 4. Key Interfaces

These are the seams. Every other piece is an internal detail of a package. Signatures are as the code declares them.

**Setting and Game** (`game`). A setting is assembled once, purely, into the `Game` every runner plays. This is the only place a Gameboard is instantiated and its seats are bound:

```purescript
assemble :: Setting -> Bindings -> Array Override -> Either GameError Game
-- Bindings = Map EntityId SeatBinding
-- SeatBinding = Strategy | Script (Array ScriptRound) | Agent | Human
```

**Instantiation and validation** (`board`). Pure and atomic: it returns the initial world or every error.

```purescript
instantiate :: Gameboard -> InstantiationConfig -> Either (Array ValidationError) InitialWorld
```

**Engine** (`rules`, `Rules.Engine.Commit`). Pure, and the only operation that changes the economy. It is the settlement sequence, and there is deliberately no single kernel call for a whole round ([02 §3.5](02_type_system.md#35-resolution)):

```purescript
resolveRound :: WorldState -> Array SignedProposal -> Either EngineError RoundResult
-- RoundResult = { world :: WorldState, record :: RoundRecord }
```

**Kernel** (`core`). Resolution runs a set of couplings through the removal procedure, and `Post` is the only writer of the ledger. The compile step (`Rules.Engine.Compile`) writes each mechanism as kernel values and calls them:

```purescript
resolve :: Kernel cap query -> Array (Coupling cap law query reason cause) -> Either ResolveError (Resolved law reason)
post    :: Opening -> Short -> { from :: Endpoint, to :: Endpoint, resource :: ResourceId, owed :: Q } -> Ledger -> Posted
spawned :: RoundIndex -> Map String (Template cap law query reason cause) -> Array (Coupling cap law query reason cause) -> Array (Consumed law reason) -> Either ConsumeError (Array (Pending cap law query reason cause))
```

**Round runner** (`effects`). Every Policy, seat or in-world, is evaluated through the same code in live play and in simulation, and then `resolveRound` is called. The round's effect row is the union of the rows it may request:

```purescript
runRound :: forall r. InWorld -> WorldState -> Run (SEAT_INPUT + INTERPRET + RECORD + NARRATE + r) (Either EngineError RoundResult)
```

Randomness needs no effect. Every draw is a pure function of the game seed or the player seed. The player seed is resolved once, before the call.

**Policy** (`policy`). A Strategy is pure and reduces to two functions over an environment. A seat, human or AI agent, answers the same two steps:

```purescript
propose :: Strategy -> Env -> Array ProposalBody
sign    :: Strategy -> Env -> Array ProposalId     -- chooses among the Proposals awaiting signature
```

The environment is the policy's `View` ([02 §4](02_type_system.md#4-policies)). A Policy never touches World State.

**Simulation** (`handlers`). The fold over a `Game`. It reads no entropy, and it is the only pure runner.

```purescript
simulate :: Seed -> Game -> Either GameError SimRun
```

**API** (`effects` row `API`, `handlers`, `apps/server`). Each HTTP route is one operation of the `API` row. A request is an `ApiRequest`, and a reply is a status and a body. The row and the route table are in [06 §8](06_simulator.md#8-persistence); the server's implementation of the operations is in `apps/server`, and the dashboard calls them through the browser-safe client.

```purescript
type API r = (apiCall :: ApiF | r)   -- every route: Call ApiRequest (Reply -> a)
```

**Run Store** (`effects` rows `RECORD`, `STORE`, `OBSERVE`). Round writes are `RECORD`, lifecycle writes are `STORE`, and reads are `OBSERVE`. The operations are in [02 §8.4](02_type_system.md#84-effects-and-handlers) and the store in [06 §8](06_simulator.md#8-persistence).

## 5. Enforcement

Checks run under `just check`, and each fails the build on a violation. Each check has a positive and a negative sample, so a rule can be shown to fire.

- **Dependency table.** Each package's imports stay in its row of [§3](#3-codebase-layout). Pure packages import no effect, async or Node library.
- **Concept rules.** Rules that imports cannot express: foreign code (`.js`) lives only in adapters and apps; `core` names no authored vocabulary; only `Core.Kernel.Post` writes the ledger or builds a settlement leg.
- **Impurity.** Only composition roots import adapters, and adapters use no domain words or domain imports.
- **Derived rule.** Flags hand-written projections, codecs, event matches and repeated names where one description should generate them. Each exception is listed with its reason.
- **Docs tables.** The marked tables in [05](05_test_instance.md) match the compiled setting.
- **Invariants.** Per-round checks are pure predicates shared by the tests, the simulator and live play. Which assertions exist is in [06 §7.1](06_simulator.md#71-required-assertions).

Tests live in each package's own test directory and in the apps' suites. The `core` suite covers the kernel only; the vocabulary's tests are in `packages/rules/test`. The mandatory tests are in [06 §14](06_simulator.md#14-mandatory-tests).

## How the docs relate

The spec docs (01 to 06) are the source of truth, and the implementation follows them. [AGENTS.md](../AGENTS.md) is the operator's guide: commands, and where each part of the implementation lives.
