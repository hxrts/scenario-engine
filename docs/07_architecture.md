# Scenario Planning Game — Technical Architecture

**Version:** 0.2  
**Scope:** Codebase structure, module boundaries, and key interfaces  
**Implementation language:** PureScript

---

## 1. Principles

- **Pure core, effects at the edge.** The engine is a pure function ([INV-36](02_type_system.md#inv-36)). Everything impure is an algebraic effect (Type System [§8.4](02_type_system.md#84-effects-and-handlers)) interpreted by a handler chosen at the entry point.
- **One-way dependencies.** Packages depend only downward ([§3](#3-codebase-layout)). Packages that sit side by side never import each other.
- **Narrow seams.** Packages meet at a few small interfaces ([§4](#4-key-interfaces)). Code behind an interface can be rewritten without touching its callers.
- **Data, not code, for scenarios.** Gameboards, instantiations, fixtures, and preset programs are declarative data decoded into core types.
- **Small.** One concept lives in one module. There are no frameworks inside the core.

## 2. Runtime Shape

```text
             entry point (session | sim | replay | test)
                         │ chooses handlers
                         ▼
   ┌─────────────── round runner (effectful) ───────────────┐
   │  prepare/observe ──► Propose ──► Sign ──► resolveRound │
   │      (pure)        SeatInput  SeatInput     (pure)     │
   └───────────────────────────────┬────────────────────────┘
                                   │ Record
                                   ▼
                     run store ──► dashboard / reports (Narrate)
```

Each round is one call to `runRound`. The engine sees only signed Proposals and returns the next World State plus events. It is an accountant: it checks authority and signatures and keeps the books balanced. It never authors or rewrites an Intent; Representation estimates are stored in World State, and Policies act on them next round. All other work happens in handlers.

## 3. Codebase Layout

The project is a spago workspace. Each package compiles on its own, and the dependency direction is enforced by each package's declared dependencies.

```text
packages/
  core/        pure types and engine; depends only on prelude-level libs
    Num          fixed-point quantities (4 dp, half-even)
    Types        Entity, Capability, Resource, Intent, Rule, Proposal, View, Market, CollectiveProcedure, Representation, …
    World        WorldState, View, Event, round records, Manifest
    Draw         keyed PRNG: draw :: Seed → DrawKey → UnitInterval
    Market       CostFunction, clearing
    Transform    facilities, recipes, circulation
    Represent    estimators: sampled behavior → stored adjustment factor
    Procedure    collective procedures: weighted support for procedure Proposals, checked at screening
    Engine       prepare, observe, screen, estimate, resolve, commit
    Invariants   INV checks as pure predicates over (World, RoundResult)
  board/       Gameboard schema (incl. Objectives), validation, instantiation (pure; → core)
  policy/      decision-language interpreter and presets (pure; → core)
  effects/     effect signatures and runRound (Run; → core, policy)
  scenarios/   data plus its loader: fractured-world/v0/*.json, presets.json, fixtures (→ board, policy)
  handlers/    concrete interpreters, one module per handler (→ effects)
    Live         SeatInput via Interpret, Clock deadlines
    Sim          SeatInput via bound seat Policies, player-seed stream
    Replay       SeatInput from recorded round records
    Test         in-memory, fully pure
  adapters/    impure backends behind effect handlers (→ handlers)
    Llm          Interpret, Narrate, AI-agent seats
    Store        Record / RunStore on SQLite
  harness/     SimulationSpec, experiments, counterfactuals, objective evaluation (→ effects, handlers)
  apps/        entry points only (→ anything above)
    cli          live session and simulation commands
    server       read-only JSON API over the run store
    dashboard    PureScript React + shadcn/ui front end
test/          property tests per package; fixtures A–G as golden tests
```

Dependency order, read bottom to top: `core ← board, policy ← effects ← handlers ← adapters, harness ← apps`. `scenarios` is data plus a loader that decodes it through `board` and `policy`.

## 4. Key Interfaces

These are the seams. Every other piece is an internal detail of a package.

**Engine** (`core/Engine`). This is pure and is the only operation that mutates the economy:

```purescript
resolveRound :: WorldState -> Array SignedProposal
             -> Either EngineError RoundResult   -- next World, events, records
```

**Instantiation** (`board`). This is pure:

```purescript
validateGameboard :: Gameboard -> Either (Array ValidationError) ValidGameboard
instantiate       :: ValidGameboard -> InstantiationConfig
                  -> Either (Array InitializationError) InitialWorld
```

**Policy** (`policy`). Every Policy kind reduces to this shape, and it is pure for programs:

```purescript
type PolicyStep =
  { propose :: View -> Array Proposal
  , sign    :: View -> Array ProposalId }   -- chooses among view.toSign
```

`View` (Type System [§4](02_type_system.md#4-policies)) is everything a Policy sees; in the Sign step its `toSign` field carries the Proposals awaiting that Policy's signature. AI agents and humans satisfy the same shape through the `SeatInput` effect rather than a pure function.

**Authorization** (`core`). One matching function over closed types (Type System [§2.4](02_type_system.md#24-capability)):

```purescript
authorized :: Array Capability -> EntityId -> Intent -> Boolean
```

**Objectives** (`harness`). Evaluation is a pure view over the records and never touches the engine (Gameboard [§10.5](03_gameboard.md#105-evaluation)):

```purescript
evaluateObjectives :: Manifest -> Array RoundRecord -> Map EntityId (Array CriterionResult)
```

**Effects** (`effects`). These are the only impure vocabulary in the system:

```purescript
type RoundEffects r = (SEAT_INPUT + RECORD + r)
runRound :: Policies -> WorldState
         -> Run (RoundEffects r) (Either EngineError RoundResult)
```

Randomness needs no effect: every draw is the pure `draw` on the game seed (in-world Policies, estimator noise) or the player seed (seat Policies, supplied by the handler). `INTERPRET`, `NARRATE`, and `CLOCK` are added only by the live and reporting stacks.

**Run store** (`adapters/Store`). It is append-only and reached through the `Record` effect plus a read API:

```purescript
type RunStoreRead m =
  { manifest :: RunId -> m Manifest
  , rounds   :: RunId -> m (Array RoundRecord)
  , metric   :: RunId -> MetricId -> m (Array Point) }
```

**Data formats.** Gameboards, instantiation configs, seat-policy presets, manifests, and round records are JSON. Each has a codec in the package that owns its type. Records are the replay boundary: manifest + round records + seeds reproduce a run ([INV-13](02_type_system.md#inv-13), [INV-31](02_type_system.md#inv-31)).

## 5. Coupling Rules

- `core` imports no effect library and no IO. JSON codecs are the `EncodeJson`/`DecodeJson` instances declared beside each type (PureScript forbids orphan instances, so they cannot live in a separate module); the engine never calls them.
- Policies never touch World State directly. They receive a `View` and return Proposals and signatures. The engine never authors or rewrites Intents.
- Handlers translate effects only. They contain no game logic, and economic rules live only in `core`.
- Adapters are replaceable. The SQLite store, the LLM provider, and the dashboard can each be swapped without changing anything above `handlers`.
- Scenarios change by editing data. Needing new engine code for a scenario is a design finding to resolve in `core` (Reference Instance [§1](05_test_instance.md#1-purpose)).
- Every invariant has one pure predicate in `core/Invariants`. Tests, the harness, and live sessions share it.

## 6. Testing

- **Unit and property tests** run per package against pure functions: conservation ([INV-04](02_type_system.md#inv-04), [INV-05](02_type_system.md#inv-05)), order independence ([INV-11](02_type_system.md#inv-11)), and resolution consistency ([INV-14](02_type_system.md#inv-14)).
- **Golden tests:** fixtures A–G are run through `resolveRound` and compared to their expected balances.
- **Replay tests:** recorded rounds re-run under the `Replay` handler must give identical results.
- **Interpretation tests:** these run against the `Llm` adapter only, separate from economic tests (Simulator [§7.4](06_simulator.md#74-ai-interpretation-tests)).

## 7. Build Order

1. `core`: Num, Types, Draw, Market, Engine, with fixtures A, F, and G as golden tests.
2. `board` and `scenarios`: load and instantiate The Fractured World.
3. `policy`, `effects`, `handlers/Test`: full rounds with preset programs.
4. `handlers/Sim`, `harness`, `adapters/Store`: saved runs, seeds, and counterfactuals.
5. `adapters/Llm`, `handlers/Live`, `apps`: live sessions, AI seats, the dashboard.

Each step produces something runnable and tested before the next one starts.
