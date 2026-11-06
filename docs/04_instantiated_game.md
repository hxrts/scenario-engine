# Scenario Planning Game — Instantiated Game Specification

**Version:** 0.2  
**Scope:** Instantiated Game Layer  
**Implementation language:** PureScript

---

## 1. Purpose

The Instantiated Game Layer constructs, executes, and records a particular playthrough of a Gameboard.

An Instantiated Game is a particular **resolution** of the Gameboard's world. The resolution decides which parts of that world are explicit Actors and which remain implicit Aggregates. The layer also fixes:

- controllers;
- initialized Policies;
- holdings and capacities;
- market conditions.

The same Gameboard supports many games without being modified. The EU might be human-controlled in one game, an NPC in another, and implicit in a third. A small-business owner might be a representative Actor in one game and absent from another.

**The primary responsibility of this layer is to transform a configurable scenario into a complete, internally consistent, executable economic world, without changing that world's economic meaning ([INV-14](02_type_system.md#inv-14)).**

Layer responsibilities, terminology, invariants, and the round sequence are defined in the Type System specification ([§2.5](02_type_system.md#25-glossary) Glossary, [§8.1](02_type_system.md#81-canonical-resolution-sequence) round sequence, [§9](02_type_system.md#9-architectural-invariants-and-numerics) invariants). This document uses them without restating them.

Everything (controllers, fallbacks, Policies, Markets, holdings, capacities, recurring flows, Representations) is resolved before play. Whether an Actor is a seat or an NPC never changes its economic semantics; the engine sees only signed Proposals. A game replays from its manifest and round records ([INV-13](02_type_system.md#inv-13), [INV-15](02_type_system.md#inv-15)).

The layer performs two separate operations:

**Validated Gameboard + Instantiation Configuration → Initial World State**

**World State + Round Inputs → New World State** (Type System [§8.1](02_type_system.md#81-canonical-resolution-sequence))

## 2. Instantiation Configuration

An Instantiation Configuration is declarative. It references a validated Gameboard and supplies the choices that the Gameboard leaves configurable.

| Domain | Responsibility |
|---|---|
| **Seats** | Seat slots and, in live play, the humans occupying them. |
| **Actor Selection** | Which Actor definitions become explicit. |
| **Controller Assignments** | Seat or NPC for each explicit Actor. |
| **Fallbacks** | The fallback applied to each unselected Actor definition. |
| **Aggregate Selection** | Which populations are represented implicitly. |
| **Representation Assignments** | Which Actors feed which Aggregates, and with which estimator. |
| **Policy Parameters** | Concrete parameters for NPC and Aggregate Policies. |
| **Economic Initialization** | Holdings, capacities, recurring flows, and market conditions. |
| **Round Schedule** | Rounds and their simulated time intervals. |
| **Execution Parameters** | Seeds and other permitted settings. |

```purescript
type InstantiationConfig =
  { seats :: Array SeatBinding
  , actors :: Array ActorAssignment
  , fallbacks :: Array FallbackSelection
  , aggregates :: Array AggregateAssignment
  , representations :: Array RepresentationAssignment
  , parameters :: ParameterSet
  , initialConditions :: InitialConditions
  , schedule :: RoundSchedule
  , seed :: Seed
  }
```

The Gameboard determines which choices are eligible. An invalid configuration is rejected before any economic state is created.

## 3. Actors, Controllers, and Fallbacks

An Actor is an economic Entity with holdings, capabilities, and institutional relationships. A seat's Policy supplies its decisions; in live play that Policy is a human player.

### 3.1 Selection and control

The configuration selects which Actor definitions become explicit. Each explicit Actor receives exactly one controller ([INV-23](02_type_system.md#inv-23)): the Policy that drives it.

```purescript
data ControllerAssignment
  = Seat SeatId        -- Policy supplied from outside the scenario
  | NPC PolicyId       -- in-world Policy from the Gameboard catalog

type ActorAssignment =
  { definition :: EntityDefinitionId
  , controller :: ControllerAssignment
  }
```

A **seat**'s Policy (human, AI agent, or preset program) is bound at run time and recorded in the manifest and round records. Swapping it never changes the Actor's definition or turns the seat into an NPC ([INV-30](02_type_system.md#inv-30)). By default, each human participant occupies one seat.

An NPC is an explicit Actor whose Policy is part of the scenario, not a separate entity type. Initialization resolves the NPC's:

- economic state;
- capabilities;
- Policy and parameters;
- observation view;
- seed, where one is declared.

### 3.2 Fallback resolution

Every eligible Actor definition declares the fallbacks it permits when it is not made explicit:

```purescript
data Fallback
  = ImplicitCoverage AggregateDefinitionId  -- accounts and activity retained in an Aggregate
  | InstitutionalPolicy PolicyId            -- institution's behavior exercised by an implicit Policy or rule mechanism
  | PermittedOmission                       -- activity the Gameboard allows to be absent

type FallbackSelection =
  { definition :: EntityDefinitionId
  , fallback :: Fallback
  }
```

The alternatives are not interchangeable. A government institution is not a statistical population: its capabilities stay with the institutional Entity or rule mechanism and are never handed to a household or business Aggregate ([INV-03](02_type_system.md#inv-03)).

The configuration selects one permitted fallback per unselected definition, or accepts the Gameboard default. Initialization fails if any required activity, liability, or authority is left without an owner ([INV-25](02_type_system.md#inv-25)).

## 4. Aggregate Initialization

The Gameboard defines potential Aggregates. The configuration selects and parameterizes them.

### 4.1 Resolving the three projections

Initialization resolves each Aggregate's membership, residual, and collective procedure separately (Type System [§2.2](02_type_system.md#22-three-projections-of-an-entity)):

1. **Membership** may overlap across Aggregates and may include explicit Actors.
2. **Residual accounts.** Every account and behavioral responsibility gets exactly one authoritative owner ([INV-08](02_type_system.md#inv-08)).
3. **Collective procedure and capabilities.** These are attached where applicable.

Each Aggregate also receives a baseline Policy, permitted observations, and any Representation assignments.

### 4.2 Partitioning

**Total population = Explicit members + Residual**

The same partition applies to holdings, liabilities, capacities, and directly modeled behavior. A business may be classified as both manufacturing and small business, but two Aggregates cannot both own its assets.

Extraction covers only the accounts the explicit Actor actually controls (Type System [§7.1](02_type_system.md#71-aggregate-semantics)). Germany remains an EU member and submits procedure inputs to the EU's collective procedure.

Population resolution proceeds in five steps:

1. Identify explicit members.
2. Subtract their accounts and activity from the residual.
3. Set residual sizes and weights.
4. Reconcile overlapping classifications.
5. Initialize baseline behavior from the residual state.

If the partition cannot be made consistent, initialization fails.

### 4.3 Resolution consistency

Two instantiations that differ only in explicitness must describe the same initial economy ([INV-14](02_type_system.md#inv-14)).

For example, in Game A German government activity is implicit within the European configuration, and in Game B the Chancellor is explicit. Before any decisions are made, the two games' totals must reconcile:

- stocks;
- liabilities;
- capacities;
- authority.

Later trajectories may legitimately diverge because players decide differently. They must never diverge because extraction duplicated resources, deleted liabilities, or changed rules.

Initialization checks the resolved partition against the Gameboard's reference (fully implicit) accounting.

## 5. Representation Assignment

The Gameboard defines Representation schemes. The configuration activates them and resolves their estimator parameters.

```purescript
type RepresentationAssignment =
  { actor :: EntityId
  , scheme :: SchemeId
  , estimator :: Estimator   -- Type System §7.2; usually a preset with overrides
  }
```

The scheme fixes the target Aggregate, the domain, and the permitted preset. The assignment resolves the parameters that depend on the actual participants:

- weights;
- shrinkage;
- deviation bound;
- delay;
- backing requirement;
- the linked collective procedure, where one applies.

Under the `constituent` preset, inputs typically arrive as procedure inputs and require no transfer. Membership is resolved independently of Representation: Germany is an EU member whether or not it submits a procedure input in a given round.

**Direct and representative effects stay separate ([INV-09](02_type_system.md#inv-09)).** The Actor's own transactions execute once against its own holdings. The Actor's validated proposals and procedure inputs feed the estimator, which adjusts the residual. The engine records the four stages: observation, estimate, proposed Aggregate Intent, and executed result (Type System [§7.3](02_type_system.md#73-representation-pipeline)).

With multiple representatives, the estimator combines inputs under the resolved weights. It never multiplies a transaction by the population size.

If an Aggregate has no representative, it follows its baseline Policy.

Initialization verifies that:

- the participants exist and are eligible;
- the domain is permitted;
- the estimator parameters are valid for the preset;
- bounds are defined where the preset requires them;
- assignments are compatible;
- nothing is counted twice ([INV-24](02_type_system.md#inv-24)).

Representation never confers the Aggregate's holdings or capabilities ([INV-03](02_type_system.md#inv-03)).

## 6. Policy Initialization

Each NPC and Aggregate receives a concrete Policy instance ([INV-26](02_type_system.md#inv-26)), consisting of:

- a definition;
- the Entity it governs;
- parameters;
- an observation view;
- triggers;
- a seed, where one is declared.

A definition may be reused with different parameters.

Policies are versioned decision programs run by the shared interpreter (Type System [§4](02_type_system.md#4-policies)). Any memory a Policy keeps between rounds, such as whether a rival deployed Military last year, lives in World State and is initialized explicitly.

Programs are fixed for the game. Behavior still changes as rules, relationships, and observations change. A `ChangeRule` alters World State rules and never rewrites a Policy.

Observation views apply the Gameboard's information permissions to each instance. NPCs see no private holdings or private player negotiations merely because the engine holds them ([INV-16](02_type_system.md#inv-16)).

Baseline behavior is reconciled with representative inputs exactly once per round, in the reconciliation phase.

## 7. Economic Initialization

### 7.1 Holdings, Markets, and capacities

Initial holdings are allocated among Actors, Aggregates, Market reserves, and other permitted accounts. Allocations respect resource definitions, ownership constraints, liabilities, and the account partition. Actors may begin highly unequal; equality is never imposed.

Each Market receives its initial state:

- cost-function parameters;
- position;
- reserves;
- derived prices;
- participation rules;
- access and tariff rules.

A Market must be feasible at the start and cannot promise settlement against resources it does not hold ([INV-06](02_type_system.md#inv-06), [INV-27](02_type_system.md#inv-27)).

Capacities that are economic state rather than Resources are initialized alongside holdings:

- production capacity;
- transport capacity;
- market access;
- tariff schedules.

Cross-vertical dependencies are resolved at this point. No Entity begins with capacity or transformation rights outside the allowed configuration.

### 7.2 Recurring flows

Initialization establishes the recurring cycle (Type System [§6](02_type_system.md#6-economic-circulation)), whose components are computed together from the snapshot:

- production from capacity;
- exchange, which delivers Money to suppliers;
- income and expenditure (revenue, wages, public spending, taxation);
- capacity renewal from investment.

Every Facility has a declared payee. Initialization checks that these flows reconcile ([INV-05](02_type_system.md#inv-05)) and flags any configuration in which resources can only be depleted.

### 7.3 Initial conditions

The Gameboard may supply defaults and ranges. The configuration resolves them, either explicitly or through a deterministic procedure.

Mechanics are deterministic by default. Randomness is used only where the Gameboard or an estimator declares it. The configuration's single `seed` is the game seed: every in-world random value, including Representation sampling noise, is a keyed draw from it (Type System [§9.1](02_type_system.md#91-numerics-and-randomness)). Live play has no player seed. Instantiation resolves each estimator's noise spread within the Gameboard's permitted range. Once recorded, initial conditions are never resampled.

## 8. Time and Session

The Round Schedule maps rounds to intervals of simulated time, typically five years over four to eight rounds. Intervals need not be equal.

Policies, transformations, deliveries, and institutional effects share one time model, so annual rates are scaled to each round's duration ([INV-28](02_type_system.md#inv-28)).

The real-time session (about six hours, with negotiation, announcements, meals, and reporting) controls only when submissions are accepted. It does not determine outcomes.

Each round has two boundaries in live play: a **propose deadline** (end of the announcement window), when Proposals freeze, and a **sign deadline** (end of the signing window), when signatures freeze. Both steps read the round's snapshot.

## 9. Runtime State and Lifecycle

The runtime holds:

- the Gameboard reference;
- the resolved configuration;
- the authoritative World State, including reservations;
- the current round;
- the round history.

```purescript
type GameInstance =
  { id :: GameId
  , gameboard :: GameboardRef
  , configuration :: ResolvedConfig
  , world :: WorldState
  , nextRound :: RoundIndex
  , history :: Array RoundRecord
  }
```

At runtime the game is the round runner under the live-session handler stack (Type System [§8.4](02_type_system.md#84-effects-and-handlers)): humans supply seat input through the interpreter, and recording, narration, and deadlines are effects. The **manifest** together with the append-only **round records** is the source of truth ([INV-31](02_type_system.md#inv-31)). Snapshots are checkpoints derivable from them, and metrics are reproducible projections.

**Lifecycle:** Draft → Validated → Initialized (initial World State frozen) → In Progress → Completed → Archived. Ordinary play is not permitted before initialization succeeds.

**The manifest** records:

- the Gameboard identity and version;
- Actor, controller, and fallback resolutions;
- memberships and account partitions;
- Policy definitions and parameters;
- Representation assignments and estimators;
- initial holdings, capacities, recurring flows, market positions, and rules;
- the round schedule;
- the seed and engine version.

Settings change during play only through supported mechanisms, never by editing the manifest.

## 10. Initialization Pipeline

Instantiation is a staged, atomic compilation:

1. **Resolve Gameboard.** Load it, check the version, and confirm every configuration reference.
2. **Resolve Actors and controllers.** Check participation and controller compatibility, and apply fallbacks.
3. **Resolve implicit economy.** Create the Aggregates and the implicit institutional machinery.
4. **Partition accounts.** Assign a single owner to each account and responsibility while preserving overlapping membership.
5. **Initialize Policies.** Set parameters, views, triggers, and seeds.
6. **Initialize Representations.** Resolve estimators and any collective procedure links.
7. **Initialize Markets, Facilities, and resources.** Set holdings, capacities, recurring flows, and rules, then check feasibility and stock-flow reconciliation.
8. **Initialize time and execution.** Set the schedule and the deterministic parameters.
9. **Validate the complete world.** Check every Instantiation invariant, including resolution consistency.
10. **Commit.** Produce the initial snapshot, the resolved configuration, and the manifest.

```purescript
instantiate
  :: ValidGameboard
  -> InstantiationConfig
  -> Either (Array InitializationError) InitialWorld
```

A failed compilation returns diagnostics and leaves no partially initialized game.

## 11. Round Execution

Each round follows the canonical sequence in Type System [§8.1](02_type_system.md#81-canonical-resolution-sequence). The engine runs phases 1–2 and 4–7; the **round runner** runs phase 3 (Propose and Sign) for every Policy at once (seats, NPC Policies, and Aggregate baselines) and passes the signed Proposals to the engine. This layer supplies the session procedures, inputs, and records around that sequence.

**Preparation and briefing.** The preparation phase runs first: deliveries arrive, scheduled shocks apply, and rule changes take effect. Each Player then receives an authorized view of the snapshot, covering:

- public conditions;
- their own holdings, capacities, and capabilities;
- active reservations, such as deployed Military;
- permitted private information;
- prior results.

The briefing is derived from the authoritative records and is never an independent source of facts.

**Negotiation.** Players negotiate out of band. A deal reaches the engine as a Proposal that names every party as a signer. One party publishes it before the propose deadline, and the others sign it before the sign deadline (Type System [§3.3](02_type_system.md#33-proposals-and-signatures)). A Proposal that is not fully signed lapses. Counteroffers are new Proposals.

**Submission and AI interpretation.** AI converts natural-language submissions into Proposals and signatures, including multi-signer deals and procedure inputs, and preserves provenance. Materially ambiguous interpretations must be confirmed by the player before the relevant deadline. The model never assigns resources or capabilities the Actor lacks. Only accepted canonical Proposals and signatures are frozen, and these are retained for replay. Interpretation is tested separately from economic resolution.

**Resolution.** NPC and Aggregate Policies propose and sign in the same steps as seats. The engine then screens (authority and signature completeness), reconciles, and resolves the economy, including the recurring cycle. Conflicts and infeasible actions follow the Type System's deterministic rules:

- limit prices and maximum spend;
- pro-rata allocation with ties broken by EntityId;
- atomic settlement, with each fully signed Proposal settling or failing as a unit ([INV-10](02_type_system.md#inv-10));
- never ordering by submission time ([INV-11](02_type_system.md#inv-11)).

Rejected, partially filled, and lapsed actions are recorded with their reasons. Prices produced in a round become observable in the next round's snapshot.

**Reporting.** A fictional financial-news front page and per-Actor balance sheets are produced, both derived from resolved state and events. Balance sheets separate accounting quantities from valuations and projections. Represented Aggregate effects are never reported as the representative's assets. Narration explains outcomes and never adds authoritative facts.

**Round record.** The engine appends a record containing:

- the starting snapshot reference;
- the accepted canonical inputs;
- Policy outputs;
- Representation observations, estimates, proposed Intents, and executed results;
- Proposals, their signatures, lapses, and settlement;
- validation and clearing outcomes, including proposed versus executed actions;
- resource, capacity, and Market changes;
- the ending snapshot reference.

Language-model interpretation may be nondeterministic. Replay therefore starts from the recorded canonical Proposals and signatures. Audio and transcripts may be kept as supporting evidence.

All participants act on one World State ([INV-15](02_type_system.md#inv-15)). Alternative outcomes require separate instances or deliberate replay branches.

## 12. Validation

A valid Instantiated Game satisfies every invariant in the Type System master table. The instantiation-tagged invariants are checked at initialization:

| ID | Checked as |
|---|---|
| [INV-03](02_type_system.md#inv-03) | Making an Entity implicit neither grants nor removes institutional authority. |
| [INV-05](02_type_system.md#inv-05) | Recurring flows reconcile, and every Facility has a payee. |
| [INV-08](02_type_system.md#inv-08) | Each account and responsibility has one owner, and residuals exclude explicit accounts. |
| [INV-14](02_type_system.md#inv-14) | The initial accounts, liabilities, and authority match the Gameboard's reference accounting. |
| [INV-15](02_type_system.md#inv-15), [INV-16](02_type_system.md#inv-16) | There is one World State, and each Policy sees only its Actor's permitted view. |
| [INV-23](02_type_system.md#inv-23) – [INV-28](02_type_system.md#inv-28) | Unique control, valid participation (including Representation), fallback completeness, Policy completeness, market feasibility, and temporal consistency. |

Initialization fails if any check fails. A configuration is never silently repaired by changing an Actor's meaning, assigning an unrelated Policy, or inventing resources.

## 13. Illustrative Instantiation

**These are illustrative choices, not decisions about the final game.**

Assume a Gameboard about US–China industrial competition over five years. It has semiconductor, energy, and manufacturing Verticals, government, corporate, and household Entities, and Representation schemes for selected populations.

| Actor | Controller | Participation |
|---|---|---|
| US Government | Seat (human) | Explicit, direct |
| Chinese Government | Seat (human) | Explicit, direct |
| EU Commission | NPC | Explicit; exercises the EU collective procedure |
| US Small-Business Owner | Seat (human) | Explicit, representative |
| Manufacturing Corporation | Seat (human) | Explicit, direct |

| Aggregate | Policy | Representation |
|---|---|---|
| US Small Businesses | Investment and production | `statistical` preset, fed by the small-business Actor |
| US Households | Consumption | None |
| Industrial Producers | Production and investment | None |

The US Small Businesses residual excludes the owner's own accounts.

**One round.** Having agreed terms out of band, the corporation publishes a Proposal that transfers Energy to the US government and Money from the US to the corporation, with both as signers. The US signs it. The US enacts a subsidy. China proposes a trade restriction, and the EU Commission's Policy responds. The small-business owner proposes reduced investment.

The estimator treats that validated proposal as bounded evidence and adjusts the residual's baseline investment. The engine then resolves everything together. The round record captures:

- the owner's intended and executed investment;
- the Aggregate's estimated and executed adjustment;
- the signed Proposal and its settlement.

**Alternative.** A second game makes the EU Commission a seat played by a human and omits the representative. The EU's capabilities are unchanged, and the small-business Aggregate follows its baseline Policy. Before any decisions are made, both games' initial accounts, liabilities, and authority reconcile.

---

## 14. Summary

This layer turns a reusable Gameboard into a playable world. It resolves:

- which Entities are explicit;
- how unselected Actors fall back;
- who controls each Actor;
- how accounts are partitioned;
- how Policies and estimators are initialized;
- which recurring flows sustain the economy.

Initialization is an atomic, validated compilation that yields one initial World State. Every subsequent round follows the canonical sequence and appends a reproducible record.

**The Gameboard defines the possible world. The Instantiation Configuration chooses its resolution. The economic engine determines how it evolves.**
