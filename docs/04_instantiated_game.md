# Instantiated Game Specification

**Version:** 0.3
**Scope:** Instantiated Game Layer
**Implementation language:** PureScript

---

## 1. Purpose

An Instantiated Game is one playthrough of a Gameboard. Instantiation is a particular **resolution** of the Gameboard's world: which parts are explicit Actors and which remain implicit Aggregates. It also fixes each Actor's controller, the initial Policy bindings, the initial holdings and capacities, and the recurring flows. The same Gameboard supports many games without modification. The EU might be a seat in one game, an NPC in another, and implicit in a third.

The layer performs two separate operations:

- **Validated Gameboard + Instantiation Configuration → Initial World State** (§10). `instantiate` is pure and atomic.
- **World State + Round Inputs → New World State** (§11). The round runner and engine resolve one round (Type System [§8.1](02_type_system.md#81-canonical-resolution-sequence)).

Terms, invariants, and the round sequence are defined in the Type System specification ([§2.5](02_type_system.md#25-glossary), [§8.1](02_type_system.md#81-canonical-resolution-sequence), [§9](02_type_system.md#9-architectural-invariants-and-numerics)). Whether an Actor is a seat or an NPC never changes its economic semantics: the engine sees only signed Proposals. A game replays from its manifest and round records ([INV-13](02_type_system.md#inv-13), [INV-15](02_type_system.md#inv-15), [INV-31](02_type_system.md#inv-31)).

## 2. Instantiation Configuration

The configuration is a JSON object, `instantiation.json` in a scenario directory, decoded by `decodeConfig`. It supplies only the choices that the Gameboard leaves open ([03 §11](03_gameboard.md#11-initialization-constraints)).

| Key | Type | Required | Meaning |
|---|---|---|---|
| `seed` | integer | yes | The game seed. Becomes the World State's `seed`. Every in-world random draw uses it ([§7.3](#73-seeds-and-randomness)). |
| `rounds` | integer | yes | Rounds to play. Must lie in the Gameboard's `roundRange` ([03 §3](03_gameboard.md#3-scenario)). |
| `seats` | object: Actor → seat id | no | Actors controlled by a seat, such as `"US": "seat-1"`. |
| `npcs` | object: Actor → Policy id | no | Actors controlled by an in-world NPC Policy, such as `"CN": "china-strategic-v0"`. |
| `fallbacks` | object: Actor → fallback tag | no | Overrides the declared fallback of an unselected Actor ([§3.2](#32-fallback-resolution)). |
| `representations` | array of `{ scheme, actor, weight }` | no | Active representatives. `weight` defaults to `1`. |

Meridian's configuration (`instantiation.json`) has 15 seats (`seat-1` to `seat-15`), one NPC (`CN`), one representation (`SME` for `sme-energy-sample`, weight 1), 5 rounds, and seed `26021` ([05 §14](05_test_instance.md#14-game-settings)).

Holdings, capacities, Market state, Rules, Policy parameters, and round intervals are not configurable. The Gameboard fixes them ([03 §11](03_gameboard.md#11-initialization-constraints)). Configuring them, or selecting Aggregates directly, is planned.

The Gameboard determines which choices are eligible. An invalid configuration fails with `ValidationError`s before any economic state is created.

## 3. Actors, Controllers, and Fallbacks

An Actor is an Entity with a `definition` ([03 §6.2](03_gameboard.md#62-actor-definitions)). It is selected when it appears in `seats` or `npcs`. An unselected Actor is resolved by its fallback.

### 3.1 Selection and control

Each selected Actor has exactly one controller ([INV-23](02_type_system.md#inv-23)):

```purescript
data Controller = Seat SeatId | NPC PolicyId
```

- **Seat.** Permitted only if the definition's `seat` is true (the default). The Policy that drives a seat is bound outside the scenario: a human's text in a live session, an AI agent, or a preset program ([06 §4](06_simulator.md#4-seat-policies)). Swapping that Policy never changes the Actor's definition or makes it an NPC ([INV-30](02_type_system.md#inv-30)). The run's spec records the binding ([06 §3](06_simulator.md#3-simulation-specification)).
- **NPC.** Permitted only if the Policy id is in the definition's `npc` list. The Policy is in-world ([03 §8](03_gameboard.md#8-policy-catalog)).
- **Required.** A `required` Actor must be selected (`RequiredNotSelected`).
- **CLI session.** `session --humans` lists the Actors played by humans. Every other seat is an AI agent ([apps/cli](../apps/cli/src/Session.purs)).

### 3.2 Fallback resolution

For each unselected Actor, in Entity-id order, instantiation resolves the fallback in this order:

1. The configuration's `fallbacks` entry for that Actor, if present.
2. Otherwise the definition's `fallback`.
3. Otherwise `omission`. Validation rejects a non-required Actor without a fallback (`MissingFallback`), so this step is reached only for a Gameboard that has not been validated.

The fallback then applies ([03 §6.5](03_gameboard.md#65-fallbacks)):

- `coverage:<aggregate>`: the Actor's holdings and activity merge into the Aggregate ([§4.2](#42-partitioning)).
- `policy:<policy>`: the Entity becomes an NPC driven by that Policy. Its capabilities stay with it.
- `omission`: nothing changes. The Entity's holdings and capabilities remain in World State without a controller, and it submits nothing. Validation does not check that this leaves no required responsibility unowned ([INV-25](02_type_system.md#inv-25)).

The alternatives are not interchangeable. A government institution's capabilities are never handed to a household or business Aggregate ([INV-03](02_type_system.md#inv-03)).

A simulation can make an Actor implicit with the override `Implicit` ([06 §3.1](06_simulator.md#31-configuration-inheritance)). That removes the Actor's controller and representations, and then the fallback above applies.

## 4. Aggregate Initialization

An Aggregate is an Entity with an `aggregate` block ([03 §6.3](03_gameboard.md#63-aggregates)). Instantiation resolves its three projections separately (Type System [§2.2](02_type_system.md#22-three-projections-of-an-entity)).

### 4.1 Resolving the three projections

1. **Members.** The member list is descriptive and does not change. Members may be explicit Actors or implicit.
2. **Residual accounts.** The Gameboard stores an Aggregate's accounts as the residual when its coverage Actors are explicit. When a coverage Actor is unselected, its holdings and capacities are added to the Aggregate ([§4.2](#42-partitioning)).
3. **Collective procedures.** For each unselected member, its weight is removed from each procedure's `weights` and added to `residualWeight`. The total weight of a procedure is therefore the same in every resolution ([§4.3](#43-resolution-consistency)).

Each Aggregate's baseline Policy is its `aggregate.policy`. Representations are assigned separately ([§5](#5-representation-assignment)).

### 4.2 Partitioning

Each unselected coverage Actor is merged into its Aggregate, one at a time:

- its accounts and capacities are summed into the Aggregate, and the Actor is removed from World State.
- its procedure weight folds into `residualWeight` (§4.1).
- every Market access list, jurisdiction entry, flow, share, production entry, and requirement that names it is retargeted to the Aggregate. Self-flows and self-shares are dropped, and requirements for the same Entity and Resource are summed. An explicit amount and one equal to purchases cannot be summed, so both are kept as separate requirements.

An explicit Actor keeps its own accounts. The Gameboard's Aggregate holdings do not include them, so each account has exactly one owner ([INV-08](02_type_system.md#inv-08)). Memberships do not move holdings. Capacity ownership is checked separately ([03 §12.2](03_gameboard.md#122-checks-implemented)).

### 4.3 Resolution consistency

Two resolutions that differ only in explicitness must describe the same initial economy ([INV-14](02_type_system.md#inv-14)). For example, a German government that is implicit within the EU, and one that is an explicit seat, must agree before any decision is made, on:

- stocks (Entity accounts and Market reserves).
- capacities.
- procedure weights.
- enacting authority.

Validation checks this for each coverage Actor, comparing the configuration with that Actor unselected ([03 §12.4](03_gameboard.md#124-resolution-consistency)). A difference is `ResolutionInconsistent`. The check runs in validation, not in `instantiate`. A simulation override that changes explicitness is instantiated without re-running validation ([06 §3.1](06_simulator.md#31-configuration-inheritance)).

Later trajectories may legitimately diverge, because players decide differently. They must not diverge because a holding was duplicated, a liability deleted, or a rule changed.

## 5. Representation Assignment

Each `representations` entry activates a scheme ([03 §9](03_gameboard.md#9-representation-schemes)) for one Actor. The resulting World State Representation takes:

- `aggregate`, `domain`, and `estimator` from the scheme.
- `actor` and `weight` from the entry, with `weight` defaulting to `1`.
- `id` from the scheme.

Instantiation checks that the scheme exists (`UnknownReference`) and that the Actor is listed in the scheme's `eligibleActors` and is selected (`IneligibleRepresentative`). Range checks on the weight and estimator, checks for duplicate assignments, and compatibility checks are planned, not implemented.

The mechanics are defined in Type System [§7.3](02_type_system.md#73-representation-pipeline) and [§7.4](02_type_system.md#74-estimation-semantics):

- A representative's own transactions execute once, against its own holdings.
- In round *r*, the engine computes the adjustment factor from the representative's validated Proposals and stores it in World State. The factor is `1 + Σ shrinkage × weight × deviation` over the Aggregate's representatives, per domain.
- In round *r + 1*, the Aggregate's Policy reads the stored factor (`Factor`) and applies it to its baseline once ([INV-09](02_type_system.md#inv-09)).
- With no representative, or no stored factor, the factor is `1` and the Aggregate follows its baseline.

Representation never confers holdings or capabilities ([INV-03](02_type_system.md#inv-03)).

## 6. Policy Initialization

A Policy is bound by reference to the catalog ([03 §8](03_gameboard.md#8-policy-catalog)):

- an NPC Actor's controller names its Policy (from `npcs`, or from an institutional fallback).
- an Aggregate's baseline is `aggregate.policy`.
- a seat's Policy is bound by the simulator or session, not by instantiation ([06 §4](06_simulator.md#4-seat-policies)).

A Policy instance is the catalog Program together with the Entity it governs. Its parameters are the Gameboard's named values ([03 §11](03_gameboard.md#11-initialization-constraints)). Per-instance parameters, triggers, and View permissions ([INV-16](02_type_system.md#inv-16)) are planned, not implemented. Each View is the beginning-of-round snapshot: the Entity's own state, prices, Rules in force, the events so far, the Entity's stored factors, and any Proposals it may sign.

Policies are fixed for the game, and a `ChangeRule` alters the World State's Rules, never a Policy. A Policy has no memory store: what it knows from earlier rounds comes from recorded events and stored factors. The stored factors start empty. The engine never authors or rewrites a Proposal. In the estimate step it stores factors and nothing else ([INV-26](02_type_system.md#inv-26)).

## 7. Economic Initialization

### 7.1 Holdings, Markets, capacities, and rules

Holdings, reserves, capacities, cost curves, Market access, and Rules come from the Gameboard. Instantiation changes them only through coverage merges ([§4.2](#42-partitioning)). Nothing is allocated or equalized.

Each Market starts at its configured `position` (default `0`), and its price is derived from the cost curve at that position. The Rules in force at the start come from `game-rules.json`. Meridian has none. Starting feasibility ([INV-27](02_type_system.md#inv-27)) is not checked at initialization. A negative holding or reserve instead fails the first round that produces one ([03 §5.6](03_gameboard.md#56-limits)).

### 7.2 Recurring flows

The cycle ([03 §5.4](03_gameboard.md#54-recurring-cycle)) is referenced, not computed, at initialization. Its entries are retargeted by coverage merges ([§4.2](#42-partitioning)) and evaluated each round. Validation checks that every costed Facility has a payee ([03 §12.2](03_gameboard.md#122-checks-implemented)). Reconciliation of the cycle ([INV-05](02_type_system.md#inv-05)) and detection of depletion-only configurations are planned, not implemented.

### 7.3 Seeds and randomness

- **Game seed.** The configuration's `seed`, stored as the World State's `seed`. It keys every in-world draw: Representation sampling noise and `Choose` in NPC and Aggregate Policies ([Type System §9.1](02_type_system.md#91-numerics-and-randomness)).
- **Player seed.** A separate seed for seat Policies, such as `Choose` in a preset. It is not part of the configuration. The CLI takes it as `--seed`, and the server as `playerSeed`. When absent, it is drawn once and recorded in the run's spec ([06 §3](06_simulator.md#3-simulation-specification)). A live `session` resolves and records one as well.
- **Keyed draws.** A draw's value depends only on its key (round, source, Entity, category, index), so replays and resumed runs match ([06 §5](06_simulator.md#5-deterministic-randomness)).

Mechanics take no random draws by default. Nothing in the initial state is resampled after instantiation.

## 8. Time and Session

The schedule is the round count (`rounds`, within `roundRange`). Every round has the same length, and `horizon` gives the total in years ([03 §3](03_gameboard.md#3-scenario)). Per-round intervals, and scaling annual rates to round length ([INV-28](02_type_system.md#inv-28)), are planned. Rates are per round.

In live play, each round has two windows, closed by the clock (the live handler, [Handlers.Live](../packages/handlers/src/Handlers/Live.purs), waits out each one with `Adapters.Clock.delayMs`): a **propose deadline** (`ProposeDeadline`), after which Proposals freeze, and a **sign deadline** (`SignDeadline`), after which signatures freeze. The CLI's `--window` sets each window's length in seconds, and it defaults to `0`. A provisional six-hour session schedule is in [05 §19](05_test_instance.md#19-session-structure). The engine does not enforce it. Windows control only when submissions are accepted. They never change outcomes.

## 9. Runtime State and Lifecycle

A run's state is:

- the **World State**: the round index, the game seed, Entities with their controllers, Markets, capacities, Rules in force, scheduled Rule changes, pending deliveries, stored factors, events, and Representations.
- the appended **round records**, one per round ([§11](#11-round-execution)).
- the **manifest**.

`runGame` folds the round runner over the World State for the configured number of rounds (`Effects.Round`). No separate instance type exists. The same `runRound` runs under three handler sets: **Live** (human text, AI agents, and the clock), **Sim** (preset programs in the simulator), and **Replay** (recorded seat inputs). Effects and handlers are defined in Type System [§8.4](02_type_system.md#84-effects-and-handlers).

The **manifest** is immutable and holds:

- `gameboard`: `<id>@<version>`.
- `engine` and `draw`: the engine and draw-scheme versions.
- `initial`: the initial World State. It carries every controller, procedure, Representation, Market, Rule, and the game seed.

It does not hold the Policy programs, the round count, or the player seed. The run's spec holds those ([06 §3](06_simulator.md#3-simulation-specification), [§8](06_simulator.md#8-persistence)). Round records are append-only, and snapshots are stored per round by the run store ([INV-31](02_type_system.md#inv-31)). Settings change during play only through supported mechanisms, never by editing the manifest.

Run states and archiving are defined in [06 §6](06_simulator.md#6-lifecycle-and-execution). A run starts only after instantiation succeeds, and a failed instantiation leaves no state ([§10](#10-initialization-pipeline)).

## 10. Initialization Pipeline

`instantiate` (`Board.Gameboard.Instantiate`) is one pure, atomic function:

```purescript
instantiate :: Gameboard -> InstantiationConfig -> Either (Array ValidationError) InitialWorld
```

It runs these steps. Gameboard decoding, merging, and validation run before it ([03 §12.1](03_gameboard.md#121-load-pipeline)):

1. **Controllers.** Every required Actor is selected. Seats are given only to Actors with `seat` true. NPC Policies appear in their Actor's `npc` list.
2. **Representations.** Each scheme exists, and each representing Actor is eligible and selected.
3. **Fallbacks.** If steps 1 and 2 produced no errors, each unselected Actor is resolved by its fallback, in Entity-id order ([§3.2](#32-fallback-resolution)). Coverage merges run as in [§4.2](#42-partitioning).
4. **Controllers set.** Each selected Actor receives its seat or NPC controller.
5. **Seed and Representations.** The World State takes the game seed. Each Representation is built from its scheme and weight ([§5](#5-representation-assignment)).
6. **Manifest.** The result is `{ world, manifest }`.

Validation ([03 §12](03_gameboard.md#12-validation-and-compilation)) runs this function as a feasibility check. It also runs the structural checks and resolution consistency ([§4.3](#43-resolution-consistency)).

Any error returns the full error list, and no world is produced. The simulator instantiates a spec's overrides with the same function, without re-running validation ([06 §3.1](06_simulator.md#31-configuration-inheritance)).

## 11. Round Execution

Each round follows the canonical sequence ([Type System §8.1](02_type_system.md#81-canonical-resolution-sequence)). The round runner (`Effects.Round`) runs phase 3 for every Entity at once and calls the engine's `resolveRound` for the remaining phases. In code order:

1. **Prepare (phase 1).** Deliveries due by this round are applied, clamped at zero. Rule changes scheduled at the last boundary take effect, and scheduled repeals are removed. Each change is recorded as an event.
2. **Observe (phase 2).** Each Entity's View is built from the prepared state.
3. **Propose (phase 3a).** Each Entity, in Entity-id order, proposes. An NPC or Aggregate Policy proposes from its View. For a seat, the round runner asks the seat source for input. For a human seat, the input is the transcript typed in the propose window; the round's transcripts are read together in one joint pass.
4. **Sign (phase 3b).** Each non-Aggregate Entity named as a signer of another Entity's Proposal is asked to sign. Human seats answer within the sign window. Aggregates do not sign.
5. **Screen (phase 4).** Duplicate Proposal ids are rejected. Authority is checked, and each signature rule is applied: every signer for a deal, or support above the threshold for a procedure. Failures are recorded as `Rejected` or `Lapsed`. Inputs are ordered by author, then submission index, never by arrival time ([INV-11](02_type_system.md#inv-11)).
6. **Estimate (phase 5).** Each Representation's factor is computed from the validated, proposed quantities, before funding, and stored for the next round.
7. **Fund and settle (phase 6).** Proposed quantities are funded from escrow. Accepted actions resolve as one batch: Market trades clear at uniform prices. Each deal settles atomically with any Facility order it bundles. A passed procedure Proposal counts as its Aggregate's action. Limit prices, pro-rata allocation, and tie-breaking are defined in Type System [§3](02_type_system.md#3-intents-and-economic-actions) and [§5.5](02_type_system.md#55-simultaneous-clearing).
8. **Circulate (phase 6).** The recurring cycle is evaluated on the batch result: production delivered next round, shares, requirements with shortfall penalties, fixed flows, and decay ([03 §5.4](03_gameboard.md#54-recurring-cycle)).
9. **Commit (phase 7).** The net change is applied to every Entity and Market, and the round index advances. A negative holding or reserve fails the round (`NegativeBalance`, `NegativeReserve`) rather than recording an invalid state.
10. **Record.** The round runner appends `{ round, inputs, events }` ([§11.1](#111-round-record)).

### 11.1 Round record

A round record contains:

- `round`: the round index.
- `inputs`: the accepted signed Proposals, each with its signatures.
- `events`: the outcomes of the round. The event kinds are `Lapsed`, `Rejected` (with its reason), `Traded`, `Transferred`, `Transformed`, `Delivered`, `Enacted`, `Repealed`, `CapacityChanged`, `Estimated`, `Consumed`, `Shortfall`, `Decayed`, and `ProcedureOutcome` (support, threshold, and whether it executed).

Events record executed quantities, so a partial fill appears as a smaller `Traded` quantity, not as a separate state. Replay starts from `inputs` and does not re-run language-model interpretation ([Handlers.Seats](../packages/handlers/src/Handlers/Seats.purs)).

### 11.2 Live session

- **Human players.** One audio submission per player per round, then no further input. Transcription and joint interpretation run once per round over all submissions. There is no clarification and no confirmation step. Inadmissible Intents are rejected by the engine like any other, and the player is told why ([Handlers.Live](../packages/handlers/src/Handlers/Live.purs)).
- **AI agent seats** speak in the same round pass: their transcripts are read with the human transcripts, and they get no confirmation.
- **Preset seats** play their bound program.

Admissibility is enforced by the engine at screening (step 5), whichever source produced a Proposal ([Type System §3.1](02_type_system.md#31-intent-validation)).

The session prints each seat's briefing from its View, and the front page and balance sheets from recorded events ([apps/cli](../apps/cli/src/Session.purs), [06 §9.2](06_simulator.md#92-run-details)). Narration adds no facts.

All participants act on one World State ([INV-15](02_type_system.md#inv-15)). Alternative outcomes need a separate instance or a deliberate replay branch.

## 12. Validation

A valid Instantiated Game satisfies every invariant in the Type System master table ([02 §9](02_type_system.md#9-architectural-invariants-and-numerics)). The instantiation-tagged invariants are checked as follows:

| ID | Checked as | Status |
|---|---|---|
| [INV-03](02_type_system.md#inv-03) | Coverage keeps enacting authority equal (§4.3). Institutional fallbacks keep capabilities. | Checked for coverage Actors |
| [INV-05](02_type_system.md#inv-05) | Cycle reconciliation. Facility payees for costed Facilities. | Payee only. Reconciliation planned |
| [INV-08](02_type_system.md#inv-08) | Capacities have one owner. Coverage merges leave no duplicate accounts. | Checked |
| [INV-14](02_type_system.md#inv-14) | Stocks, capacities, procedure weights, and authority match across explicitness. | Checked for coverage Actors. Liabilities and flows planned |
| [INV-15](02_type_system.md#inv-15), [INV-16](02_type_system.md#inv-16) | One World State per run. Each Policy's View limited to its permitted information. | One World State: by construction. View limits: planned |
| [INV-23](02_type_system.md#inv-23) | Each selected Actor has exactly one controller. | By construction. Controller compatibility checked |
| [INV-24](02_type_system.md#inv-24) | Selection, eligibility, and scheme existence. | Partly checked. Ranges planned |
| [INV-25](02_type_system.md#inv-25) | Non-required Actors have a fallback. | Partly checked |
| [INV-26](02_type_system.md#inv-26) | Aggregate and NPC Policies exist and decode. | Checked |
| [INV-27](02_type_system.md#inv-27) | Starting Market and capacity feasibility. | Planned |
| [INV-28](02_type_system.md#inv-28) | Round intervals fit the horizon. | Planned |

Initialization fails if any checked condition fails. A configuration is never repaired silently, by changing an Actor's meaning, assigning an unrelated Policy, or inventing resources.

## 13. Illustrative Instantiation

Meridian's seat assignments, initial holdings, Market state, round schedule, and phase mapping are specified in [05 Part III](05_test_instance.md#part-iii-instantiated-game-configuration) and [05 §20](05_test_instance.md#20-phase-mapping). The fixtures that exercise individual rounds are in [05 Part IV](05_test_instance.md#part-iv-playtest-fixtures).

---

## 14. Summary

Instantiation turns a reusable Gameboard into a playable world. It:

- selects which Entities are explicit, and resolves each unselected Actor by its fallback.
- assigns a controller to each explicit Actor.
- merges implicit coverage into Aggregates and binds Representations.
- produces one initial World State and its manifest, atomically.

Each round then follows the canonical sequence and appends a record. The Gameboard defines the possible world, the configuration chooses its resolution, and the engine determines how it evolves.
