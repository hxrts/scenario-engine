# Instantiated Game Specification

## Terms

The assembly and runtime terms of this layer. The type-system terms are in the [Type System glossary](02_type_system.md#25-glossary).

| Term | Definition |
|---|---|
| **Game** | One run's assembled state, which every runner plays: the initial World State, the manifest, the in-world Strategies, the rounds to play, and one SeatBinding per seat. Built from a setting by assembly ([§10](#10-initialization-pipeline)). |
| **SeatBinding** | What answers a seat's Propose and Sign steps: a Strategy (declarative rules), a Script (literal Proposals and signatures per round), an Agent (a model call per round), or a Human (a transcript per round). |
| **Propose deadline** | The end of a round's window for human transcripts in live play ([§8](#8-time-and-session)). |
| **Joint pass** | The interpretation step that reads all of a round's transcripts together into Proposals ([§11.2](#112-live-session)). |
| **Manifest** | The immutable record of a game's resolved configuration and initial World State ([§9](#9-runtime-state-and-lifecycle)). |
| **Resolution** | The choice of which parts of a Gameboard's world are explicit and which are aggregate ([§1](#1-purpose)). |
| **Round record** | The append-only record of one round's inputs, proposed and executed actions, and events ([§11.1](#111-round-record)). |

## 1. Purpose

This layer assembles a setting into one playable Game. Instantiation chooses a **resolution** of the Gameboard's world: which parts are explicit Actors and which remain implicit Aggregates. It also fixes each Actor's controller and the initial Policy bindings. Coverage merge is the only way instantiation changes the Gameboard's holdings and flows ([§4.2](#42-partitioning)).

The layer performs two operations:

- **Validated Gameboard + Instantiation Configuration → Initial World State** ([§10](#10-initialization-pipeline)). Instantiation is pure and atomic.
- **World State + Round Inputs → New World State** ([§11](#11-round-execution)). The round is resolved by the canonical sequence ([02 §8.1](02_type_system.md#81-canonical-resolution-sequence)).

A game replays from its manifest and round records ([INV-13](02_type_system.md#inv-13), [INV-31](02_type_system.md#inv-31)).

## 2. Instantiation Configuration

The configuration is the choice of each game that the Gameboard leaves open. It is decoded from the setting's configuration file.

| Key | Type | Required | Meaning |
|---|---|---|---|
| `seed` | integer | yes | The game seed, which becomes the World State's seed ([§7.3](#73-seeds-and-randomness)). |
| `rounds` | integer | yes | Rounds to play. Must lie in the Gameboard's round range ([03 §3](03_gameboard.md#3-setting)). |
| `seats` | Actor to seat id | no | Actors controlled by a seat. |
| `npcs` | Actor to Policy id | no | Actors controlled by an in-world NPC Policy. |
| `fallbacks` | Actor to fallback | no | Overrides the declared fallback of an unselected Actor ([§3.2](#32-fallback-resolution)). |
| `representations` | list of scheme, Actor, weight | no | Active representatives. Weight defaults to 1 ([§5](#5-representation-assignment)). |

Meridian's values are in [05 §14](05_test_instance.md#14-game-settings).

The configuration may not set holdings, Market state, Rules, Policy parameters, or round intervals. The Gameboard fixes them and determines which choices are eligible ([03 §11](03_gameboard.md#11-initialization-constraints)). Configuring those, or selecting Aggregates directly, is **Planned**.

Overrides respect the layer boundaries. Tuning an instance changes declarative data, never engine code. Changing a value within its permitted range is an Instantiation change. Changing the definition of a Market or Facility needs a Gameboard variant. Neither change touches the Type System.

An invalid configuration fails before any economic state is created ([§10](#10-initialization-pipeline)).

## 3. Actors, Controllers, and Fallbacks

This section says who controls each Actor, and what happens to an Actor that nothing controls.

### 3.1 Selection and control

Selection decides which Actors a game plays. Control decides who answers for each one.

- An Actor is **selected** when it appears in `seats` or `npcs`. Each selected Actor has exactly one controller, a seat or an NPC ([INV-23](02_type_system.md#inv-23)).
- **Seat.** Allowed only if the Actor's definition permits seats, which is the default ([03 §6.2](03_gameboard.md#62-actor-definitions)). The seat's Policy is bound outside the setting, to a human's transcript, an AI agent, or a preset strategy ([06 §4](06_simulator.md#4-seat-policies)). Swapping it never changes the Actor's definition. The run's spec records the binding ([06 §3](06_simulator.md#3-simulation-specification)).
- **NPC.** Allowed only if the Policy is in the Actor's NPC list. The Policy is in-world ([03 §8](03_gameboard.md#8-policy-catalog)).
- **Required.** A required Actor must be selected.
- **Unselected.** Unless its fallback applies, an unselected Actor keeps the Gameboard's default controller, the seat of its own id. A seat with no binding submits nothing. Assembly neither asks for nor accepts a binding for an unselected Actor.
- **Live seats.** In live play, each listed human is bound to a Human seat, and every other configured seat to an Agent.
- **Binding errors.** Every configured seat has exactly one bound seat Policy. A binding for an NPC, an Aggregate, or an unknown id is an error, and so is a configured seat with no binding ([§10](#10-initialization-pipeline)).

### 3.2 Fallback resolution

A fallback decides what happens to an Actor that no seat or NPC controls.

For each unselected Actor, in Entity-id order, the first of these applies:

1. the configuration's fallback for that Actor, if it has one;
2. otherwise the definition's fallback;
3. otherwise omission. Validation rejects a non-required Actor with no definition fallback ([03 §12.2](03_gameboard.md#122-checks-implemented)), so this step is reached only for an unvalidated Gameboard.

The effect of each fallback is in [03 §6.5](03_gameboard.md#65-fallbacks). Coverage merges the Actor into an Aggregate ([§4.2](#42-partitioning)). A Policy fallback makes the Entity an NPC driven by that Policy, with its capabilities. Omission leaves the Entity's holdings and capabilities in World State with no controller, and it submits nothing. No check ensures that a required responsibility stays owned ([INV-25](02_type_system.md#inv-25)).

The three are not interchangeable. Coverage never hands a government's capabilities to a household or business Aggregate ([INV-03](02_type_system.md#inv-03)).

An override can make an Actor implicit ([06 §3.1](06_simulator.md#31-configuration-inheritance)). That removes the Actor's controller and representations, and the fallback then applies.

## 4. Aggregate Initialization

This section turns each Aggregate's three projections into initial World State. An Aggregate is an Entity with an aggregate block ([03 §6.3](03_gameboard.md#63-aggregates)). Instantiation resolves its projections separately ([02 §2.2](02_type_system.md#22-three-projections-of-an-entity)).

### 4.1 Resolving the three projections

Each projection resolves as follows.

1. **Members.** The member list is descriptive and does not change. Members may be explicit Actors or implicit.
2. **Residual accounts.** The Gameboard stores an Aggregate's accounts as the residual when its coverage Actors are explicit. Each unselected coverage Actor's holdings are added to them ([§4.2](#42-partitioning)).
3. **Collective procedures.** For each merged Actor, its weight moves from each procedure's weights into the residual weight. A procedure's total weight is the same in every resolution ([§4.3](#43-resolution-consistency)).

Representations are assigned separately ([§5](#5-representation-assignment)).

### 4.2 Partitioning

An Aggregate's economic accounts are the residual after explicitly modeled accounts are assigned to other Entities ([02 §2.2](02_type_system.md#22-three-projections-of-an-entity)). Each unselected coverage Actor is merged into its Aggregate, one at a time:

- its accounts are summed into the Aggregate, and the Actor leaves World State;
- its procedure weight folds into the residual weight ([§4.1](#41-resolving-the-three-projections));
- every reference to it is retargeted to the Aggregate. These are market access, groups, flows, shares, production, requirements, facility payment recipients, effect holders, Rules in force and scheduled, Aggregate member lists, and other Aggregates' procedure weights. Self-flows and self-shares are dropped. Identical production entries stay one. Requirements for the same Entity and Resource are summed when both are explicit amounts. Two that are "equal to purchases" stay one. An explicit amount and a purchases-based one stay two;
- a merged Actor is no longer in World State, so it cannot be chosen as a representative ([§5](#5-representation-assignment)).

An explicit Actor keeps its own accounts, and the Gameboard's Aggregate holdings do not include them. Each account therefore has one owner ([INV-08](02_type_system.md#inv-08)).

### 4.3 Resolution consistency

Two resolutions that differ only in explicitness must describe the same initial economy ([INV-14](02_type_system.md#inv-14)). Validation checks this for each coverage Actor and reports a failure ([03 §12.4](03_gameboard.md#124-resolution-consistency)). Instantiation does not run this check, so a simulation override that changes explicitness is instantiated without it ([06 §3.1](06_simulator.md#31-configuration-inheritance)).

## 5. Representation Assignment

This section says how a configuration activates a scheme for an Actor. Each entry in the configuration's representations activates a scheme ([03 §9](03_gameboard.md#9-representation-schemes)) for one Actor. The World State Representation takes its target, domain, observed Resource, estimator, and identifier from the scheme. It takes its Actor and weight, which defaults to 1, from the entry.

Instantiation checks that the scheme exists and that the Actor is eligible for it and selected ([§10](#10-initialization-pipeline)). Its planned checks are in [03 §12.3](03_gameboard.md#123-not-yet-checked-planned). The adjustment factor, and how it is applied in the round, are in [02 §7.3](02_type_system.md#73-representation-pipeline) and [§7.4](02_type_system.md#74-estimation-semantics).

## 6. Policy Initialization

This section says how each Policy instance is bound to its Entity. A Policy is bound by reference to the catalog ([03 §8](03_gameboard.md#8-policy-catalog)):

- an NPC Actor's controller names its Policy, from the configuration's NPC list or from an institutional fallback;
- an Aggregate's baseline is its aggregate's Policy;
- a seat's Policy is bound by the simulator or session, not by instantiation ([06 §4](06_simulator.md#4-seat-policies)).

A Policy instance is the catalog Strategy together with the Entity it governs. Its parameters are the Gameboard's named values ([03 §11](03_gameboard.md#11-initialization-constraints)). Policies are fixed for the game. A rule change alters the World State's Rules, never a Policy. The stored factors start empty. **Planned**: triggers for Policies.

## 7. Economic Initialization

This section carries the Gameboard's economic values into World State. The values are carried unchanged, except through coverage merges.

### 7.1 Holdings, Markets, and rules

Holdings, reserves, cost curves, Market access, and Rules come from the Gameboard. Instantiation changes them only through coverage merges ([§4.2](#42-partitioning)). Nothing is allocated or equalized.

Each Market starts at its configured position, and its price is read from the cost curve there. The Rules in force at the start are the Gameboard's initial Rules. Starting feasibility is a Planned check ([03 §12.3](03_gameboard.md#123-not-yet-checked-planned)). A negative holding or reserve fails the round that produces it ([§11](#11-round-execution)).

### 7.2 Recurring flows

The recurring cycle ([03 §5.4](03_gameboard.md#54-recurring-cycle)) is carried into World State at initialization, not computed. It is evaluated each round. Coverage merges retarget its entries ([§4.2](#42-partitioning)). Reconciliation of the cycle is Planned ([03 §12.3](03_gameboard.md#123-not-yet-checked-planned)).

### 7.3 Seeds and randomness

Seeds make every random draw reproducible.

- **Game seed.** Each game has exactly one. It is read from the configuration, recorded in the manifest, and stored as the World State's seed. It keys every in-world draw: Representation sampling, and the choices made by NPC and Aggregate Policies ([02 §9.1](02_type_system.md#91-numerics-and-randomness)).
- **Player seed.** Keys the draws of seat Policies, such as a preset's choices. It is not part of the configuration. The caller supplies it. If none is given, it is drawn once and recorded in the run's spec ([06 §3](06_simulator.md#3-simulation-specification)).
- **Keyed draws.** Draws are keyed, and the two seeds never share a stream ([06 §5](06_simulator.md#5-deterministic-randomness)).

## 8. Time and Session

This section says how many rounds a game has and when human input is accepted. The schedule is the round count, within the round range ([03 §3](03_gameboard.md#3-setting)). Every round has the same length, and rates are per round ([03 §10.2](03_gameboard.md#102-temporal-structure)).

In live play each round has one propose deadline. Humans give no input after their transcript, so there is no sign window. The live handler prompts every human in turn under one window, opened at the round's first prompt. A human who has not answered when the window closes says nothing that round. The window's length is given in seconds, and 0 means no deadline. Windows control only when submissions are accepted. They never change outcomes.

## 9. Runtime State and Lifecycle

A run's state is:

- the World State ([02 §8](02_type_system.md#8-world-state-and-round-resolution));
- the appended round records, one per round ([§11](#11-round-execution));
- the manifest.

A run folds the round runner over the World State for the configured number of rounds. The live, simulated, and replayed runs share one round runner and one set of effect rules, and only their handlers differ ([02 §8.4](02_type_system.md#84-effects-and-handlers)). The instance a runner plays is the Game that assembly builds ([§10](#10-initialization-pipeline)). No other instance type exists.

The manifest is immutable and holds:

- the Gameboard's identifier and version;
- the engine and draw-scheme versions;
- the initial World State, with every controller, procedure, Representation, Market, Rule, and the game seed.

It does not hold the Policy strategies, the round count, or the player seed. The run's spec holds those ([06 §3](06_simulator.md#3-simulation-specification)). Settings change during play only through supported mechanisms, never by editing the manifest.

A run starts only after instantiation succeeds. A failed instantiation leaves no state ([§10](#10-initialization-pipeline)). Run states and archiving are in [06 §6](06_simulator.md#6-lifecycle-and-execution).

## 10. Initialization Pipeline

This section gives the ordered steps that turn a Gameboard and configuration into an initial World State, and the errors each step can raise. Instantiation either yields the initial World State and manifest, or the full list of errors with nothing created.

Instantiation is one pure, atomic function. It takes a Gameboard and a configuration, and returns the initial world or every error. Decoding, merging, and validation of the Gameboard run before it ([03 §12.1](03_gameboard.md#121-load-pipeline)). It runs these steps:

1. **Checks.** Every required Actor is selected. Seats go only to Actors that allow them. NPC Policies appear in their Actor's list. Each scheme exists, and each representing Actor is eligible and selected. Errors: `RequiredNotSelected`, `ControllerNotAllowed`, `UnknownScheme`, and `IneligibleRepresentative`. Any error returns the full list, and no world is produced.
2. **Controllers.** Each selected Actor receives its seat or NPC controller.
3. **Fallbacks.** Each unselected Actor is resolved by its fallback, in Entity-id order ([§3.2](#32-fallback-resolution)). Coverage merges run as in [§4.2](#42-partitioning).
4. **Seed and Representations.** The World State takes the game seed. Each Representation is built from its scheme and weight ([§5](#5-representation-assignment)).
5. **Manifest.** The result is the World State and its manifest.

Assembly builds the Game. It resolves the overrides, runs instantiation, and then checks the seat bindings. Its errors are:

| Error | Raised when |
|---|---|
| `Invalid` | Instantiation or validation fails. It carries the validation errors. |
| `Unresolved` | An override or a bound Strategy names something the Gameboard lacks. |
| `UnboundSeat` / `NotASeat` | A configured seat has no binding, or a binding names something that is not a configured seat. |
| `LiteralOverrides` | Overrides are given to a literal setting. |
| `RoundsOutside` | A launch's round count is outside the round range. |

## 11. Round Execution

This section gives the order of one round as the runner executes it. A live session runs a Game through the live runner. Each seat's binding answers. A Human seat types its transcript in the propose window, and an Agent seat answers through its own model call. A Strategy or Script seat answers from its binding. An in-world Entity answers through its Policy.

The runner performs the steps in this order. Steps 3 to 6 and 11 are the runner's own work. Steps 1, 2, and 7 to 10 are engine phases that the runner calls. The phases and their guarantees are defined in the canonical sequence ([02 §8.1](02_type_system.md#81-canonical-resolution-sequence)).

1. **Prepare (phase 1, engine).** The engine applies the round's due deliveries and the Rule changes scheduled at the last boundary. Each change is recorded as an event.
2. **Observe (phase 2, engine).** Each Entity's View is built from the prepared state.
3. **Speak.** Each Entity without an in-world Policy speaks: a human's typed transcript, an AI agent's model call, or silence. Strategy, Script, and unbound seats are silent.
4. **Joint pass.** If any transcript exists, one joint pass reads all of them together into Proposals ([§11.2](#112-live-session)). If none exists, the round makes no model call.
5. **Propose (phase 3a).** Every Entity, in Entity-id order, drafts Proposals against the same beginning-of-round View. An in-world Policy drafts from its View, and a seat drafts through its binding. The joint Proposals are numbered after each author's own, so replay assigns the same ids. They join the drafted Proposals.
6. **Sign (phase 3b).** Every Entity is asked to sign the Proposals it may sign: non-Aggregate Proposals that are not its own and that name it as a signer. Human and Agent seats give no separate signature. The joint pass records their agreement.
7. **Screen (phase 4, engine).** The engine checks authority and signatures. Failures are recorded as `Rejected` or `Lapsed`.
8. **Estimate (phase 5, engine).** Each Representation's factor is computed from the validated Proposals and stored for the next round.
9. **Fund, settle, and circulate (phase 6, engine).** Accepted actions settle as one batch. The recurring cycle is then evaluated on the batch result, in the order of [03 §5.4](03_gameboard.md#54-recurring-cycle).
10. **Commit (phase 7, engine).** The net change is applied to every Entity and Market, and the round index advances. A negative holding or reserve fails the round.
11. **Record.** The runner appends the round record ([§11.1](#111-round-record)) with the transcripts its joint pass read, and narrates the round. The Run Store saves them ([06 §8](06_simulator.md#8-persistence)).

### 11.1 Round record

A round record holds the round number, its inputs, and its events.

- **Inputs**: every submitted signed Proposal, with its signatures, including those that later lapsed or were rejected. Replay needs the full set, and the events say what was dropped.
- **Events**: the outcomes of the round, whose kinds are defined in the Type System ([02 §2.5](02_type_system.md#25-glossary)). Events record executed quantities. A partial fill appears as a smaller leg, and a shortfall as moved less than owed on a leg.

Replay answers seats from the recorded Proposals and signatures, so it makes no model call ([06 §4](06_simulator.md#4-seat-policies)).

### 11.2 Live session

Live play turns typed transcripts into Proposals and signatures. Players give no input after their transcript.

- **Human players.** Each human types one transcript per round at the propose prompt, within the propose window. Nothing further is asked: no clarification and no confirmation. Inadmissible Intents are rejected by the engine with their reason ([§11.1](#111-round-record)).
- **Joint pass.** One model call reads all the transcripts together into Proposals. If the reply does not decode, the pass makes up to two corrections, so at most three calls. If the last reply still does not decode, the pass yields no Proposals, each human is told that nothing was submitted, and the round proceeds.
- **AI agent seats** speak in the same round pass. Their transcripts are read with the human transcripts.
- **Signatures.** Negotiation happens out of band, and human and AI agent seats give no separate signature. Each speaker whose transcript describes a Proposal counts as signing it. The joint pass reconciles the transcripts into one Proposal.
- **Strategy and Script seats** answer through their binding.

Admissibility is enforced by the engine at screening, whichever source produced a Proposal ([02 §3.1](02_type_system.md#31-intent-validation)).

## 12. Validation

Validation runs instantiation as a feasibility check, and adds the Gameboard's checks. The checks are in [03 §12](03_gameboard.md#12-validation-and-compilation). The simulator instantiates a spec's overrides with the same function, without re-running validation ([06 §3.1](06_simulator.md#31-configuration-inheritance)). The invariants a valid game must satisfy are in [02 §9](02_type_system.md#9-architectural-invariants-and-numerics).

Initialization fails if any checked condition fails. A configuration is never repaired silently. No Actor changes meaning, no unrelated Policy is assigned, and no resources are invented.
