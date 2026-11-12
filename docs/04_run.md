# Run Specification

## 1. Purpose

A PremiseComposition describes a set of possible Worlds, and a Run realizes one of them, Round by Round. Assembly builds what a runner plays from a PremiseComposition ([§10](#10-initialization-pipeline)): the initial World, the Run manifest, the in-world Strategies, the rounds to play, and one seat binding per seat. Instantiation chooses a resolution of the PremiseComposition: which parts are explicit Actors and which remain implicit Aggregates. It also fixes each Actor's controller and the initial Strategy bindings. Coverage merge is the only way instantiation changes the PremiseComposition's holdings and flows ([§4.2](#42-partitioning)).

A Run performs two operations:

- **Validated PremiseComposition + Instantiation Configuration → initial World** ([§10](#10-initialization-pipeline)). Instantiation is pure and atomic.
- **World + Round Inputs → new World** ([§11](#11-round-execution)). The round is resolved by the canonical sequence ([02 §8.1](02_kernel.md#81-canonical-resolution-sequence)).

A Run replays from its manifest and round records ([INV-13](02_kernel.md#inv-13), [INV-31](02_kernel.md#inv-31)).

## 2. Instantiation Configuration

The configuration is the choice of each Run that the PremiseComposition leaves open. It is decoded from the PremiseComposition's configuration file.

| Key | Type | Required | Meaning |
|---|---|---|---|
| `seed` | integer | yes | The game seed, which becomes the World's seed ([§7.3](#73-seeds-and-randomness)). |
| `rounds` | integer | yes | Rounds to play. Must lie in the PremiseComposition's round range ([03 §3](03_premise.md#3-premise)). |
| `seats` | Actor to seat id | no | Actors controlled by a seat. |
| `nonPlayableActors` | Actor to Strategy id | no | Actors controlled by an in-world non-playable Actor Strategy. |
| `fallbacks` | Actor to fallback | no | Overrides the declared fallback of an unselected Actor ([§3.2](#32-fallback-resolution)). |
| `representations` | list of scheme, Actor, weight | no | Active representatives. Weight defaults to 1 ([§5](#5-representation-assignment)). |
| `preset` | preset name | no | The preset a launch plays when it names none. It must be one of the PremiseComposition's presets ([07 §4.2](07_simulator.md#42-presets)). |

Meridian's values are in [06 §14](06_meridian.md#14-run-configuration), and Pacific's in [05 §3.1](05_pacific.md#31-resources-and-holdings).

The configuration may not set holdings, Exchange state, Rules, Strategy parameters, or round intervals. The PremiseComposition fixes them and determines which choices are eligible ([03 §11](03_premise.md#11-initialization-constraints)).

Overrides respect the layer boundaries. Tuning a Run's configuration changes declarative data, never engine code. Changing a Representation's weight or the round count is an Instantiation change. Changing the definition of an Exchange or Transformation needs a PremiseComposition variant. Neither change touches the kernel.

An invalid configuration fails before any economic state is created ([§10](#10-initialization-pipeline)).

## 3. Actors, Controllers, and Fallbacks

This section covers Actors with a definition, not Aggregates: who controls each one, and what happens to one that nothing controls.

### 3.1 Selection and control

Selection decides which Actors a Run plays. Control decides who answers for each one.

- An Actor is selected when it appears in `seats` or `nonPlayableActors`. Each selected Actor has exactly one controller, a seat or an in-world Strategy ([INV-23](02_kernel.md#inv-23)).
- **Seat.** Allowed only if the Actor's definition permits seats, which is the default ([03 §6.2](03_premise.md#62-actor-definitions)). The seat's Strategy is bound outside the PremiseComposition, to a human's transcript, an AI agent, or a preset strategy ([07 §4](07_simulator.md#4-seat-strategies)). Swapping it never changes the Actor's definition. The run's spec records the binding ([07 §3](07_simulator.md#3-simulation-specification)).
- **Non-playable Actor.** Allowed only if the Strategy is in the Actor's list of non-playable Actor Strategies. The Strategy is in-world ([03 §8](03_premise.md#8-strategy-catalog)).
- **Required.** A required Actor must be selected.
- **Unselected.** Unless its fallback applies, an unselected Actor keeps the PremiseComposition's default controller, the seat of its own id. A seat with no binding submits nothing. Assembly neither asks for nor accepts a binding for an unselected Actor.
- **Live seats.** In live play, each listed human is bound to a Human seat, and every other configured seat to an Agent.
- **Seat bindings.** A seat binding answers a seat's Propose and Sign steps: `StrategyRules`, a Script (literal candidate Transactions and signatures per Round), an Agent (a model call per Round), or a Human (a transcript per Round).
- **Binding errors.** Every configured seat has exactly one bound seat Strategy. A binding for a non-playable Actor, an Aggregate, or an unknown id is an error, and so is a configured seat with no binding ([§10](#10-initialization-pipeline)).

### 3.2 Fallback resolution

A fallback decides what happens to an Actor that no seat or in-world Strategy controls.

For each unselected Actor, in Actor-id order, the first of these applies:

1. the configuration's fallback for that Actor, if it has one;
2. otherwise the definition's fallback;
3. otherwise omission. Validation rejects a non-required Actor with no definition fallback ([03 §12.2](03_premise.md#122-checks-implemented)), so this step is reached only for an unvalidated PremiseComposition.

The effect of each fallback is in [03 §6.5](03_premise.md#65-fallbacks). Coverage merges the Actor into an Aggregate ([§4.2](#42-partitioning)). A Strategy fallback makes the Actor a non-playable Actor driven by that Strategy, with its capabilities. Omission leaves the Actor's holdings and capabilities in the World with no controller, and it submits nothing. Every required responsibility has an owner, since validation requires a required Actor to be selected and every other to have a fallback ([INV-25](02_kernel.md#inv-25)).

The three are not interchangeable. Coverage never hands a government's capabilities to a household or business Aggregate ([INV-03](02_kernel.md#inv-03)).

An override can make an Actor implicit ([07 §3.1](07_simulator.md#31-configuration-inheritance)). That removes the Actor's controller and representations, and the fallback then applies.

## 4. Aggregate Initialization

This section turns each Aggregate's three projections into the initial World. An Aggregate is an Actor with an aggregate block ([03 §6.3](03_premise.md#63-aggregates)). Instantiation resolves its projections separately ([02 §2.2](02_kernel.md#22-three-projections-of-an-actor)).

### 4.1 Resolving the three projections

Each projection resolves as follows.

1. **Members.** The member list is descriptive and does not change. Members may be explicit Actors or implicit.
2. **Residual accounts.** The PremiseComposition stores an Aggregate's accounts as the residual when its coverage Actors are explicit. Each unselected coverage Actor's holdings are added to them ([§4.2](#42-partitioning)).
3. **Collective procedures.** For each merged Actor, its weight moves from each procedure's weights into the residual weight. A procedure's total weight is the same in every resolution ([§4.3](#43-resolution-consistency)).

Representations are assigned separately ([§5](#5-representation-assignment)).

### 4.2 Partitioning

An Aggregate's economic accounts are the residual after explicitly modeled accounts are assigned to other Actors ([02 §2.2](02_kernel.md#22-three-projections-of-an-actor)). Each unselected coverage Actor is merged into its Aggregate, one at a time:

- its accounts are summed into the Aggregate, and the Actor leaves the World;
- its procedure weight folds into the residual weight ([§4.1](#41-resolving-the-three-projections));
- every reference to it is retargeted to the Aggregate. These are exchange access, groups, flows, shares, production, requirements, Transformation payment recipients, effect holders, Rules in force and scheduled, Aggregate member lists, and other Aggregates' procedure weights. Self-flows and self-shares are dropped. Identical production entries stay one. Requirements for the same Actor and Resource are summed when both are explicit amounts. Two that are "equal to purchases" stay one. An explicit amount and a purchases-based one stay two;
- a merged Actor is no longer in the World, so it cannot be chosen as a representative ([§5](#5-representation-assignment)).

An explicit Actor keeps its own accounts, and the PremiseComposition's Aggregate holdings do not include them. Each account therefore has one owner ([INV-08](02_kernel.md#inv-08)).

### 4.3 Resolution consistency

Two resolutions that differ only in explicitness must describe the same initial economy ([INV-14](02_kernel.md#inv-14)). Validation checks this for each coverage Actor and reports a failure ([03 §12.3](03_premise.md#123-resolution-consistency)). Instantiation does not run this check, so a simulation override that changes explicitness is instantiated without it ([07 §3.1](07_simulator.md#31-configuration-inheritance)).

## 5. Representation Assignment

This section says how a configuration activates a scheme for an Actor. Each entry in the configuration's representations activates a scheme ([03 §9](03_premise.md#9-representation-schemes)) for one Actor. The Representation in the World takes its target, domain, observed Resource, estimator, and identifier from the scheme. It takes its Actor and weight, which defaults to 1, from the entry.

Instantiation checks that the scheme exists and that the Actor is eligible for it and selected ([§10](#10-initialization-pipeline)). Estimator ranges and duplicate adjustments are checked at load ([03 §12.2](03_premise.md#122-checks-implemented)). The adjustment factor, and how it is applied in the round, are in [02 §7.3](02_kernel.md#73-representation-pipeline) and [§7.4](02_kernel.md#74-estimation-semantics).

## 6. Strategy Initialization

This section says how each Strategy instance is bound to its Actor. A Strategy is bound by reference to the catalog ([03 §8](03_premise.md#8-strategy-catalog)):

- a non-playable Actor's controller names its Strategy, from the configuration's `nonPlayableActors` or from an institutional fallback;
- an Aggregate's Strategy is its baseline;
- a seat's Strategy is bound by the simulator or session, not by instantiation ([07 §4](07_simulator.md#4-seat-strategies)).

A seat whose Actor definition names a routine ([03 §6.2](03_premise.md#62-actor-definitions)) plays it after its player, whatever the player is: a preset, an agent or a human. Assembly composes every such seat (`Composed { routine, player }`); a Script is a seat's literal whole input and is never composed. In a round the player answers first. Its proposals, and for an agent or a human the Intents its interpreted speech put forward, are what the routine's `planned` reads ([03 §8](03_premise.md#8-strategy-catalog)), so a routine can sell what the player did not commit. Both halves propose as the one Actor, under its own Capabilities, and their candidate Transactions are ordinary, recorded inputs, so a replay answers the whole seat from the record and runs neither half again. The engine never adds or rewrites an Intent for a routine.

A Strategy instance is the catalog `StrategyRules` together with the Actor it governs. Its parameters are the PremiseComposition's named values ([03 §11](03_premise.md#11-initialization-constraints)). Strategies are fixed for the Run. A rule change alters the World's Rules, never a Strategy. The stored factors start empty.

## 7. Economic Initialization

This section carries the PremiseComposition's economic values into the World. The values are carried unchanged, except through coverage merges.

### 7.1 Holdings, Exchanges, and rules

Holdings, reserves, cost curves, Exchange access, and Rules come from the PremiseComposition. Instantiation changes them only through coverage merges ([§4.2](#42-partitioning)). Nothing is allocated or equalized.

Each Exchange starts at its configured position, and its price is read from the cost curve there. The Rules in force at the start are the PremiseComposition's initial Rules. The World holds them as authored Rules, which initialize as laws: each is read as a kernel law when it applies ([03 §14](03_premise.md#14-compilation-to-the-kernel)). Starting feasibility is checked at load ([03 §12.2](03_premise.md#122-checks-implemented)). A negative holding or reserve fails the round that produces it ([§11](#11-round-execution)).

### 7.2 Recurring flows

The recurring cycle ([03 §5.4](03_premise.md#54-recurring-cycle)) is carried into the World at initialization, not computed, as authored recurring rules, which initialize as time laws. Each round every rule runs as a stock law, and a created flow spawns a scheduled Flow ([03 §14](03_premise.md#14-compilation-to-the-kernel)). Coverage merges retarget its entries ([§4.2](#42-partitioning)).

### 7.3 Seeds and randomness

Seeds make every random draw reproducible.

- **Game seed.** Each Run has exactly one. It is read from the configuration, recorded in the manifest, and stored as the World's seed. It keys every in-world draw: Representation sampling, and the choices made by non-playable Actor and Aggregate Strategies ([02 §9.1](02_kernel.md#91-numerics-and-randomness)).
- **Player seed.** Keys the draws of seat Strategies, such as a preset's choices. It is not part of the configuration. The caller supplies it. If none is given, it is drawn once and recorded in the run's spec ([07 §3](07_simulator.md#3-simulation-specification)).
- **Keyed draws.** Draws are keyed, and the two seeds never share a stream ([07 §5](07_simulator.md#5-deterministic-randomness)).

## 8. Time and Session

This section says how many rounds a Run has and when human input is accepted. The schedule is the round count, within the round range ([03 §3](03_premise.md#3-premise)). Rates are per round ([03 §10.2](03_premise.md#102-temporal-structure)).

In live play each round has one propose deadline. Humans give no input after their transcript, so there is no sign window. Every human answers under one window, opened at the round's first prompt, and a human's text reaches the round through one source, whether typed at the console or posted to the session routes ([§11.2](#112-live-session)). A human who has not answered when the window closes says nothing that round. The window's length is given in seconds, and 0 means no deadline. Windows control only when submissions are accepted. They never change outcomes.

## 9. Runtime State and Lifecycle

A run's state is:

- the World ([02 §8](02_kernel.md#8-world-and-round-resolution));
- the appended round records, one per round ([§11](#11-round-execution));
- the manifest.

The World includes the pending transactions and the templates table ([02 §3.4](02_kernel.md#34-successors)). Instantiation starts `pending` empty and takes the templates from the PremiseComposition ([03 §15.3](03_premise.md#153-agreements-in-a-premise-and-in-play)). Pending transactions are consumed at the round boundary, and the kernel's spawns append to the list as a run goes.

A run folds the round runner over the World for the configured number of rounds. The live, simulated, and replayed runs share one round runner and one set of effect rules, and only their handlers differ ([02 §8.4](02_kernel.md#84-effects-and-handlers)). A runner plays only what assembly builds ([§10](#10-initialization-pipeline)).

The Run manifest is the immutable record of the resolved configuration and initial World. It holds:

- the PremiseComposition's identifier and version, and its Premises in order, each with its version;
- the engine and draw-scheme versions;
- the initial World, with every controller, procedure, Representation, Exchange, Rule, and the game seed.

It does not hold the Strategies, the round count, or the player seed. The run's spec holds those ([07 §3](07_simulator.md#3-simulation-specification)). Premises change during play only through supported mechanisms, never by editing the manifest.

A run starts only after instantiation succeeds. A failed instantiation leaves no state ([§10](#10-initialization-pipeline)). Run states and archiving are in [07 §6](07_simulator.md#6-lifecycle-and-execution).

## 10. Initialization Pipeline

This section gives the ordered steps that turn a PremiseComposition and configuration into an initial World, and the errors each step can raise. Instantiation either yields the initial World and manifest, or the full list of errors with nothing created.

Instantiation is one pure, atomic function. It takes a PremiseComposition and a configuration, and returns the initial World or every error. Decoding, merging, and validation of the PremiseComposition run before it ([03 §12.1](03_premise.md#121-load-pipeline)). It runs these steps:

1. **Checks.** Every required Actor is selected. Seats go only to Actors that allow them. Non-playable Actor Strategies appear in their Actor's list. Each scheme exists, and each representing Actor is eligible and selected. Errors: `RequiredNotSelected`, `ControllerNotAllowed`, `UnknownScheme`, and `IneligibleRepresentative`. Any error returns the full list, and no world is produced.
2. **Controllers.** Each selected Actor receives its seat or in-world Strategy as its controller.
3. **Fallbacks.** Each unselected Actor is resolved by its fallback, in Actor-id order ([§3.2](#32-fallback-resolution)). Coverage merges run as in [§4.2](#42-partitioning).
4. **Seed and Representations.** The World takes the game seed. Each Representation is built from its scheme and weight ([§5](#5-representation-assignment)).
5. **Manifest.** The result is the World and its manifest.

Assembly builds what a runner plays. It resolves the overrides, runs instantiation, and then checks the seat bindings. Its errors are:

| Error | Raised when |
|---|---|
| `Invalid` | Instantiation or validation fails. It carries the validation errors. |
| `Unresolved` | An override or bound `StrategyRules` name something the PremiseComposition lacks. |
| `UnboundSeat` / `NotASeat` | A configured seat has no binding, or a binding names something that is not a configured seat. |
| `LiteralOverrides` | Overrides are given to a literal PremiseComposition. |
| `RoundsOutside` | A launch's round count is outside the round range. |

## 11. Round Execution

This section gives the order of one round as the runner executes it. Propose and Sign are the round runner's vocabulary: it gathers candidate Transactions and signatures, and the engine screens them and compiles them to transactions ([03 §14](03_premise.md#14-compilation-to-the-kernel)). A live session runs through the live runner. Each seat's binding answers. A Human seat gives its transcript in the propose window, and an Agent seat answers through its own model call. A `StrategyRules` or Script seat answers from its binding. An in-world Actor answers through its Strategy.

The runner performs the steps in this order. Steps 3 to 6 and 11 are the runner's own work. Steps 1, 2, and 7 to 10 are engine phases that the runner calls. The phases and their guarantees are defined in the canonical sequence ([02 §8.1](02_kernel.md#81-canonical-resolution-sequence)).

1. **Prepare (phase 1, engine).** The engine consumes every pending transaction due at the boundary, in phase then tier order: scheduled Flows, Rule changes scheduled at the last boundary, and any authored successors. Each move or change is recorded as an event, and each consumption can spawn successors.
2. **Observe (phase 2, engine).** Each Actor's View is built from the prepared state.
3. **Speak.** Each Actor without an in-world Strategy speaks: a human's transcript, an AI agent's model call, or silence. `StrategyRules`, Script, and unbound seats are silent.
4. **Joint pass.** If any transcript exists, one joint pass reads all of them together into candidate Transactions ([§11.2](#112-live-session)). If none exists, the round makes no model call.
5. **Propose (phase 3a).** Every Actor, in Actor-id order, drafts candidate Transactions against the same beginning-of-round View. An in-world Strategy drafts from its View, and a seat drafts through its binding. The joint candidate Transactions are numbered after each author's own, so replay assigns the same ids. They join the drafted candidate Transactions.
6. **Sign (phase 3b).** Every Actor is asked to sign the candidate Transactions it may sign: non-Aggregate candidate Transactions that are not its own and that name it as a signer. Human and Agent seats give no separate signature. The joint pass records their agreement.
7. **Screen (phase 4, engine).** The engine checks authority and signatures. Failures are recorded as `Rejected` or `Lapsed`.
8. **Estimate (phase 5, engine).** Each Representation's factor is computed from the validated candidate Transactions and stored for the next round.
9. **Fund, settle, and circulate (phase 6, engine).** Accepted actions settle as one batch. The recurring cycle is then evaluated on the batch result, in the order of [03 §5.4](03_premise.md#54-recurring-cycle).
10. **Commit (phase 7, engine).** The net change is applied to every Actor and Exchange, and the round index advances. A negative holding or reserve fails the round.
11. **Record.** The runner appends the round record ([§11.1](#111-round-record)) with the transcripts its joint pass read. The Run Store saves them ([07 §8](07_simulator.md#8-persistence)).

### 11.1 Round record

A round record holds the round number, its inputs, and its events.

- **Inputs**: every submitted signed candidate Transaction, with its signatures, including those that later lapsed or were rejected, in the authored vocabulary. The transactions compiled from them are not stored: replay recompiles them from the inputs, and the events say what was dropped.
- **Events**: the outcomes of the round, whose kinds are the `Event` constructors in `Laws.World`. Events record executed quantities. A partial fill appears as a smaller settled Flow, and a shortfall as moved less than owed on a settled Flow.

Replay answers seats from the recorded candidate Transactions and signatures, so it makes no model call ([07 §4](07_simulator.md#4-seat-strategies)).

### 11.2 Live session

Live play turns transcripts into candidate Transactions and signatures. Players give no input after their transcript. A session is run from the console (`just session`) or hosted by the server, through the session routes: `POST /api/sessions` starts one, `GET /api/sessions/:id` reports its round and who has spoken, and `POST /api/sessions/:id/transcripts` takes a seat's text. Their contract is in [07 §8.2](07_simulator.md#82-external-services). Both use the same round rules and one path into a round.

- **Human players.** Each human gives one transcript per round, within the propose window: typed at the console, or posted once per seat per round to the session route, usually by the external speech component. Nothing further is asked: no clarification and no confirmation. Inadmissible Intents are rejected by the engine with their reason ([§11.1](#111-round-record)).
- **Joint pass.** One model call reads all the transcripts together into candidate Transactions. If the reply does not decode, the pass makes up to two corrections, so at most three calls. If the last reply still does not decode, the pass yields no candidate Transactions, each human is told that nothing was submitted, and the round proceeds.
- AI agent seats speak in the same round pass. Their transcripts are read with the human transcripts.
- **Signatures.** Negotiation happens out of band, and human and AI agent seats give no separate signature. Each speaker whose transcript describes a candidate Transaction counts as signing it. The joint pass reconciles the transcripts into one candidate Transaction.
- `StrategyRules` and Script seats answer through their binding.

Admissibility is enforced by the engine at screening, whichever source produced a candidate Transaction ([03 §12.4](03_premise.md#124-static-refusals)).

## 12. Validation

Validation runs instantiation as a feasibility check, and adds the PremiseComposition's checks. The checks are in [03 §12](03_premise.md#12-validation-and-compilation). The simulator instantiates a spec's overrides with the same function, without re-running validation ([07 §3.1](07_simulator.md#31-configuration-inheritance)). The invariants a valid Run must satisfy are in [02 §9](02_kernel.md#9-architectural-invariants-and-numerics).

Initialization fails if any checked condition fails. A configuration is never repaired silently. No Actor changes meaning, no unrelated Strategy is assigned, and no resources are invented.
