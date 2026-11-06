# Scenario Planning Game — Simulation Harness and Dashboard Specification

**Version:** 0.2  
**Scope:** Automated Simulation, Persistence, Testing, and Gameplay Analysis  
**Implementation language:** PureScript  
**Reference scenario:** The Fractured World

---

## 1. Purpose

The Simulation Harness plays complete games automatically and deterministically. It is used to check implementation correctness and to improve gameplay through repeated experiments.

The harness runs on the existing Type System, Gameboard, and Instantiated Game machinery. It does not change the economic rules and does not introduce a separate simulation model. Terms follow the Glossary, and invariants are cited by their IDs in the Type System specification ([§2.5](02_type_system.md#25-glossary) and [§9](02_type_system.md#9-architectural-invariants-and-numerics)).

The harness lets a developer:

1. Select a Gameboard and configure an Instantiated Game.
2. Bind a seat Policy (preset program, AI agent, or recorded human) to every seat.
3. Run a whole game automatically, including NPC and Aggregate behavior.
4. Save complete runs persistently.
5. Inspect balance sheets and how the world economy evolved.
6. Compare saved runs, including controlled counterfactuals.
7. Vary parameters to investigate correctness, balance, and player agency.

The harness should also expose architectural defects. Inconsistent accounting, duplicated activity, invalid settlement, or an unexplained outcome must fail the run explicitly. It must never be silently compensated for.

## 2. Position and Separation of Concerns

The harness is a development component, not a fourth layer of economic semantics.

**Gameboard + Instantiation Configuration + Simulation Spec → Simulation Run → Run Store → Dashboard**

| Component | Responsibility |
|---|---|
| **Simulation Harness** | Binds seat Policies, drives the round runner and the engine, and checks assertions. |
| **Run Store** | Persists manifests, round records, snapshots, and derived metrics. |
| **Analytics Dashboard** | Read-only presentation of runs, trajectories, comparisons, and diagnostics. |

The harness invokes the same instantiation and resolution operations used in live play. Simulated and live games therefore use the same engine and interpreter ([INV-29](02_type_system.md#inv-29)). A simulated game is economically equivalent to a live game in which humans submitted the same canonical Intents.

The harness is a set of effect handlers (Type System [§8.4](02_type_system.md#84-effects-and-handlers), [INV-36](02_type_system.md#inv-36)). Its simulation handler interprets `SeatInput` with bound seat Policies (their randomness drawn from the player seed) and `Record` with the Run Store. The replay handler feeds recorded Proposals and signatures instead. The round runner and engine code are the same as in live play.

The engine remains responsible for everything economic: accounting, validation, Policies, Representation, collective procedures, settlement, transformations, and rule changes. The harness only generates inputs, records results, and evaluates them. The Dashboard is read-only and never alters authoritative state or run histories. Changing any parameter creates a new run.

Narrative reporting is optional in simulation. Analysis reads authoritative engine data, never language-model summaries.

---

# Part I — Simulation Model

## 3. Simulation Specification

A Simulation Specification is a serializable, versioned description of one complete automated playthrough.

```purescript
type Context = { gameboard :: Gameboard, config :: InstantiationConfig, presets :: Object Program }

type SimulationSpec =
  { seats :: Map EntityId SeatPolicy    -- Preset name or a Custom program; unlisted seats submit nothing
  , playerSeed :: Seed
  , overrides :: Array Override         -- GameSeed, Parameter, Capacity, Shrinkage, Implicit
  , rounds :: Maybe Int }               -- defaults to the configuration's round count
```

The specification must be enough to reproduce the complete economic history with the same engine version. The game seed lives only in the `InstantiationConfig`. The player seed drives all seat-Policy randomness and is recorded in the run manifest.

### 3.1 Configuration inheritance

A specification may reference a baseline and apply declarative overrides. For example: start from `fractured-world-test-01`, raise Energy production capacity by 20%, cut procurement capacity from 30 to 20, set SME estimator shrinkage to 0.40, and give the US the `Aggressive` preset.

Overrides must respect the layer boundaries. Tuning an instance changes declarative data, never engine code:

- Changing a value within the permitted range is an Instantiation change.
- Changing the definition of a Market or Transformation requires a Gameboard variant.
- Neither change touches the Type System.

Each resolved specification is saved in full, so later changes to a baseline never alter historical runs.

## 4. Seat Policies

The engine accepts only Intents (Type System [§4](02_type_system.md#4-policies)). A **seat** is an Actor whose Policy is supplied from outside the scenario; an NPC's Policy is part of the scenario. In simulation the harness binds each seat to a **seat Policy**. Swapping a seat's Policy never changes the Actor's definition and never turns a seat into an NPC ([INV-30](02_type_system.md#inv-30)).

Seat Policies come in the three kinds of Type System [§4](02_type_system.md#4-policies) (preset program, AI agent, human). In simulation the human kind is replay of recorded canonical Proposals and signatures from a live session. Preset programs run on the shared decision-language interpreter used by NPC and Aggregate Policies ([INV-29](02_type_system.md#inv-29)). AI agents and presets may be given their Actor's objective ([§9.3](05_test_instance.md#93-objectives)).

Every kind sees only its Actor's authorized view ([INV-16](02_type_system.md#inv-16)) and maps it to Intents:

**Seat Policy + Actor View + Turn → Proposals (3a) and signatures (3b)**

Accepted Intents are always recorded, so every run replays from its round records; preset-only runs can also be regenerated from the game and player seeds. Seat Policies never mutate World State, and every Intent passes through ordinary validation and settlement. Seat-Policy memory lives in the run record, not World State, because seat Policies are not part of the game world.

### 4.1 Propose and sign

Each seat Policy acts twice per round, against the same snapshot (Type System [§3.3](02_type_system.md#33-proposals-and-signatures), [§8.1](02_type_system.md#81-canonical-resolution-sequence)). In **Propose** it publishes Proposals, including offers that need other signers. In **Sign** it sees the Proposals addressed to it and signs those it endorses; an author's own signature is implicit in publishing. Preset programs sign by rule, for example "sign any Energy offer at ≤ 2.6 M/E". AI agents read the Proposals and choose. A Proposal missing a signature lapses ([INV-10](02_type_system.md#inv-10)); the round still resolves, and the lapse is recorded as an ordinary outcome. Counteroffers are new Proposals, signable next round.

Illustrative preset seat Policies:

- **US:** keep at least 50 E, buying with a 3.0 M/E limit. Procure Military when Chinese Military exceeds a threshold. Spend up to 15 M per year on Public Attention while Energy is below 3. Keep at least 40 M in reserve.
- **Germany:** keep at least 25 E. Sign the EU's `eu-solidarity` procedure Proposals.
- **SA and JP:** SA proposes 20 E for 48 M to JP; JP's sign rule accepts Energy offers at ≤ 2.4 M/E ([Fixture G](05_test_instance.md#27-fixture-g-proposals-and-signatures)).
- **SME:** cut Energy purchases above 3 M/E. Raise them below 2.2 M/E. Keep operating cash.

### 4.2 Presets

Preset programs are named parameter sets of the shared interpreter, not separate programs:

| Preset | Behavior |
|---|---|
| `Passive` | Submits no discretionary actions. |
| `Conservative` | Maintains reserve targets and buys only below a limit price. |
| `Aggressive` | Prioritizes Military procurement and restrictions against rivals. |
| `Accumulator` | Buys a target Resource up to a holding target. |
| `Trader` | Buys below and sells above configured price bands. |
| `Cooperative` | Proposes configured deals, signs offers within configured terms, and signs procedure Proposals. |
| `Opportunistic` | Proposes and signs deals only while their terms are favorable at the snapshot. |
| `Investor` | Spends surplus on capacity-building Transformations. |
| `CrisisResponse` | Switches to reserve preservation when a declared stress indicator triggers. |
| `RandomValid` | Picks among valid actions using seeded draws. |

### 4.3 Coverage

Every seat has exactly one bound seat Policy. A seat Policy that produces nothing yields an explicit `NoAction`. The harness never invents default decisions. A diagnostic mode may deliberately leave seats inactive.

## 5. Deterministic Randomness

Mechanics are deterministic by default. Randomness enters only as a keyed `draw` on a seed, where a Gameboard, an estimator, or a preset program declares a stochastic choice ([INV-13](02_type_system.md#inv-13), [INV-36](02_type_system.md#inv-36)).

```purescript
simulate
  :: ValidGameboard
  -> SimulationSpec
  -> Either SimulationError SimulationRun
```

Draws come from two streams (Type System [§9.1](02_type_system.md#91-numerics-and-randomness)):

| Stream | Seed | Used for |
|---|---|---|
| `Game` | Game seed (`InstantiationConfig`) | Every in-world draw: estimator sampling noise, declared Policy randomness, declared shocks |
| `Player` | Player seed (`SimulationSpec`) | Every seat-Policy draw: `RandomValid` and any stochastic preset. AI agents receive it where their provider supports seeding, but are still reproduced from recorded input |

The streams never share draws. Varying the player seed never changes an in-world draw, and varying the game seed never changes seat-Policy behavior. Live play has no player seed.

- **Player-seed distributions:** fix the game seed and vary the player seed to see the spread of outcomes caused by player behavior alone, with everything else held constant.
- **Game-seed variation:** fix the player seed and vary the game seed to see in-world stochastic variation.
- **Counterfactuals** fix both seeds, so differences come only from the intervention.

Conversion of random values into quantities uses the Type System's fixed-point rule ([§9.1](02_type_system.md#91-numerics-and-randomness)).

---

# Part II — Execution and Assertions

## 6. Lifecycle and Execution

Runs move through these states: **Configured → Validated → Initialized → Running → Completed**, or **Failed**. A failed run keeps its last valid state and diagnostics.

Each round follows the canonical sequence (Type System [§8.1](02_type_system.md#81-canonical-resolution-sequence)). The harness drives the same round runner and engine as live play, with seat Policies bound:

| Canonical phase | Harness role |
|---|---|
| 2. Create observations | Passes each seat its authorized view. |
| 3a. Propose / 3b. Sign | Drives the round runner, which runs the bound seat Policies alongside the NPC and Aggregate Policies on the shared interpreter. |
| 7. Commit and record | Persists the round record, snapshot, and assertion results. |

The engine performs phases 1–2 and 4–7. The harness never evaluates NPC or Aggregate Policies itself, computes Representation estimates or procedure support, or repeats Aggregate actions; it delegates to the round runner, which is the same code used in live play.

The harness exposes two operations, **run to completion** and **step one round**. Both use the same execution path.

### 6.1 Invalid actions and failures

An invalid action is an ordinary outcome. It is recorded with its reason and handled by the engine's deterministic reject or partial-fill rules (Type System [§3.2](02_type_system.md#32-infeasible-actions)). The round record keeps both the proposed and the executed action. A representative's observation comes from the proposal, even when its fill differs.

An invariant violation fails the run. The harness preserves the configuration, last valid state, failing round, Intents, Policy, Representation, and procedure outputs, the violated invariant, and the relevant DrawKeys. A failure therefore becomes a reproducible regression test.

## 7. Run Assertions

Assertions are evaluated after initialization and after every round. They check engine properties without reimplementing the engine.

### 7.1 Required assertions

Each required assertion checks exactly one invariant from the Type System master table:

| Assertion | Invariant |
|---|---|
| Unified action semantics | [INV-01](02_type_system.md#inv-01) |
| Authority separation | [INV-02](02_type_system.md#inv-02) |
| No authority amplification | [INV-03](02_type_system.md#inv-03) |
| Resource consistency | [INV-04](02_type_system.md#inv-04) |
| Stock-flow reconciliation | [INV-05](02_type_system.md#inv-05) |
| Market solvency | [INV-06](02_type_system.md#inv-06) |
| Price is not production | [INV-07](02_type_system.md#inv-07) |
| Account partition | [INV-08](02_type_system.md#inv-08) |
| Representation consistency | [INV-09](02_type_system.md#inv-09) |
| Signature completeness | [INV-10](02_type_system.md#inv-10) |
| Order independence | [INV-11](02_type_system.md#inv-11) |
| Policy reproducibility | [INV-12](02_type_system.md#inv-12) |
| Deterministic replay | [INV-13](02_type_system.md#inv-13) |
| Resolution consistency | [INV-14](02_type_system.md#inv-14) |
| Single authoritative state | [INV-15](02_type_system.md#inv-15) |
| Information isolation | [INV-16](02_type_system.md#inv-16) |

Harness invariants [INV-29](02_type_system.md#inv-29) to [INV-31](02_type_system.md#inv-31) are checked by the harness's own test suite rather than by per-round assertions.

Conservation checks read each Resource's declared properties and the Transformations that produce or consume it ([INV-04](02_type_system.md#inv-04), [INV-05](02_type_system.md#inv-05)). They do not hard-code the reference Resources. Soft Power is produced and decays. Energy is produced and consumed. Military is reserved by deployment, lost through declared attrition, and restored by procurement. Money is conserved unless authorized issuance occurs. A blanket "totals are constant" assertion would be wrong.

Each assertion reports: pass or fail, round, Entity or Market, expected versus observed quantities, the relevant events, and an explanation. Results are visible in the test runner and in the Dashboard.

### 7.2 Metamorphic tests

These tests transform a configuration in a controlled way:

- Reorder submissions ([INV-11](02_type_system.md#inv-11)).
- Replay with the same game and player seeds ([INV-13](02_type_system.md#inv-13)).
- Remove all representative observations; Aggregates must follow their baselines ([INV-09](02_type_system.md#inv-09)).
- Withhold one signature; the Proposal lapses and the round still resolves ([INV-10](02_type_system.md#inv-10)).
- Change one seat Policy or one Market parameter.
- Change explicitness ([§7.3](#73-resolution-consistency-test)).

### 7.3 Resolution-consistency test

This test is the direct check of [INV-14](02_type_system.md#inv-14).

Instantiate the reference board twice:

- **A:** German government activity stays implicit in the EU.
- **B:** The German Chancellor is explicit. Only government accounts are extracted; German households and firms stay in the EU residual.

Before any decisions differ, the test checks that stocks reconcile, that every liability and capability has exactly one holder, and that behavioral responsibilities are covered exactly once.

The test compares A and B at initialization, where any difference is an accounting defect. It does not then play B with a Germany seat that reproduces the implicit baseline exactly: in A the implicit EU spends Germany's flows internally, so no seat Policy can match it action for action.

### 7.4 AI interpretation tests

Text-to-Intent interpretation is tested separately. The harness consumes canonical Intents and never calls the language model.

The interpretation suite contains:

- text inputs paired with their expected canonical Intents;
- materially ambiguous cases, where the expected output is a clarification request;
- inadmissible cases, such as claimed resources or authority the Actor lacks, where the expected output is a flagged rejection.

Accepted canonical Intents are the replay boundary.

---

# Part III — Run Store

## 8. Persistence

Every run is persisted and survives restarts.

| Field | Contents |
|---|---|
| **Run ID, name, status** | Identity and lifecycle state. |
| **Manifest** | Immutable resolved configuration, initial World State, seat-Policy bindings, game and player seeds, and exact versions of the Gameboard, Policies, seat Policies, engine, numerics, and PRNG. Also overrides and a configuration digest. |
| **Round records** | Append-only Proposals, signatures, lapsed Proposals, proposed and executed actions, procedure Proposals with their support and outcome, stored Representation estimates, and events. |
| **Snapshots** | World State after initialization and after each round. |
| **Assertions** | Results and diagnostics. |
| **Metrics** | Derived projections, tagged with the version of the metric definitions. |

**Source of truth ([INV-31](02_type_system.md#inv-31)).** The manifest, round records, and snapshots are authoritative. Metrics are reproducible projections of them. When a metric definition changes, its values are recomputed and the records stay untouched. Snapshots support fast inspection; records explain outcomes. Neither replaces the other. Every outcome traces to its inputs, Policies, and configuration.

Completed runs are immutable, and a changed configuration always gets a new Run ID.

Live sessions also keep evidence of each language-model call: the effect, model ID, template and input hashes, a pointer to the raw output, and the decoded result. This evidence lives beside the run, outside the round records. Replay never re-invokes the model; the canonical Proposals and signatures in the round records are enough.

The initial record is written before Round 1. After each round, the round record, snapshot, and assertions are saved atomically. If Round 4 fails, Rounds 1–3 remain available, and the run can be resumed from its last snapshot where that is supported.

A stored seed is insufficient without the versioned PRNG, which is why the manifest records it. Run IDs never influence sampling.

Initial storage is a local database such as SQLite, accessed through a Run Store interface. That interface supports save, retrieve, list and filter, full round histories, metric series, labels, resume, and explicit archive or delete. The Dashboard reads through it and never touches engine internals.

---

# Part IV — Dashboard

## 9. Pages

The Dashboard has three pages: **Simulation Runs**, **Run Details**, and **Comparison**. It renders from persisted data without running the engine.

### 9.1 Simulation Runs

The page shows a sortable, filterable table with these columns: name, Gameboard version, configuration, game seed, player seed, rounds completed/total, status, assertion counts, created date, and actions.

The available actions are:

- **Run**
- **Duplicate:** edit a copy of the configuration.
- **Rerun:** same configuration and both seeds.
- **Variant:** change selected parameters.
- **Counterfactual** ([§10.4](#104-counterfactual-experiments))
- **Inspect**
- **Compare**

The new-simulation form selects the Gameboard, the configuration, seat Policies or presets, permitted parameters, and both seeds. It separates Gameboard-level parameters from Instantiation-level ones. A Gameboard change creates or selects a Gameboard variant.

### 9.2 Run Details

**Overview.** Shows identity, versions, rounds, seat Policies, both seeds, assertion results, and summary metrics. For the reference game these include the regional Energy prices, total procurement, Soft Power produced, and rejected Intents. These are descriptive measures, not victory conditions.

**Timeline.** Lets the developer select any round boundary (Initial → Year 1 … Year 5) and play back saved states. Playback is visualization only, not re-execution.

**Balance sheets.** One balance sheet for every explicit Actor and every Aggregate. Aggregates are clearly marked as residual representations.

| Resource | Initial | Current | Reserved | Change |
|---|---:|---:|---:|---:|
| Money | | | | Δ |
| Energy | | | | Δ |
| Military | | | | Δ |
| Soft Power | | | | Δ |

"Reserved" shows deployed capacity. Each balance sheet also lists the round's flows (production, income, expenditure), signed deals, and non-Resource state such as capacities and market access.

Holdings are not collapsed into a single net-worth figure, because Military and Soft Power have no market price in the Gameboard. Trajectory charts can show any set of Actors and Resources, in absolute terms or as change from the start, and always in their own units.

**Markets and facilities.**

- Each regional Energy Market view shows: marginal price, executed volume, buy and sell volumes, orders left unfilled or blocked by their limit price, reserves, position, and the tariffs and bans in force.
- Facilities (Defense Procurement, Public Attention, Energy Shipping) have their own views: orders, capacity and utilization, cost, deliveries, rejected Proposals, and Money paid to each payee.
- Every view separates explicit activity from Aggregate activity.

**World economy.** This view covers:

- Resource totals across all accounts, with production, consumption, issuance, and decay shown separately.
- Activity and unfilled demand.
- Circulation: Money flows between Entity categories, with accounts that only accumulate flagged ([INV-05](02_type_system.md#inv-05)).
- Concentration.
- Explicit versus implicit share.
- Pressure indicators: shortages, price moves, capacity utilization, rejections.

It does not invent macroeconomic indicators the Gameboard does not define.

**Representation.** One view covers every active Representation. It shows the four-stage record, which spans two rounds:

1. **Observation** (round r), from the validated, escrowed Proposal.
2. **Estimate** (round r): the stored adjustment factor, the parameters (weight, reference, shrinkage, noise spread, bound), the noise draw, and whether the bound applied.
3. **Proposed Aggregate Intent** (round r+1), from the Aggregate's Policy applying the stored factor.
4. **Executed result** (round r+1), shown beside the representative's own proposed and executed actions.

The view reports the **Representation influence ratio**: the induced change in Aggregate activity divided by the resources the representative directly controls. This is a balancing parameter, not a claim that the representative is an unbiased sample.

**Collective procedures.** A separate view covers every procedure an Aggregate holds. Per round it shows each procedure Proposal, the members who signed, the support against the threshold, and the outcome (executed or lapsed), all within that round. An optional resource contribution appears separately, as a Transfer.

**Objectives.** For each Actor, the view lists its objective criteria (Gameboard [§10.5](03_gameboard.md#105-evaluation)) as met or not met, with the measured values. Criteria are evaluated by a pure function over the manifest and round records and never affect the economy.

**Event inspector.** For any round, it lists in canonical phase order:

- round preparation;
- Proposals, signatures, and lapses;
- seat Intents and signatures, including signatures on procedure Proposals;
- NPC and Aggregate outputs;
- procedure Proposals with support, threshold, and outcome;
- stored Representation estimates, which Aggregates apply in the next round;
- validation results, including partial fills and their allocation basis;
- the economic cycle and settlement;
- resource changes;
- triggered assertions.

## 10. Comparison

Comparison is a core feature. It supports both correctness testing and balancing.

### 10.1 Selection and compatibility

The developer selects two to eight runs and picks a baseline. Larger batches are shown as statistical summaries. Metrics can be filtered by Actor, Aggregate, or Market.

The Dashboard flags runs that are not directly comparable: different Gameboard versions, units, round counts, Actor selections, Market definitions, or engines. Incompatible configurations and metrics are flagged, never combined. Trajectories are aligned by simulated time, with no interpolation that has no basis in the data.

### 10.2 Views

- **Trajectory:** overlay one metric across runs, for example regional Energy price, US Money, or Global SME demand under different shrinkage values.
- **Final state:** ending balances and measurements side by side.
- **Difference from baseline:** absolute and, where meaningful, percentage differences. Percentages are omitted when the baseline is zero.
- **Configuration diff:** the exact parameter differences, with deliberate changes distinguished from version, seat-Policy, or seed differences.

| Parameter | Baseline | Variant |
|---|---:|---:|
| Atlantic Energy reserves | 350 E | 450 E |
| SME estimator shrinkage | 0.25 | 0.40 |
| Game seed | 26021 | 26021 |
| Player seed | 1 | 1 |

### 10.3 Actor and whole-economy views

Any Actor's or Aggregate's trajectory can be compared across runs; for example, German Energy under Baseline, High Scarcity, and Strong EU Weight. Whole-economy metrics are also available, including demand, prices, procurement, Soft Power, concentration, unfilled demand, and the Money distribution. These separate a seat Policy that helps one Actor from a configuration that destabilizes the whole economy.

### 10.4 Counterfactual experiments

The counterfactual operation works as follows:

1. Duplicate a run's resolved configuration and both seeds.
2. Change exactly one seat Policy or one parameter.
3. Replay the run.
4. Compare it against the original as the baseline.

In the Dashboard, a variant or counterfactual is launched as an ordinary run (preset, player seed, overrides) and then opened beside its source in the comparison view. Both seeds and all DrawKeys are held fixed, so the difference measures the intervention's total modeled effect, including downstream consequences. Counterfactual runs are ordinary immutable runs, linked to their source run.

Typical questions:

- How much did Germany's signature change EU behavior?
- How much did the SME shift Global SME demand?
- What happens without the US deployment?
- Does more Energy liquidity help smaller Actors?

### 10.5 Agency analysis

For each Actor, the harness replays the game with alternative presets for that Actor while holding everything else fixed. It then measures changes in:

- prices;
- other Actors' holdings;
- Aggregate behavior;
- institutional decisions;
- the Actor's own objective criteria ([§9.3](05_test_instance.md#93-objectives)).

The Dashboard shows an agency profile for each role, with its objective criteria under each alternative. A role whose alternatives produce negligible differences on every measure is flagged as a balancing finding. Agency matters more than equal starting resources.

### 10.6 Saved comparisons

Selections, baseline, metrics, and display preferences persist as Dashboard state. Named comparison sets (for example "Energy Scarcity Experiment") reference immutable Run IDs and never copy run data.

---

# Part V — Experimentation

## 11. Experiments

A **Variant Experiment** generates one specification for each declared parameter value, runs every resulting game, saves the runs, and opens them as a comparison set.

- **Single-parameter experiments:**
  - Energy curve slope (0.01, 0.02, 0.04, 0.08)
  - SME estimator shrinkage (0, 0.10, 0.25, 0.50)
  - Germany's procedure weight, with the residual weight complementary
  - Procurement capacity and attrition rate
  - Production capacities, revenue rates, and wages: does the economy sustain itself over five years, or does it deplete or grow without bound?
- **Multi-parameter experiments:** controlled combinations, run as batches, for example low supply with aggressive procurement.
- **Seat-Policy experiments:** hold parameters fixed and vary presets. Examples: all Conservative; governments Aggressive with corporations as Investors; one deviator among Cooperative players.
- **Player-seed distributions:** fix the game seed and run a batch of player seeds to estimate the distribution of outcomes from player behavior alone.
- **Game-seed variation:** fix the player seed and vary the game seed to measure in-world stochastic variation.

The Dashboard separates variation caused by configuration, by seat Policy, by the player seed, and by the game seed.

## 12. Correctness Versus Gameplay Quality

**Correctness** is established by the assertions ([§7](#7-run-assertions)) and by the mandatory tests ([§14](#14-mandatory-tests)).

**Gameplay quality** asks whether the world produces interesting choices. For example: do small players matter, do governments have reasons to negotiate, are shortages consequential without being permanently disabling, does cooperation pay, can several strategies succeed, do early decisions matter later, and is the economy responsive without excessive volatility?

The Dashboard supports these judgments with indicators. It does not replace human playtesting.

| Indicator | Interpretation |
|---|---|
| Actor activity | Frequency and scale of accepted actions |
| Market liquidity | Share of proposed trades executed |
| Resource exhaustion | Actors unable to act feasibly |
| Influence distribution | Concentration of economic and institutional influence |
| Strategic diversity | Variety of chosen and successful action patterns |
| Policy responsiveness | NPCs and Aggregates react when relevant conditions change |
| Representation and procedure impact | Attributable Aggregate change, influence ratio, and procedure outcomes changed by member signatures |
| Deal activity | Multi-signer Proposals published, signed, and lapsed |
| Agency | Per-Actor sensitivity to seat-Policy changes ([§10.5](#105-agency-analysis)) |
| Circulation health | Money and Energy keep moving rather than pooling |
| Path dependence | Early decisions shape later opportunities |

Evaluation combines each Actor's measurable objective criteria with system-level metrics. There is no single winner, and outcomes are not reduced to Money.

---

# Part VI — Reference Test Suite

## 13. Reference Coverage

The initial suite runs *The Fractured World* (`fractured-world-test-01`). Its contents are defined in the reference test instance specification and are not restated here.

Every one of the fifteen seats receives a simple, predictable preset seat Policy, so its actions can be checked independently.

## 14. Mandatory Tests

Tests that exercise a reference fixture name it and do not re-describe it. Each test passes when the cited fixture's expected values hold and the listed invariants pass.

| # | Test | Procedure | Pass condition |
|---|---|---|---|
| 1 | Full game | Three preset configurations: all `Conservative`; all `Passive`; `RandomValid` across two player seeds with the game seed fixed. | Five valid rounds with [INV-01](02_type_system.md#inv-01)–16 passing; the cycle sustains five years ([INV-05](02_type_system.md#inv-05), [INV-07](02_type_system.md#inv-07)); the same seeds reproduce, and different player seeds give different valid histories under one game seed ([INV-13](02_type_system.md#inv-13)). |
| 2 | Replay | Run Test 1 twice with the same game and player seeds, and once more under the replay handler from recorded input. | Identical records, snapshots, and final state ([INV-13](02_type_system.md#inv-13)). |
| 3 | Submission order | Permute the order of Intent supply. | Identical settlement ([INV-11](02_type_system.md#inv-11)). |
| 4 | Market exchange | Runs [Fixture A](05_test_instance.md#21-fixture-a-direct-market-exchange), including its limit-price variant. | Fixture values; [INV-04](02_type_system.md#inv-04), [INV-06](02_type_system.md#inv-06). |
| 5 | Germany and EU | Runs [Fixture B](05_test_instance.md#22-fixture-b-germany-and-the-eu-collective-procedure) as a counterfactual pair: Germany signs vs does not sign. | Fixture values; the procedure Proposal executes in the same round only when support exceeds the threshold ([INV-10](02_type_system.md#inv-10)); signing moves no resources ([INV-03](02_type_system.md#inv-03)). |
| 6 | SME Representation | Runs [Fixture C](05_test_instance.md#23-fixture-c-small-business-owner-representation) (C1–C4 noise-free, C5 seeded noise) with shrinkage 0 and 0.25. | Fixture values, with the factor applied in the following round; C5 target within range and identical on replay; [INV-09](02_type_system.md#inv-09), [INV-13](02_type_system.md#inv-13). |
| 7 | NPC response | Runs [Fixture D](05_test_instance.md#24-fixture-d-china-responds-as-an-npc) as a counterfactual pair: with and without the deployment. | Fixture values; [INV-12](02_type_system.md#inv-12). |
| 8 | Scarcity | Runs [Fixture E](05_test_instance.md#25-fixture-e-scarcity-and-trade-restriction) (Round 3 disruption and regional prices). | Qualitative: the fixture's expected propagation holds; [INV-05](02_type_system.md#inv-05), [INV-06](02_type_system.md#inv-06). |
| 9 | Conflicting actions | Runs [Fixture F](05_test_instance.md#26-fixture-f-simultaneous-conflicting-orders). | Fixture fills; [INV-11](02_type_system.md#inv-11). |
| 10 | Signed Proposals | Runs [Fixture G1](05_test_instance.md#27-fixture-g-proposals-and-signatures), G2, G3, and G5. | Fully signed Proposals settle atomically with fixture values; unsigned ones lapse while the round resolves; a shipped deal pays SHIP and is rejected as a unit under an `AccessBan` ([INV-10](02_type_system.md#inv-10)). |
| 11 | Trade restriction | The US enacts an `AccessBan` on China, effective next round ([Fixture E2](05_test_instance.md#25-fixture-e-scarcity-and-trade-restriction)). | Shipping orders on the route become infeasible; the EU publishes and passes its tariff Proposal in the round the ban is in force, and the tariff applies from the next round. |
| 12 | Resolution consistency | [§7.3](#73-resolution-consistency-test). | [INV-14](02_type_system.md#inv-14). |
| 13 | Persistence | Save, reload, and inspect all snapshots. | Identical run ([INV-31](02_type_system.md#inv-31)). |
| 14 | Agency | [§10.5](#105-agency-analysis) across all fifteen roles. | Every role has measurable effect and its objective criteria are reported; failures are reported as balancing findings. |

---

## 15. Summary

The Simulation Harness executes complete games without human participants.

Simulated seats are driven by seat Policies (preset programs, AI agents, or recorded humans) that submit ordinary Intents, including multi-signer Proposals for deals. NPCs, Aggregates, Representation, collective procedures, and settlement all run through the unchanged engine. Every run is an immutable manifest plus append-only round records, from which all metrics derive. Assertions map one-to-one onto the master invariants.

The development loop is:

**Configure → Simulate → Inspect → Compare (including counterfactuals and agency) → Adjust → Simulate again**
