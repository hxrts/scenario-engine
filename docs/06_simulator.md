# Simulator and Dashboard Specification

**Version:** 0.3  
**Scope:** Automated Simulation, Persistence, Read API, Dashboard, Live Play, and Tests  
**Implementation language:** PureScript  
**Reference premise:** Meridian

Items marked **Planned** are specified but not implemented. Unmarked behaviour is implemented. The mandatory tests in §14 check the economic and persistence behaviour.

---

## 1. Purpose

The Simulator plays complete games automatically and deterministically. It is used to check implementation correctness and to improve gameplay through repeated experiments.

The simulator runs on the existing Type System, Gameboard, and Instantiated Game machinery. It does not change the economic rules and does not introduce a separate simulation model. Terms follow the Glossary, and invariants are cited by their IDs in the Type System specification ([§2.5](02_type_system.md#25-glossary) and [§9](02_type_system.md#9-architectural-invariants-and-numerics)).

The simulator lets a developer:

1. Select a Gameboard and configure an Instantiated Game.
2. Bind a seat Policy (preset strategy, AI agent, or recorded human) to every seat.
3. Run a whole game automatically, including NPC and Aggregate behavior.
4. Save complete runs persistently.
5. Inspect balance sheets and how the world economy evolved.
6. Compare saved runs, including controlled counterfactuals.
7. Vary parameters to investigate correctness, balance, and player agency.

The simulator should also expose architectural defects. Inconsistent accounting, duplicated activity, invalid settlement, or an unexplained outcome must fail the run explicitly. It must never be silently compensated for.

## 2. Position and Separation of Concerns

The simulator is a development component, not a fourth layer of economic semantics.

**Premise → `assemble` → Game + player seed → Simulation Run → Run Store → Dashboard**

| Component | Responsibility |
|---|---|
| **Simulator** | Folds a `Game` through the round runner and the engine, and checks assertions ([§7](#7-run-assertions)). |
| **Run Store** | Persists manifests, round records, snapshots, and metrics. |
| **Read API and Dashboard** | Serves run views and presents them. Read-only except for the run operations in §8.1. |

The simulator plays the same `Game` that live play runs, assembled by `game`. Simulated and live games therefore use the same engine and interpreter ([INV-29](02_type_system.md#inv-29)). A simulated game is economically equivalent to a live game in which humans submitted the same canonical Intents.

The simulator is a set of effect handlers (Type System [§8.4](02_type_system.md#84-effects-and-handlers), [INV-36](02_type_system.md#inv-36)). The simulation handler (`Handlers.Sim`) interprets `SEAT_INPUT` with bound seat Policies (their randomness drawn from the player seed), and the round writes reach the Run Store through `RECORD`. The replay handler (`Handlers.Replay`) feeds recorded Proposals and signatures instead. The round runner and engine are the same as in live play.

The engine remains responsible for everything economic: accounting, validation, Policies, Representation, collective procedures, settlement, transformations, and rule changes. The simulator only generates inputs, records results, and evaluates them. The Dashboard reads through the read API and never alters authoritative state or run histories. Changing any parameter creates a new run. Resume (§8.1) is the one operation that adds rounds to an existing run.

Narrative reporting is optional in simulation. Views and experiments read authoritative engine data, never language-model summaries.

---

# Part I: Simulation Model

## 3. Simulation Specification

A simulation is one complete automated playthrough, and it is a pure call over an assembled `Game`:

```purescript
simulate :: Seed -> Game -> Either GameError SimRun
```

The `Game` holds the manifest, the initial World State, the in-world Strategies, the rounds to play, the parameters, and one `SeatBinding` per seat. A seat's preset, agent, script, or human is chosen when the game is assembled, not inside the simulator. The player seed is the argument, and it drives all seat-Policy randomness. The game seed is part of the World State. The program boundary resolves the player seed to a concrete value before `simulate` is called (the seed rule in §5), and the run's stored spec records that value.

The stored spec (`Game.Recipe`) records how the run was made: the preset every seat plays, the resolved player seed, the overrides, and the round count. With the same engine version, the spec and the seeds reproduce the complete economic history.

### 3.1 Configuration inheritance

A specification may reference a baseline and apply declarative overrides. For example: start from `meridian-test-01` ([05 §14](05_test_instance.md)), raise Energy production capacity by 20%, cut procurement capacity from 30 to 20, set SME estimator shrinkage to 0.40, and give the US the `Aggressive` preset.

Overrides must respect the layer boundaries. Tuning an instance changes declarative data, never engine code:

- Changing a value within the permitted range is an Instantiation change.
- Changing the definition of a Market or Transformation requires a Gameboard variant.
- Neither change touches the Type System.

Each resolved specification is saved in full, so later changes to a baseline never alter historical runs.

## 4. Seat Policies

The engine accepts only Intents (Type System [§4](02_type_system.md#4-policies)). A **seat** is an Actor whose Policy is supplied from outside the premise. An NPC's Policy is part of the premise. In simulation each seat is bound to a **seat Policy** (its `SeatBinding`) when the game is assembled. Swapping a seat's Policy never changes the Actor's definition and never turns a seat into an NPC ([INV-30](02_type_system.md#inv-30)).

Seat Policies come in the three kinds of Type System [§4](02_type_system.md#4-policies) (preset strategy, AI agent, human). In simulation the human kind is replay of recorded canonical Proposals and signatures from a live session. Preset strategies run on the shared decision-language interpreter used by NPC and Aggregate Policies ([INV-29](02_type_system.md#inv-29)). AI agents and presets may be given their Actor's objective ([§9.3](05_test_instance.md#93-objectives)).

Every kind sees only its Actor's authorized view ([INV-16](02_type_system.md#inv-16)) and maps it to Intents:

**Seat Policy + Actor View + Turn → Proposals (3a) and signatures (3b)**

Accepted Intents are always recorded, so every run replays from its round records. Preset-only runs can also be regenerated from the game and player seeds. Seat Policies never mutate World State, and every Intent passes through ordinary validation and settlement. Seat-Policy memory lives in the run record, not World State, because seat Policies are not part of the game world.

### 4.1 Propose and sign

Each seat Policy acts twice per round, against the same snapshot (Type System [§3.3](02_type_system.md#33-proposals-and-signatures), [§8.1](02_type_system.md#81-canonical-resolution-sequence)). In **Propose** it publishes Proposals, including offers that need other signers. In **Sign** it sees the Proposals addressed to it and signs those it endorses. An author's own signature is implicit in publishing. Preset strategies sign by rule, for example "sign any Energy offer at ≤ 2.6 M/E". AI agents read the Proposals and choose. A Proposal missing a signature lapses ([INV-10](02_type_system.md#inv-10)). The round still resolves, and the lapse is recorded as an ordinary outcome. Counteroffers are new Proposals, signable next round.

Illustrative seat Policies (not all are presets):

- **US:** keep at least 50 E, buying with a 3.0 M/E limit. Procure Military after a major Chinese deployment last round. Spend up to 15 M per year on Public Attention while Energy is below 3. Keep at least 40 M in reserve.
- **Germany:** keep at least 25 E. Sign the EU's `eu-solidarity` procedure Proposals.
- **SA and JP:** SA proposes 20 E for 48 M to JP. JP signs what is addressed to it while the Energy price is at most 2.4 M/E ([Fixture G](05_test_instance.md#27-fixture-g-proposals-and-signatures)).
- **SME:** cut Energy purchases above 3 M/E. Raise them below 2.2 M/E. Keep operating cash.

### 4.2 Presets

Preset strategies are named parameter sets of the shared interpreter, not separate strategies. The presets are the executable definition: this table states what each one does (`packages/premises/src/Premises/Presets.purs`, compiled to `packages/premises/presets.json`). "Cheapest" and "costliest" are the Energy Markets the Entity has access to with the lowest and highest beginning-of-round price (Type System [§4.4](02_type_system.md#44-shared-decision-language-interpreter)); a seat with access to one Energy Market trades there, and a seat with none trades nowhere. Every quantity is in Resource units per round.

| Preset | Behavior |
|---|---|
| `Passive` | Submits nothing. |
| `Conservative` | Buys 5 Energy at the cheapest Market at a limit of 2.5 while its Energy holding is below 30 and that price is at most 2.5. |
| `Aggressive` | Runs `defense-procurement` at 4 units every round. It has no restrictions. |
| `Accumulator` | Buys 10 Energy at the cheapest Market at a limit of 3 while that price is at most 3. It has no holding target. |
| `Trader` | Buys 5 Energy at the cheapest Market at a limit of 2.2 while that price is at most 2.2. Sells 5 at the costliest Market at a limit of 3 while that price is above 3. The bands are fixed in the preset. |
| `Cooperative` | Proposes nothing. Signs every Proposal addressed to it. |
| `Opportunistic` | Sells 5 Energy at the costliest Market at a limit of 3 while that price is above 3. Signs every Proposal addressed to it. |
| `Investor` | Runs `energy-capacity-investment` for 1 unit while its Money holding is above 100, as a deal that CHIP co-signs. |
| `CrisisResponse` | Buys 3 Energy at the cheapest Market at a limit of 4 while that price is above 3.5 and its Energy holding is below 20. |
| `RandomValid` | Each round, one seeded draw picks one option: no action, buy 5 Energy at the cheapest Market at a limit of 3, or sell 5 at the costliest at a limit of 2. |
| `Deployer` | While the Atlantic price is above 3, reserves 60 Mil for `military-deployment` and sells 40 Mil on the `defence-goods` Market at a limit of 2.4 M/Mil in the same round. The reservation and the sale compete for the seat's Mil, so both are scaled ([05 §13.3](05_test_instance.md#133-military-deployment)). |

The Dashboard's launch form offers the same names: `GET /api/premise` sends them (`Premises.Presets.presetNames`, in the order written).

### 4.3 Coverage

Every seat has exactly one bound seat Policy. A binding for an NPC, an Aggregate, or an unknown id is an error (`GameError.NotASeat`), and so is a configured seat with no binding (`GameError.UnboundSeat`). A seat meant to stay silent is bound explicitly to an empty `Script`. A literal premise has no seat configuration, so any Entity it writes answers for may be bound. A seat Policy that produces nothing submits nothing for the round, and nothing is recorded for it. The simulator never invents default decisions.

## 5. Deterministic Randomness

Mechanics are deterministic by default. Randomness enters only as a keyed `draw` on a seed, where a Gameboard, an estimator, or a preset strategy declares a stochastic choice ([INV-13](02_type_system.md#inv-13), [INV-36](02_type_system.md#inv-36)).

**The seed rule.** A run has one player seed, and it is resolved once, before initialization, at the program boundary. An explicitly injected seed is used as given. An absent seed is drawn from the operating system exactly once, and the concrete value is the one the run uses. The CLI (`--seed`, else drawn and printed), the server (`POST /api/runs`: `playerSeed` null or absent means drawn, and the reply names the seed used), and the dashboard (a blank player seed means drawn) all resolve it the same way, through `Adapters.Random.resolveSeed`. The stored spec records the resolved seed, never an absent value. The simulator functions take a concrete seed and read no entropy, so **the same seed gives the same run**, and a recorded seed replays it. Drawing a seed `s` and injecting `s` produce identical records and an identical stored spec.

`simulate` takes a concrete seed and reads no entropy. Its signature is in [07 §4](07_architecture.md#4-key-interfaces).

Draws come from two streams (Type System [§9.1](02_type_system.md#91-numerics-and-randomness)):

| Stream | Seed | Used for |
|---|---|---|
| `Game` | Game seed (`InstantiationConfig`) | Every in-world draw: estimator sampling noise, declared Policy randomness, declared shocks |
| `Player` | Player seed (`SimulationSpec`) | Every seat-Policy draw: `RandomValid` and any stochastic preset. AI agents receive it where their provider supports seeding, but are still reproduced from recorded input |

The streams never share draws. Varying the player seed never changes an in-world draw, and varying the game seed never changes seat-Policy behavior. Live play has no player seed.

- **Player-seed distributions:** fix the game seed and vary the player seed to see the spread of outcomes caused by player behavior alone.
- **Game-seed variation:** fix the player seed and vary the game seed to see in-world stochastic variation.
- **Counterfactuals** fix both seeds, so differences come only from the intervention.

Conversion of random values into quantities uses the Type System's fixed-point rule ([§9.1](02_type_system.md#91-numerics-and-randomness)).

---

# Part II: Execution and Assertions

## 6. Lifecycle and Execution

In the Run Store a run's status is `running`, `completed`, or `failed`, and a finished run's status is set by the `finish` program (`Effects.Store`). The dashboard's run list adds `unreadable` for a run it cannot decode. A run is `running` from its creation until it finishes. A failed run keeps its last valid state and diagnostics.

Each round follows the canonical sequence (Type System [§8.1](02_type_system.md#81-canonical-resolution-sequence)). The simulator drives the same round runner and engine as live play, with seat Policies bound:

| Canonical phase | Simulator role |
|---|---|
| 2. Create observations | Passes each seat its authorized view. |
| 3a. Propose / 3b. Sign | Drives the round runner, which runs the bound seat Policies alongside the NPC and Aggregate Policies on the shared interpreter. |
| 7. Commit and record | Persists the round record, snapshot, assertion results, and metrics. |

The engine performs phases 1–2 and 4–7. The simulator never evaluates NPC or Aggregate Policies itself, computes Representation estimates or procedure support, or repeats Aggregate actions. It delegates to the round runner, which is the same code used in live play.

The simulator exposes **run to completion** (`simulate`, or `simulateWith` with a per-round sink) and **extend** (`resumeFrom`, which continues from a stored snapshot). **Planned:** a single-round step operation.

### 6.1 Invalid actions and failures

An invalid action is an ordinary outcome. It is recorded with its reason and handled by the engine's deterministic reject or partial-fill rules (Type System [§3.2](02_type_system.md#32-infeasible-actions)). The round record keeps both the proposed and the executed action. A representative's observation comes from the proposal, even when its fill differs.

An invariant violation is reported, not blocked: the round's assertions are stored beside it, the run's status stays `completed`, and the dashboard surfaces them. The simulator preserves the configuration, last valid state, failing round, Intents, Policy, Representation, and procedure outputs, the violated invariant, and the relevant DrawKeys. A failure therefore becomes a reproducible regression test.

A failed run is never left `running`. It is marked `failed` with its reason in each of these cases: an engine error or invariant violation (the reason is the error). An exception while simulating or saving a round, such as a store fault (the reason is the exception). A live session that stops early, such as a language-model backend that cannot start, a non-zero exit, or input that ends at a prompt (the reason names the cause). A backend failure in live play is fatal to the run: the round in progress is not completed, and the run keeps the rounds already saved. Decoding errors from a model reply are not backend failures. A joint-pass reply that does not decode is corrected: the model gets its previous reply and the decode error, at most two times. If the last reply still does not decode, it becomes an empty submission (the speakers are told) and the round proceeds. A narrator or seat reply that does not decode becomes a plain event listing or an empty transcript. A run records the process that created it (`owner`: its role, process id, and start time). At server startup, runs still marked `running` that a server left (by a process that stopped) are marked failed with the reason `interrupted`, and so are older rows with no owner. A run owned by another role, such as a live CLI session on the same store, is left alone.

Live play has no confirmation step. Every transcript of the round, human and AI, is interpreted together into Intents. A description of a deal in a transcript counts as that party's signature, and the interpreter adjusts matching descriptions slightly so that they become one Proposal. The transcripts and the Intents are recorded with the round, so replay uses the recorded Intents and does not re-interpret.

### 6.2 Entry points and flags

| Command | Behaviour |
|---|---|
| `run <premise> [--preset P] [--rounds n] [--seed n]` (`just sim`) | Plays every seat with preset `P` (default `Passive`) for `n` rounds (default 5, at least 1). The player seed is `--seed`, else drawn and printed. Prints a per-round summary and does not save to the Run Store. |
| `session <premise> [--humans S,…] [--model id] [--rounds n] [--window s] [--id name] [--db file] [--seed n]` (`just session`) | Live facilitator console. Seats named in `--humans` type one transcript per round, then give no further input. The rest are AI-agent seats. Defaults: model `openrouter/deepseek/deepseek-v4-flash-latest`, window 0, id `session-1`, database `runs.db`. Saves the session as a run in `--db`; the raw model output and the call log go to `runs/<id>/`. |
| `server [--db file] [--port n] [--static dir]` (`just serve`) | The read API and the dashboard. Defaults: `runs.db`, port 8787 (1–65535), static `apps/dashboard/dist`. |

An unknown flag, a flag without a value, or a bad value is a usage error and exits 1. Nothing is ignored. The premise argument is the premise's directory name (`meridian`), and its version is `v0`. The dashboard's dev proxy and its end-to-end server use separate ports, API 8787 and E2E 8799 (`apps/dashboard/ports.mjs`).

## 7. Run Assertions

Assertions check engine properties after every round without reimplementing the engine. They are the `checks` package's pure predicates over the records: `checkRun` applies `Core.Invariants.check` to each round, `checkRound` compares an `Expect` with that round's `observe`d record, and `evaluateObjectives` scores each Actor's objective ([§9.2](#92-run-details)). Initialization is not asserted separately.

### 7.1 Required assertions

Each required assertion checks exactly one invariant from the Type System master table. The status column says what the code checks per round today.

| Assertion | Invariant | Status |
|---|---|---|
| Unified action semantics | [INV-01](02_type_system.md#inv-01) | Implemented (settlement events against Intents and declared flows) |
| Authority separation | [INV-02](02_type_system.md#inv-02) | Implemented |
| No authority amplification | [INV-03](02_type_system.md#inv-03) | Implemented |
| Resource consistency | [INV-04](02_type_system.md#inv-04) | Implemented |
| Stock-flow reconciliation | [INV-05](02_type_system.md#inv-05) | Implemented |
| Market solvency | [INV-06](02_type_system.md#inv-06) | Implemented |
| Price is not production | [INV-07](02_type_system.md#inv-07) | Planned |
| Account partition | [INV-08](02_type_system.md#inv-08) | Implemented per round for facility capacities |
| Representation consistency | [INV-09](02_type_system.md#inv-09) | Implemented for the estimator bound |
| Signature completeness | [INV-10](02_type_system.md#inv-10) | Implemented |
| Order independence | [INV-11](02_type_system.md#inv-11) | Planned per round. Tested by §14 test 3 |
| Policy reproducibility | [INV-12](02_type_system.md#inv-12) | Planned |
| Deterministic replay | [INV-13](02_type_system.md#inv-13) | Tested by §14 test 2 |
| Resolution consistency | [INV-14](02_type_system.md#inv-14) | Tested by §14 test 12 |
| Single authoritative state | [INV-15](02_type_system.md#inv-15) | Planned |
| Information isolation | [INV-16](02_type_system.md#inv-16) | Planned |

Simulator invariants [INV-29](02_type_system.md#inv-29) to [INV-31](02_type_system.md#inv-31) are checked by the simulator's own test suite rather than by per-round assertions.

Conservation checks read each Resource's declared properties and the Transformations that produce or consume it ([INV-04](02_type_system.md#inv-04), [INV-05](02_type_system.md#inv-05)). They do not hard-code the reference Resources. Soft Power is produced and decays. Energy is produced and consumed. Military is reserved by deployment, lost through declared attrition, and restored by procurement. Money is conserved unless authorized issuance occurs. A blanket "totals are constant" assertion would be wrong.

A failed assertion records its invariant ID and message with its round. The run list and overview report the number of failing assertions. **Planned:** per-assertion expected and observed quantities, the relevant events, and a dashboard view of each round's results.

### 7.2 Metamorphic tests

These tests transform a configuration in a controlled way. Those marked with a test number in §14 are implemented. The rest are planned.

- Reorder submissions ([INV-11](02_type_system.md#inv-11)). §14 test 3.
- Replay with the same game and player seeds ([INV-13](02_type_system.md#inv-13)). §14 tests 2 and 13.
- Remove all representative observations. Aggregates must follow their baselines ([INV-09](02_type_system.md#inv-09)). Planned.
- Withhold one signature. The Proposal lapses and the round still resolves ([INV-10](02_type_system.md#inv-10)). Covered by §14 test 10.
- Change one seat Policy or one Market parameter. Covered by the `variant` and `counterfactual` experiments.
- Change explicitness ([§7.3](#73-resolution-consistency-test)). §14 test 12.

### 7.3 Resolution-consistency test

This test is the direct check of [INV-14](02_type_system.md#inv-14).

Instantiate the reference board twice:

- **A:** German government activity stays implicit in the EU.
- **B:** The German Chancellor is explicit. Only government accounts are extracted. German households and firms stay in the EU residual.

Before any decisions differ, the test checks that stocks reconcile, that every liability and capability has exactly one holder, and that behavioral responsibilities are covered exactly once.

The test compares A and B at initialization, where any difference is an accounting defect. It does not then play B with a Germany seat that reproduces the implicit baseline exactly: in A the implicit EU spends Germany's flows internally, so no seat Policy can match it action for action.

### 7.4 AI interpretation tests

Text-to-Intent interpretation is tested separately: the handlers suite (`Test.Handlers.Llm`) through a stub backend (`Test.Handlers.Stub`) or the replayed recording, covering prompts, reply decoding, and evidence; and the adapter suite (`Test.Adapters.Pi`) for the `pi` process boundary, with a fake `pi` and no model. The simulator consumes canonical Intents and never calls the language model.

The interpretation suite contains:

- text inputs paired with their expected canonical Intents.
- cross-player cases, where a deal described by both parties reconciles into one Proposal with both as signers, and a deal described by one party alone lapses.
- inadmissible cases, such as claimed resources or authority the Actor lacks, where the engine rejects the Intent as a `Rejected` event with its reason ([04 §11.2](04_instantiated_game.md#112-live-session)).

Accepted canonical Intents are the replay boundary.

---

# Part III: Run Store

## 8. Persistence

Every run is persisted and survives restarts. Persistence goes through three rows of the effect vocabulary ([02 §8.4](02_type_system.md#84-effects-and-handlers)): `RECORD` writes each round, `STORE` writes the run lifecycle, and `OBSERVE` reads. The server reaches them only through the `API` row (§8.1).

| Field | Contents |
|---|---|
| **Run ID, name, status** | Identity and lifecycle state. Also `created`, `reason` (for a failed run), and the derivation link (`source`, `kind`). |
| **Manifest** | Immutable Gameboard id, engine version, draw version, and initial World State. Seat bindings, the player seed, the round count, and overrides are in the run's spec ([04 §9](04_instantiated_game.md)). |
| **Round records** | Append-only Proposals, signatures, lapsed Proposals, proposed and executed actions, procedure Proposals with their support and outcome, stored Representation estimates, and events. |
| **Snapshots** | The World State each round ends with. The initial World State is in the manifest. |
| **Assertions** | Failures per round, with their invariant IDs. |
| **Metrics** | Derived projections (`price:<market>` after each round), tagged with the version of the metric definitions (`v1`). |

**Source of truth ([INV-31](02_type_system.md#inv-31)).** The manifest, round records, and snapshots are authoritative. Metrics are reproducible projections of them. When a metric definition changes, its values are recomputed and the records stay untouched. Snapshots support fast inspection. Records explain outcomes. Neither replaces the other. Every outcome traces to its inputs, Policies, and configuration.

**Mutability.** Completed runs are immutable, and a changed configuration always gets a new Run ID. Round records, snapshots, and metrics are never edited. A run's status, failure reason, archive flag, and spec change only through the operations in §8.1. Resume is the only one that adds rounds, and it keeps every existing round record. Delete removes a run with its rounds and metrics.

Live sessions also keep evidence of each language-model call: the effect, model ID, template and input hashes, a pointer to the raw output, and the decoded result. This evidence lives beside the run, outside the round records. Replay never re-invokes the model. The canonical Proposals and signatures in the round records are enough.

The initial record is written before Round 1. After each round, the round record, snapshot, assertions, and metrics are saved in one transaction. If Round 4 fails, Rounds 1–3 remain available.

**Schema.** The store is SQLite, accessed through the primitives of `Adapters.SQLite` and interpreted by `Handlers.Store`. Its schema version is `PRAGMA user_version`, and the migrations are ordered and append-only:

1. Tables `runs` (`id`, `name`, `status`, `spec`, `manifest`, `created`), `rounds` (`run`, `round`, `record`, `snapshot`, `assertions`. Key `run`, `round`), and `metrics` (`run`, `metric`, `version`, `round`, `value`. Key `run`, `metric`, `version`, `round`).
2. `runs.reason`.
3. `runs.archived` (default 0), `runs.source`, and `runs.kind`.
4. `rounds.transcripts`: the round's seat transcripts as a JSON array, stored in the round's transaction (default `[]`).

**Backend.** The `StoreBackend` carries typed values only: a run's `Spec` (its string fields), its `Manifest`, and each round's record, snapshot, assertions, and transcripts. `Adapters.SQLite` does the JSON, encoding on write and decoding on read, and the SQL. A stored text that does not decode is a `Left` in its field, naming the field, so one unreadable value does not hide the other runs.

**Access.** Opening read-write creates the store or applies the missing migrations, each in its own transaction. A store whose version is newer than the build is refused. A read-only open never creates a file and never migrates: a missing store, or one at another schema version, is an error. Writers wait up to five seconds for a lock held by another connection before reporting it busy. Metric rows carry the version of the definition they were computed under, so a projection computed under one definition is never mixed with another. The Dashboard reads whole rounds, not metric series.

**Replay.** Replaying a run from its records requires a record for every round 1 to n. A missing round is an error naming the rounds, never a silent replay with no seat inputs. A record may have empty inputs. The round must exist.

**Identity.** A stored seed is insufficient without the versioned PRNG, which is why the manifest records it. The player seed is recorded in the run's stored spec as the resolved value, whether it was injected or drawn. Run IDs never influence sampling. A new run's ID (`run-N`) is never one that a stored run still cites as its source.

### 8.1 Read API and run operations

The server (`apps/server`) serves JSON through the `API` row. `Handlers.Api.Route` is the one route table, `Handlers.Api.Client` answers the same operations for a browser, and the server's `Server.Implementation` supplies what each operation does. Every other method or path is a 404. Replies are 200 unless stated. A finished run is decoded once and kept in the server's cache (`Server.Cache`); a running run is not kept, and every write operation clears the cache. Text replies and the dashboard's files are gzipped for a client that accepts it; the hashed bundle files are immutable, and `index.html` is revalidated on each load.

| Method and path | Purpose | Replies |
|---|---|---|
| `GET /api/premise` | The launch form's premise: name, version, round range (`min`, `max`), default rounds, game seed, `defaultName` (the next run id, the default name of a launch), and `presets` (the seat presets, in the order they are written). | 200 |
| `GET /api/runs[?archived=true]` | The run list: a `Table` plus `runs`, the typed flags of each run in the table's order (`id`, `archived`, `readable`; a run that cannot be decoded is not readable). Archived runs are left out unless `archived=true`. | 200 |
| `POST /api/runs` | Launch a run of one preset on every seat. Body: `name` (string), `preset`, `playerSeed` (int, null or absent to draw), `rounds` (int, null or absent for the default). | 200 `{id, playerSeed}`. 400 for a bad body or a refused launch |
| `GET /api/runs/:id/:view[?round=n]` | One view. The `overview` reply is a `Table` plus `rounds` and `target` (completed and target rounds), `facts` (the header's label and value pairs: Name, Premise, Status, Rounds completed, Game ID). `view` is `overview`, `objectives`, `balances`, `markets`, `facilities`, `economy`, `representation`, `procedures`, `events`, `standings`, `networth`, `actions`, `drivers`, or `balance`. `round` applies to `balances` (default: the final round) and `events` (default 1). | 200. 400 for a bad or out-of-range round. 404 for an unknown run or view. 500 for an unreadable run |
| `GET /api/runs/:id/entities` | The run's Entities: `entities`, each `{id, seat}` (`seat` is true for an Actor, false for an Aggregate), and `default`, the first seat (or the first Entity). | 200. 404. 500 |
| `GET /api/runs/:id/entity/:entity` | One Entity's balance sheet after every round (`Views.Entity`). Reply: `table` (a row per Resource held or quoted, plus `Net worth (M)`, a column per round `Round 0`…), `chart` (one line per held Resource, in its own unit), `netWorth`. | 200. 400 for an Entity the run does not have, listing the valid names. 404. 500 |
| `POST /api/runs/:id/derive` | Derive a run. Body: `kind` (`duplicate`, `rerun`, `variant`, `counterfactual`), optional `name`, `preset`, `playerSeed` (null draws), `rounds`, `overrides`. | 201 `{id, source, kind}`. 400 for a bad kind, field, preset, or override. 404 for an unknown source. 409 for a rerun whose source cannot be compared |
| `POST /api/runs/:id/resume` | Extend a completed run to `rounds` total rounds. Body `{rounds}`. | 200 `{id, rounds}`. 400 if the run is not completed or `rounds` is not above its completed count. 404. 500 |
| `POST /api/runs/:id/archive`, `…/unarchive` | Set or clear the archive flag. Idempotent. | 200 `{id, archived}`. 404 |
| `DELETE /api/runs/:id?confirm=:id` | Delete the run with its rounds and metrics. `confirm` must name the run. | 200 `{deleted}`. 400 without a matching `confirm`. 404 |
| `GET /api/compare?runs=a,b[&market=m]` | Compare a scenario set. The first valid run is the baseline. Reply fields: `compatibility`, `trajectory`, `markets`, `netWorth`, `standings`, `balance`, `keyValues`, `final`, `diff`, `config`, `seeds`, `skipped`, `baseline` (the baseline run's column). `final` and `config` are tables with a `marked` flag per cell, set where the cell differs from the baseline. | 200. 400 when no run is valid or `market` is unknown |
| `GET /api/glossary` | The glossary the dashboard shows: `definitions` and `reasons` by column or field name, `unnamed` (the text for a name with no entry, with `{name}`), and `price` (the prefix and text of the overview's `Final price` fields, with `{market}`). Served whole, so the dashboard holds no glossary text. | 200 |
| `GET /api/agency[?playerSeed=n&preset=p&alternative=a]` | Agency analysis: every seat plays `preset` (default `Conservative`), then each Actor in turn plays `alternative` (default `Aggressive`). `playerSeed` defaults to 1. | 200. 400 for a bad seed or an unknown preset |

Operation rules:

- **Derive.** `duplicate` and `rerun` take no spec field. Any of `preset`, `playerSeed`, `rounds`, `overrides` is refused. `variant` replaces `preset`, `playerSeed`, or `rounds` and takes no overrides. `counterfactual` requires at least one override, which it adds to the source's, and takes no `preset`, `playerSeed`, or `rounds`. The default name is `<source name> (<kind>)`. Every derived run stores `source` and `kind` in its spec and its own columns.
- **Rerun.** The zero-change case (§10.4): its reply's `recordsMatch` is `true` when its round records equal the source's under `Simulator.Records.recordsEqual`. The source must be completed with stored records, or the rerun is refused with 409 and nothing is created. `recordsMatch: false` is a finding about reproducibility, not a reply error. `recordsMatch` is present only for a rerun.
- **Resume.** Starts from the snapshot after the run's last completed round and updates the stored spec's round count. The run keeps its ID.
- **Deleted sources.** A derived run whose source was deleted keeps its `source`, which then shows as deleted.
- **Premise route.** `GET /api/premise` was named `GET /api/scenario` before the premise naming. No alias is kept.

Dashboard controls exist for launch, resume, archive, and delete. **Planned:** dashboard controls for duplicate, rerun, variant, and counterfactual. Until then these are API-only.

The **Store interface** (`STORE` for writes and `OBSERVE` for reads, in `Effects.Signatures`, see [07 §4](07_architecture.md#4-key-interfaces)) supports save, retrieve, list, full round histories, resume, and explicit archive or delete. **Planned:** labels. The Dashboard reads through the read API and never touches engine internals.

---

# Part IV: Dashboard

## 9. Pages

The Dashboard has three pages, each on its own hash route, and a top bar that links to them from every page: **Runs** (`#/runs`, the home page, and `#/runs/:id` with that run selected), **Prepare** (`#/prepare`), and **Compare** (`#/compare`, and `#/compare/:ids` with the ticked runs). It renders from persisted data without running the engine.

**Display kinds.** Every number a page, the CLI, or a Compare view shows has one display kind ([02 §9.1](02_type_system.md#91-numerics-and-randomness)). The stored and computed values stay exact at four decimals. A **quantity** (Money and Resource counts) shows whole numbers without decimals and otherwise at most two: holdings, balance sheets, net worth, standings, flows, pressure, procured units, shortfalls, and the balance spread. A **price** shows two decimals with trailing zeros trimmed: the marginal price and its change, in the Marginal prices chart and the Compare agency table. A **ratio** shows two decimals: the Gini coefficient, the Representation shares, procedure support and threshold, and tariff rates. Chart axes and tooltips use the chart's kind, and the plotted lines keep full precision. The Prepare form's inputs and the JSON API's text are unchanged.

**Layout.** One 12-column grid with named spacing tokens (`apps/dashboard/app.css`) and the components in `Dashboard.Layout`. Every visible table column and overview field has a plain-language definition on hover: the server's glossary (`GET /api/glossary`), loaded before the first page renders and looked up by `Dashboard.Glossary`. Every request the dashboard makes goes through `Dashboard.Client`, one typed function per API operation, run over `Handlers.Api.Client`, and the views are fetched once per selection: the run page fetches its overview once and passes it to its results and its manage panel.

### 9.1 Simulation Runs

The run list is a sortable table, one row per stored run in creation order. Its columns are `Run`, `Name`, `Gameboard`, `Game seed`, `Player seed`, `Rounds` (completed), `Status`, `Assertions failing`, `Reason` (for a failed run), `Archived`, and `Source` (a link to the run it was derived from). A run that cannot be decoded shows status `unreadable` and its reason, and the other runs stay visible.

- A **Show archived runs** toggle adds the archived runs. Without it they are hidden.
- Selecting a row (or opening `#/runs/:id`) shows that run's results beside the list. With no selection, the latest readable run is shown. Selecting a run gives it its own address.
- **Resume** appears on the run page only for a completed run, which it extends to the round count it is given. Resume never continues a failed or interrupted run; the overview's `resumable` field says which runs qualify.
- **Archive** / **Unarchive** and **Delete** (one click, the request's `confirm` names the run) are on the run page under "Manage this run".
- **Run** on the Prepare page launches a run (§9.3).

The server performs and validates every operation. The dashboard sends the request, shows the server's message when it refuses one, and then opens the new run or refreshes this one. The Runs page has no comparison control.

### 9.2 Run Details

The page opens with a compact header: run id, then the overview's facts (name, Premise, status, rounds completed out of target, and game id). Resume is offered from the overview's `resumable`. Then the sections below appear in this order, each full width with one purpose line and no disclosure control. Each section after the first draws on one read API view (§8.1).

1. **Key figures.** The five statistics the sections answer with, each with its question, definition, and formula. Each is a `Statistic` record in the module that computes it, and the dashboard renders that record. No view computes an economic statistic in the browser.
2. **Marginal prices.** The marginal price of each Market after every round (`markets`, first chart).
3. **Standings.** Each player's net worth after the final round, its change from the start, and its rank (`standings`).
4. **Net worth over time.** One line per player, from round 0 to the final round (`networth`).
5. **Actions.** Per player: trades, transforms, transfers paid, deals signed, and proposals rejected, as a bar chart and totals table (`actions`). Round-by-round counts are API-only.
6. **Drivers.** A table by round of the rules enacted and repealed, the collective procedure outcomes (executed or lapsed, with support and threshold), and the shortfalls per Resource (`drivers`).
7. **Balance measures.** The spread and the Gini coefficient of final net worth (`balance`).
8. **Objectives.** Each Actor's objective criteria as met, not met, or not evaluated, with the measured values (`objectives`). A criterion is not evaluated when the run has no rounds or its series is empty. It is never reported as met.
9. **Final balance sheet.** Every Entity's holdings (Actors, NPCs, and Aggregates, with a `Kind` column) by Resource at the final round: `Initial`, `Current`, and `Change` (`balances`). There is no Reserved column, because a reservation does not outlive its round. Aggregates are residual representations.
10. **Entity balance sheet.** A selector of the run's Entities, defaulting to the first seat. `GET /api/runs/:id/entities` says which Entities are seats and which is the default. It draws **Holdings by Resource**, one line per held Resource in its own unit, and **Net worth** in M on its own scale, from `/api/runs/:id/entity/:entity` (§8.1).

**Definitions.** *Net worth*, *spread*, and the *Gini coefficient* are defined in [02 §2.5](02_type_system.md#25-glossary). The mean price of a Resource's Markets is a view convention, not an engine rule, and Soft Power has no Market. Aggregates are residual and are not players. Standings, the net-worth chart, and the Entity balance sheet all use this one definition (`Views.Standings`). Standings rank 1 is the highest. Equal net worths share a rank. The spread and the Gini coefficient are computed in `Views.Balance`.

**Not on the run page** (served by the API for analysis and tooling): the round-by-round action counts. The Market, facility, economy, and pressure views (`markets`, `facilities`, `economy`). The representation and procedure records (`representation`, `procedures`). The raw event log (`events`). And the overview fields (engine, draws, seeds, rejected Proposals, assertion counts, and `Rounds target`). The Dashboard does not show macroeconomic indicators the Gameboard does not define.

**Representation (API-only).** `representation` covers every active Representation as a four-stage record that spans two rounds: the observation (round r, from the validated, escrowed Proposal). The estimate (round r: the stored adjustment factor, its parameters, the noise draw, and whether the bound applied). The proposed Aggregate Intent (round r+1, applying the stored factor). And the executed result (round r+1, beside the representative's own actions). The **influence ratio** is the induced change in Aggregate activity divided by the resources the representative directly controls. It is `n/a` in the last round and where the representative bought nothing. It is a balancing parameter, not a claim that the representative is an unbiased sample.

**Procedures (API-only).** `procedures` lists each procedure Proposal per round, the members who signed, the support against the threshold, and the outcome (executed or lapsed). An optional resource contribution is a separate Transfer.

**Event order (API-only).** `events?round=n` lists a round's events in canonical phase order: round preparation. Proposals, signatures, and lapses. Seat Intents and signatures. NPC and Aggregate outputs. Procedure Proposals with support and outcome. Stored Representation estimates. Validation results with partial fills. The economic cycle and settlement. Resource changes. And triggered assertions.

**Planned** on the run page: the lineage section ("Derived from" and "Derived runs", the list's Source column already links to the source). The representation and procedure views. The event inspector. And the flows, signed deals, and non-Resource state (capacities, market access) in the balance sheets.

### 9.3 Prepare

Prepare (`#/prepare`) has one panel, **Launch a run**, which launches a run of the premise. Every field has a default, so Run needs no edits:

| Field | Required | Default | Notes |
|---|---|---|---|
| Name | optional | `defaultName`, the next run id (`run-N`) | A label. The server names the default, and the dashboard fills a blank name with it. The API requires a `name`. |
| Preset | required | Conservative | The seat Policy every seat plays. The choices are the `presets` `GET /api/premise` sends. |
| Premise | required | `meridian@v0` | The one premise the server serves. |
| Rounds | optional | the premise's default (5) | A select of the premise's supported counts (4 to 8). Resume takes a whole number above the run's completed rounds, with no upper bound (§8.1). |
| Player seed | optional | blank | Blank draws a seed once at launch and stores it with the run. A whole number fixes it. |
| Game seed | read-only | the premise's seed (26021) | The server takes no game seed at launch, so the value is shown and cannot be edited. |

A summary line says what Run will play, for example "This run: Conservative, 5 rounds, seed drawn at launch." A launch the server accepts opens the new run. A refused one shows the server's message. The round range, default, game seed, default name, and presets come from `GET /api/premise`.

### 9.4 Compare

The Compare page (`#/compare`) lists every readable run as a tick box, and the ticked runs, in the order ticked, form the scenario set. The first run ticked is the baseline, and the top bar shows how many are ticked. A run leaves the comparison only by being unticked. The page has no other removal control. The sections, each with one purpose line:

- **Prices by market:** one plot per Market, one line per run, in the Market's quote per base unit.
- **Net worth over time:** a player selector. One line per run for that player, from round 0 to the final round. A run that lacks the player has no point.
- **Final standings:** one group per player, one bar per run, in M.
- **Spread and Gini:** the spread (M) and Gini of final net worth, one bar per run.
- **Key values:** one row per measure or player, one column per run, headed by the run's name and id. Rows are each player's final net worth, change, and rank. Each player's trades, transforms, and transfers. The spread and Gini. Each role's objective criteria met. Proposals rejected by all players. And each Market's final price.
- **Details:** tabs for final state (differing values highlighted), baseline diff, configuration (differing values highlighted), compatibility, seed variation, and agency (§10.5).

Every statistic is computed in the views package: `Views.Compare` and `Views.Markets` (`/api/compare`). A run's colour is its place among the ticked runs, from the theme's chart variables (`--chart-1` to `--chart-5`), so it reads in both themes. Colours repeat beyond five runs. Each legend names every run and marks the baseline. A full-width plot is 360px tall. The page is stale-while-revalidate: ticking a run refetches the comparison, and the last loaded comparison stays on screen until the new one arrives, with each plot in a frame of fixed size. Drivers and procedures are per run and are not compared.

## 10. Comparison

Comparison is a core feature. It supports both correctness testing and balancing.

### 10.1 Selection and compatibility

The Compare page takes any number of ticked runs, with the first as the baseline. The Dashboard flags runs that are not directly comparable: different Gameboard versions, engines, round counts, units, Market definitions, or Actor selections (the preset every seat plays). The compatibility table names each difference, for example `no: flagged, not combined (engine, Actor selection)`. Units and Market definitions are compared on the manifest's initial World State. Incompatible configurations and metrics are flagged, never combined. Trajectories are aligned by simulated time, with no interpolation that has no basis in the data. A net-worth chart has no point for a round where a run lacks that player. A comparison with no valid run is a client error (400). A run that cannot be read is listed as skipped with its reason.

**Planned:** a limit of two to eight runs, statistical summaries for larger batches, and metric filters by Actor, Aggregate, or Market.

### 10.2 Views

- **Trajectory:** one metric across runs, for example each Market's price (`markets`) or each player's net worth.
- **Final state:** ending holdings side by side, with differing values highlighted.
- **Difference from baseline:** the holdings that differ from the baseline, as absolute changes. **Planned:** percentage differences.
- **Configuration diff:** the parameter differences between runs, one row each, categorised as version (`gameId`, `gameboard`, `engine`), seat Policy (`preset`), seed (`game seed`, `player seed`), or other (a deliberate change, such as overrides), so deliberate changes are distinguished from the rest.

### 10.3 Actor and whole-economy views

**Planned:** comparing any Actor's or Aggregate's holdings trajectory across runs, and whole-economy metrics such as demand, concentration, unfilled demand, and the Money distribution. The `economy` view today gives resource totals and Money circulation for one run. These measures are meant to separate a seat Policy that helps one Actor from a configuration that destabilizes the whole economy.

### 10.4 Counterfactual experiments

A [rerun](02_type_system.md#25-glossary) duplicates a run's resolved configuration and both seeds with nothing changed, so its round records must equal the source's (§8.1). `recordsMatch: true` confirms that the source reproduces. `recordsMatch: false` means nondeterminism, a stored-versus-live mismatch, or an unrecorded seed, and is a finding.

A **counterfactual** (`kind` `counterfactual`) is the intervention case:

1. Duplicate a run's resolved configuration and both seeds.
2. Change exactly one seat Policy or one parameter (through `overrides`, or a `variant`'s preset).
3. Replay the run as an ordinary immutable run, linked to its source.
4. Compare it against the original as the baseline (Compare page).

Both seeds and all DrawKeys stay fixed, so the difference measures the intervention's total modeled effect, including downstream consequences. Typical questions: how much did Germany's signature change EU behavior. What happens without the US deployment. Does more Energy liquidity help smaller Actors.

### 10.5 Agency analysis

For each Actor, the simulator replays the game with the alternative preset for that Actor, holding everything else fixed, and measures changes in prices, other Actors' holdings, Aggregate behavior, institutional decisions, and the Actor's own objective criteria ([§9.3](05_test_instance.md#93-objectives)). The unknown-preset check is in §8.1: an unknown name is an error, never a silent baseline.

The Compare page's **Agency (fixed baseline)** tab shows one row per Actor under the alternative, with its price change, others' and Aggregates' holdings, institutional decisions, and own criteria changed. A row whose alternative produces negligible differences on every measure is marked `no: balancing finding`. **Planned:** the criteria results under each alternative, shown as an agency profile per role. Agency matters more than equal starting resources.

### 10.6 Saved comparisons

The comparison's runs and baseline are carried in the route (`#/compare/:ids`), and the theme preference is kept in browser storage. **Planned:** persisting selections, metrics, and display preferences as Dashboard state, and named scenario sets (for example "Energy Scarcity Experiment"), which would reference immutable Run IDs and never copy run data.

---

# Part V: Experimentation

## 11. Experiments

**Planned as a launcher.** Experiments run today in the `experiments` package (`Experiments.Agency`: `playerSeedRuns`, `counterfactual`, and `agency`). It assembles game variants, runs the pure `simulate`, scores them with `checks`, and presents them through `views`. Its suite is `Test.Experiments.Experiments`. No CLI or API generates a batch. The target is a **Variant Experiment** that generates one specification for each declared parameter value, runs every resulting game, saves the runs, and opens them as a scenario set.

Target experiment kinds:

- **Single-parameter:** Energy curve slope (0.01, 0.02, 0.04, 0.08). SME estimator shrinkage (0, 0.10, 0.25, 0.50). Germany's procedure weight, with the residual weight complementary. Procurement capacity and attrition rate. Production capacities, revenue rates, and wages, asking whether the economy sustains itself over five years or depletes or grows without bound.
- **Multi-parameter:** controlled combinations run as batches, for example low supply with aggressive procurement.
- **Seat-Policy:** hold parameters fixed and vary presets, for example all Conservative. Governments Aggressive with corporations as Investors. One deviator among Cooperative players.
- **Player-seed and game-seed batches:** as in §5.

The target is also for the Dashboard to separate variation caused by configuration, seat Policy, player seed, and game seed. Planned.

## 12. Correctness Versus Gameplay Quality

**Correctness** is established by the assertions ([§7](#7-run-assertions)) and by the mandatory tests ([§14](#14-mandatory-tests)).

**Gameplay quality** asks whether the world produces interesting choices: do small players matter, do governments have reasons to negotiate, are shortages consequential without being permanently disabling, does cooperation pay, can several strategies succeed, do early decisions matter later, and is the economy responsive without excessive volatility.

The indicators below are the target. Actions, balance measures, and objectives are on the run page today. The others are planned. The Dashboard supports these judgments with indicators. It does not replace human playtesting.

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

# Part VI: Reference Test Suite

## 13. Reference Coverage

The initial suite runs *Meridian* (the reference Gameboard `meridian`, version `v0`). Its contents are defined in the reference test instance specification and are not restated here.

Every one of the fifteen seats receives a simple, predictable preset seat Policy, so its actions can be checked independently.

## 14. Mandatory Tests

Tests that exercise a reference fixture name it and do not re-describe it. Each test passes when the cited fixture's expected values hold and the listed invariants pass. Tests 1–6, 7b–7d, and 8–12 are in `packages/simulator/test/Main.purs`, test 8b in `packages/simulator/test/Test/Simulator/Shock.purs`, test 13 in `packages/adapters/test/Main.purs`. Tests 7 and 14 analyse runs through `Experiments.Agency`, so they run in the experiments suite (`packages/experiments/test/Test/Experiments/Mandatory.purs`). The fixtures and the capstone run that these suites share are in `testkit`.

| # | Test | Procedure | Pass condition |
|---|---|---|---|
| 1 | Full game | Three preset configurations: all `Conservative`. All `Passive`. `RandomValid` across two player seeds with the game seed fixed. | Five valid rounds with the invariants [§7.1](#71-required-assertions) marks Implemented holding. The cycle sustains five years ([INV-05](02_type_system.md#inv-05)). The same seeds reproduce, a drawn player seed and the same value injected give identical records and spec, and different player seeds give different valid histories under one game seed ([INV-13](02_type_system.md#inv-13)). |
| 2 | Replay | Run Test 1 twice with the same game and player seeds, and once more under the replay handler from recorded input. | Identical records, snapshots, and final state ([INV-13](02_type_system.md#inv-13)). |
| 3 | Submission order | Permute the order of Intent supply. | Identical settlement ([INV-11](02_type_system.md#inv-11)). |
| 4 | Market exchange | Runs [Fixture A](05_test_instance.md#21-fixture-a-direct-market-exchange), including its limit-price variant. | Fixture values. [INV-04](02_type_system.md#inv-04), [INV-06](02_type_system.md#inv-06). |
| 5 | Germany and EU | Runs [Fixture B](05_test_instance.md#22-fixture-b-germany-and-the-eu-collective-procedure) as a counterfactual pair: Germany signs vs does not sign. | Fixture values. The procedure Proposal executes in the same round only when support exceeds the threshold ([INV-10](02_type_system.md#inv-10)). Signing moves no resources ([INV-03](02_type_system.md#inv-03)). |
| 6 | SME Representation | Runs [Fixture C](05_test_instance.md#23-fixture-c-small-business-owner-representation) (C1–C4 noise-free, C5 seeded noise) with shrinkage 0 and 0.25. | Fixture values, with the factor applied in the following round. C5 target within range and identical on replay. [INV-09](02_type_system.md#inv-09), [INV-13](02_type_system.md#inv-13). |
| 7 | NPC response | Runs [Fixture D](05_test_instance.md#24-fixture-d-china-responds-as-an-npc) as a counterfactual pair: with and without the deployment. | Fixture values. [INV-12](02_type_system.md#inv-12). |
| 7b | Reservation | Runs [Fixture R](05_test_instance.md#133-military-deployment) and R-alone: a deployment reserves Mil, and a Military sale on the same Mil is scaled. The reservation is released at commit. | Fixture values. [INV-04](02_type_system.md#inv-04), [INV-05](02_type_system.md#inv-05). |
| 7c | Deployer | Runs the shipped `Deployer` preset with the other seats Passive, and checks the scaled deployment and sale in the rounds the Atlantic price is above 3. | Exact values from [05 §13.3](05_test_instance.md#133-military-deployment). |
| 7d | Investor | Five seats (US, ENCO, CHIP, SHIP, DEF) play `Investor`, so ENCO signs the capacity investment it is offered. | `energy-capacity-investment` executes, and no Rejected event says the Proposal "must sign". |
| 8 | Scarcity | Runs [Fixture E](05_test_instance.md#25-fixture-e-scarcity-and-trade-restriction) (Round 3 disruption and regional prices). | Qualitative: the fixture's expected propagation holds. [INV-05](02_type_system.md#inv-05), [INV-06](02_type_system.md#inv-06). |
| 8b | Major shock | Every seat plays each preset, calm and with the [§13.6](05_test_instance.md#136-energy-disruption) major shock as overrides, same seats and player seed. A preset flips when its seats propose different kinds of action in some round. | At least four presets flip. The shock is deterministic under the fixed player seed ([INV-13](02_type_system.md#inv-13)). |
| 9 | Conflicting actions | Runs [Fixture F](05_test_instance.md#26-fixture-f-simultaneous-conflicting-orders). | Fixture fills. [INV-11](02_type_system.md#inv-11). |
| 10 | Signed Proposals | Runs [Fixture G1](05_test_instance.md#27-fixture-g-proposals-and-signatures), G2, G3, and G5. | Fully signed Proposals settle atomically with fixture values. Unsigned ones lapse while the round resolves. A shipped deal pays SHIP and is rejected as a unit under an `AccessBan` ([INV-10](02_type_system.md#inv-10)). |
| 11 | Trade restriction | The US enacts an `AccessBan` on China, effective next round ([Fixture E2](05_test_instance.md#25-fixture-e-scarcity-and-trade-restriction)). | Shipping orders on the route become infeasible. The EU publishes and passes its tariff Proposal in the round the ban is in force, and the tariff applies from the next round. |
| 12 | Resolution consistency | [§7.3](#73-resolution-consistency-test). | [INV-14](02_type_system.md#inv-14). |
| 13 | Persistence | Save, reload, and inspect all snapshots. | Identical run ([INV-31](02_type_system.md#inv-31)). |
| 14 | Agency | [§10.5](#105-agency-analysis) across all fifteen roles. | Every role has measurable effect and its objective criteria are reported. Failures are reported as balancing findings. |

**Live interpretation check (one-off, billed, not in CI).** `just live-interpret` makes one real model call over a fixed set of transcripts (`packages/adapters/test/Test/Adapters/LiveInterpret.purs`). It passes when the reply decodes and the two transcripts that describe one deal reconcile into one Proposal signed by both. It refuses to run unless `ALLOW_BILLED_LIVE=1` is set, and no suite (`spago test -p`, `just test`, `just check`, `just all`, `just dashboard-test`) runs it.

---

## 15. Summary

The Simulator executes complete games without human participants. Simulated seats are driven by seat Policies (preset strategies, AI agents, or recorded humans) that submit ordinary Intents, including multi-signer Proposals for deals. NPCs, Aggregates, Representation, collective procedures, and settlement all run through the unchanged engine. Every run is an immutable manifest plus append-only round records, from which all metrics derive.

The development loop is:

**Configure → Simulate → Inspect → Compare (including counterfactuals and agency) → Adjust → Simulate again**
