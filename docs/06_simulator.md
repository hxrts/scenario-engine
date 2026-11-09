# Simulator and Dashboard Specification

**Reference setting:** Meridian ([05](05_test_instance.md))

---

## Terms

The store, derivation, and analysis terms of this layer. The type-system terms are in the [Type System glossary](02_type_system.md#25-glossary).

| Term | Definition |
|---|---|
| **Run Store** | The persistent store of runs: manifests, specs, round records, snapshots, assertions, and metrics (§8). |
| **Spec** | A run's stored specification: the string fields it was launched or derived with, beside its manifest. |
| **Derivation of a run** | A new run started from a stored one, of kind `duplicate`, `rerun`, `variant`, or `counterfactual` (§8.1). It is distinct from derivation of a Setting ([03 Terms](03_gameboard.md#terms)). |
| **Rerun** | The zero-change derivation of a run: its resolved configuration and both seeds with nothing changed, so its round records must equal the source's. |
| **Counterfactual** | A derivation of a run that changes one thing by override and compares the result with the source. |
| **Net worth** | Money plus each Resource that a Market trades against Money, valued at its marginal price. Where several Markets trade one Resource, its price is their mean, so a holding counts once. A Resource with no Market is not counted. |
| **Spread** | The highest minus the lowest final net worth of the players. |
| **Gini coefficient** | A measure of final net worths: 0 when every player holds the same and 1 − 1/n when one holds everything. It is n/a when the total is zero or a value is negative. |
| **Representation influence ratio** | Next-round Aggregate change attributable to a represented Actor, divided by that Actor's executed purchase. |

## 1. Purpose

The simulator plays complete games automatically and deterministically. Developers use it to check implementation correctness and to improve gameplay through repeated experiments. It is a development component. It adds no economic model of its own: it runs the Type System, Gameboard, and Instantiated Game machinery. Invariants are cited by ID ([02 §9](02_type_system.md#9-architectural-invariants-and-numerics)).

A developer configures a game, binds a seat Policy to every seat, runs the whole game, saves it, inspects it, compares it with other runs (including controlled counterfactuals), and varies parameters to study correctness, balance, and player agency.

A failed invariant is reported on the run and never compensated for ([§7](#7-run-assertions)).

## 2. Position and Separation of Concerns

The simulator is a development component, not a layer of economic semantics.

**Setting → `assemble` → Game + player seed → Simulation Run → Run Store → Dashboard**

| Component | Responsibility |
|---|---|
| **Simulator** | Plays a Game through the round runner and engine, records each round, and checks assertions (§7). |
| **Read API and Dashboard** | Serve and present run views. Read-only, except the run operations in §8.1. |

The simulator plays the same Game that live play runs, through the same round runner and engine ([INV-29](02_type_system.md#inv-29)). A simulated game is economically equivalent to a live game in which humans submitted the same canonical Intents.

The simulator is a set of effect handlers ([02 §8.4](02_type_system.md#84-effects-and-handlers), [INV-36](02_type_system.md#inv-36)). A simulation handler answers seat inputs with the bound seat Policies, drawing randomness from the player seed. A replay handler answers the same inputs from recorded rounds ([04 §11.1](04_instantiated_game.md#111-round-record)). Round writes reach the Run Store through the `RECORD` operation.

The engine owns all economic behaviour ([02 §8.1](02_type_system.md#81-canonical-resolution-sequence), [04 §11](04_instantiated_game.md#11-round-execution)). The simulator generates inputs, records results, and evaluates them. The Dashboard never alters authoritative state or run histories. Changing any parameter creates a new run. Resume (§8.1) is the one operation that adds rounds to an existing run.

---

# Part I: Simulation Model

## 3. Simulation Specification

A simulation is one complete automated playthrough: a call over an assembled Game ([07 §4](07_architecture.md#4-key-interfaces)). The seat bindings are chosen when the game is assembled, not inside the simulator. The simulator rejects a game with an `Agent` or `Human` seat (`GameError.NotSimulable`). The player seed is an argument, and it drives all seat-Policy randomness. The game seed is part of the World State. The program boundary resolves the player seed to a concrete value before the call (§5), and the stored spec records that value.

The stored spec records how the run was made: the game id, the preset every seat plays (absent for a live session), the resolved player seed, the overrides, the round count, and, for a derived run, its `source` and `kind`. A live session also records its model and human seats. With the same engine version, the spec and the seeds reproduce the complete economic history.

### 3.1 Configuration inheritance

A specification may reference a baseline and apply declarative overrides: the game seed, a parameter, an Entity's capacity holding, a scheme's shrinkage, or making an Actor implicit. A seat's preset is not an override: a stored run plays one preset on every seat. The layer rules for which changes an override may make are in [03](03_gameboard.md) and [04 §2](04_instantiated_game.md#2-instantiation-configuration).

Each run stores its overrides in its spec and its initial World State in its manifest, so later changes to a baseline never alter historical runs.

## 4. Seat Policies

The engine accepts only Intents ([02 §4](02_type_system.md#4-policies)). In simulation each seat is bound, at assembly, to a **seat Policy** ([04 §3.1](04_instantiated_game.md#31-selection-and-control)). Swapping a seat's Policy never changes the Actor's definition and never turns a seat into an NPC ([INV-30](02_type_system.md#inv-30)).

The simulator runs two machine bindings: a preset strategy and a script of literal Proposals and signatures per round. A replay answers a live session's seats from such a script. Agent and human seats are answered only in live play, where each is briefed with its Actor's objective ([05 §9.3](05_test_instance.md#93-objectives)). Presets run on the shared decision-language interpreter that NPC and Aggregate Policies use ([02 §4.4](02_type_system.md#44-shared-decision-language-interpreter), [INV-29](02_type_system.md#inv-29)).

Every kind sees only its Actor's authorized view ([INV-16](02_type_system.md#inv-16)) and maps it to Intents. Every accepted Intent is recorded, so every run replays from its round records. Seat Policies never mutate World State: each Intent passes ordinary validation and settlement.

### 4.1 Bindings and coverage

The simulator never invents default decisions. A seat meant to stay silent is bound explicitly to an empty script. Which bindings an assembled game must have, and the errors for a missing or misplaced one, are in [04 §3.1](04_instantiated_game.md#31-selection-and-control). A seat Policy that produces nothing submits nothing for the round, and nothing is recorded for it.

### 4.2 Presets

Presets are named parameter sets of the shared interpreter, not separate strategies. The executable definition is the settings package's preset file, compiled into each setting. A setting binds the presets to its own staple Resource and its money, build, investment, and deployment Transformations. "Cheapest" and "costliest" mean the staple Markets the seat can access with the lowest and highest opening price ([02 §4.4](02_type_system.md#44-shared-decision-language-interpreter)). A seat with access to none trades nowhere. Quantities are in Resource units per round.

| Preset | Behaviour |
|---|---|
| `Passive` | Submits nothing. |
| `Conservative` | Buys 5 of the staple at the cheapest Market, limit 2.5, while its staple holding is below 30 and that price is at most 2.5. |
| `Aggressive` | Runs the bound build Transformation at its bound quantity every round, with no restrictions. |
| `Accumulator` | Buys 10 of the staple at the cheapest Market, limit 3, while that price is at most 3. No holding target. |
| `Trader` | Buys 5 of the staple at the cheapest Market, limit 2.2, while that price is at most 2.2. Sells 5 at the costliest Market, limit 3, while that price is above 3. |
| `Cooperative` | Proposes nothing. Signs every Proposal addressed to it. |
| `Opportunistic` | Sells 5 of the staple at the costliest Market, limit 3, while that price is above 3. Signs every Proposal addressed to it. |
| `Investor` | Runs the bound investment Transformation for 1 unit while its Money holding is above 100, as a joint Proposal co-signed by the capacity's owner. Signs every Proposal addressed to it. |
| `CrisisResponse` | Buys 3 of the staple at the cheapest Market, limit 4, while that price is above 3.5 and its staple holding is below 20. |
| `RandomValid` | Each round, one seeded draw picks one option: no action, buy 5 of the staple (limit 3), or sell 5 at the costliest Market (limit 2). |
| `Deployer` | While the setting's reference Market price is above its deployment threshold, orders the bound deployment and a sale of the bound Resource in the same round. Both are scaled to the seat's holding. |

`GET /api/setting` lists these names, in the order written, for each setting that has them. The launch form offers the same names.

## 5. Deterministic Randomness

Randomness enters only as keyed draws on a seed ([02 §9.1](02_type_system.md#91-numerics-and-randomness)), where a Gameboard, an estimator, or a preset declares a stochastic choice ([INV-13](02_type_system.md#inv-13)).

**The seed rule.** A run has one player seed, resolved once, before the run starts, at the program boundary. An injected seed is used as given. An absent seed is drawn from the operating system exactly once. The concrete value is what the run uses and what its spec records; a stored spec never holds an absent value. Each entry point resolves it this way: the CLI draws when `--seed` is absent and prints the seed; the server draws when `playerSeed` is null or absent and names the seed in its reply; the Dashboard draws for a blank seed.

The simulator functions take a concrete seed and read no entropy, so the same seed gives the same run.

**The two streams.** The game seed and the player seed never share draws. Their keys and what each one draws are in [04 §7.3](04_instantiated_game.md#73-seeds-and-randomness). A live session also records a player seed, but its agent and human seats draw nothing from it, and the model is not seeded.

Fixing one stream and varying the other isolates its effect: varying the player seed shows the spread caused by player behaviour alone, and varying the game seed shows in-world stochastic variation. Fixing both gives counterfactuals, whose differences come only from the intervention.

---

# Part II: Execution and Assertions

## 6. Lifecycle and Execution

A run's status is `running`, `completed`, or `failed`. A run is `running` from its creation until it finishes, and its final status is written when it finishes. A failed run keeps its last valid state and diagnostics. The Dashboard's run list adds `unreadable` for a run it cannot decode.

Each round follows the canonical sequence ([02 §8.1](02_type_system.md#81-canonical-resolution-sequence)). The simulator drives the same round runner and engine as live play, with seat Policies bound. Its own work is supplying the seat inputs and saving each round's record, snapshot, assertion results, and metrics once the round is complete. It does not evaluate NPC or Aggregate Policies, compute Representation estimates or procedure support, or repeat Aggregate actions.

Two operations drive a run: **run to completion**, and **extend**, which continues from a stored snapshot. A **single-round step** plays one round of a completed run short of its target, through extend's snapshot path.

### 6.1 Invalid actions and failures

An invalid action is an ordinary outcome. It is recorded with its reason and handled by the engine's deterministic reject or partial-fill rules ([02 §3.2](02_type_system.md#32-infeasible-actions)). The round record keeps both the proposed and the executed action.

An invariant violation is reported, not blocked. The round's assertions are stored beside it, the status stays `completed`, and the run list counts them. The store keeps the configuration, the seeds, every round's state and record, and the violated invariant, so a failure can be reproduced and turned into a regression test.

A run is never left `running` once its work has ended. It is marked `failed`, with a reason, in these cases:

- the engine returns an error (the reason is the error);
- an exception occurs while simulating or saving a round, such as a store fault (the reason is the exception);
- a live session stops early: a language-model backend cannot start, exits non-zero, or its input ends at a prompt (the reason names the cause). A backend failure is fatal: the round in progress is not completed, and the rounds already saved remain.

**Interruption.** A run records the process that created it (`owner`: its role, process id, and start time). When a server starts, runs still marked `running` are marked `failed` with a reason beginning `interrupted` if a server created them, if they have no owner, or if their creating process has gone. A run owned by another live process, such as a CLI session on the same store, is left alone.

### 6.2 Commands

| Command | Behaviour |
|---|---|
| `run <setting> [--preset P] [--rounds n] [--seed n]` (`just sim`) | Plays every seat with preset `P` (default `Passive`) for `n` rounds. `n` is a whole number of at least 1, and defaults to the setting's configured count; `checkRounds` refuses a count outside the setting's round range. Prints a header, one line per round, and the invariant results. Does not save to the Run Store. |
| `session <setting> [--humans S,…] [--model id] [--rounds n] [--window s] [--id name] [--db file] [--seed n]` (`just session`) | Live facilitator console. Seats named in `--humans` answer with a typed transcript each round; the rest are AI-agent seats. `--window` is whole seconds, where 0 gives no propose window. The id is 1 to 64 letters, digits, `-` or `_`, starting with a letter or digit. Defaults: window 0, id `session-1`, database `runs.db`, and the configured model. Saves the session as a run; raw model output and call evidence are kept beside it (§8). |
| `presets` | Prints the shipped preset names, one per line. |
| `server [--db file] [--port n] [--static dir]` (`just serve`) | Serves the read API and the dashboard. Defaults: `runs.db`, port 8787 (1 to 65535), and the dashboard build directory. |

An unknown, repeated, or valueless flag, or a bad value, is a usage error and exits 1. The setting argument is the setting's directory name (`meridian`), and the CLI loads its version `v0`. The dashboard's dev proxy and its end-to-end server use separate ports, 8787 and 8799.

## 7. Run Assertions

Assertions check engine properties after every round, without reimplementing the engine. They are pure predicates over the records: the invariant checks applied to each round, the setting's expectations ([03](03_gameboard.md)) checked against each round's observed record, and the objective scoring of each Actor ([§9.2](#92-run-details)). Initialization is not asserted separately.

### 7.1 Required assertions

Each assertion checks one invariant from the Type System master table ([02 §9](02_type_system.md#9-architectural-invariants-and-numerics)). The status column says what is checked per round today.

| Assertion | Invariant | Status |
|---|---|---|
| Unified action semantics | [INV-01](02_type_system.md#inv-01) | Implemented (settlement events against Intents and declared flows) |
| Authority separation | [INV-02](02_type_system.md#inv-02) | Implemented |
| No authority amplification | [INV-03](02_type_system.md#inv-03) | Implemented |
| Resource consistency | [INV-04](02_type_system.md#inv-04) | Implemented |
| Stock-flow reconciliation | [INV-05](02_type_system.md#inv-05) | Implemented |
| Market solvency | [INV-06](02_type_system.md#inv-06) | Implemented |
| Price is not production | [INV-07](02_type_system.md#inv-07) | Implemented (no leg a Market curve settles comes from the source or goes to the sink) |
| Account partition | [INV-08](02_type_system.md#inv-08) | Implemented per round for Resources drawn from their holder |
| Representation consistency | [INV-09](02_type_system.md#inv-09) | Implemented for the estimator bound |
| Signature completeness | [INV-10](02_type_system.md#inv-10) | Implemented |
| Order independence | [INV-11](02_type_system.md#inv-11) | Implemented as `orderIndependent`, which replays the round in other orders. Run by §14 test 3, not per round in play |
| Policy reproducibility | [INV-12](02_type_system.md#inv-12) | Not checked per round. Covered by §14 tests 2 and 13 |
| Deterministic replay | [INV-13](02_type_system.md#inv-13) | Tested by §14 test 2 |
| Resolution consistency | [INV-14](02_type_system.md#inv-14) | Tested by §14 test 12 |
| Single authoritative state | [INV-15](02_type_system.md#inv-15) | Not checked per round. Structural: the engine's one round update holds a single World State |
| Information isolation | [INV-16](02_type_system.md#inv-16) | Deferred until players see different information |

Simulator invariants [INV-29](02_type_system.md#inv-29) and [INV-30](02_type_system.md#inv-30) are checked by the handler suite, and [INV-31](02_type_system.md#inv-31) by §14 test 13. Conservation checks read each Resource's declared properties and the Transformations that produce or consume it ([INV-04](02_type_system.md#inv-04), [INV-05](02_type_system.md#inv-05)). They hard-code no Resource, because a Resource can be produced, consumed, reserved, or issued by design, so a blanket "totals are constant" check would be wrong.

A failed assertion records its invariant ID and message with its round. The run list reports the number of failing assertions. The run page shows each failed assertion with its expected and observed values, when it compares two, and the round's events that concern it.

### 7.2 Metamorphic tests

These tests transform a configuration in a controlled way. Those marked with a test number are implemented.

- Reorder submissions ([INV-11](02_type_system.md#inv-11)), replay with the same seeds ([INV-13](02_type_system.md#inv-13)), withhold one signature ([INV-10](02_type_system.md#inv-10)), and change explicitness (§7.3): tests 3, 2 and 13, 10, and 12.
- Change one seat Policy or one Market parameter: the `variant` and `counterfactual` derivations (§10.3).
- Remove all representative observations: Aggregates follow their baselines ([INV-09](02_type_system.md#inv-09)). An unnumbered test in §14.

### 7.3 Resolution-consistency test

This is the direct check of [INV-14](02_type_system.md#inv-14). It instantiates the reference setting twice, once with a government's activity implicit and once with that government explicit ([05 §10.1](05_test_instance.md#101-germany-and-the-eu), [05 §14.2](05_test_instance.md#142-resolution-consistency)). At initialization the two instances must reconcile stocks, give every liability and capability exactly one holder, and cover each behavioural responsibility exactly once; any difference is an accounting defect. The explicit instance is not played further, because an implicit government spends its members' flows internally, which no seat Policy can reproduce action for action.

### 7.4 AI interpretation tests

Text-to-Intent interpretation is tested apart from the simulator, which consumes canonical Intents and never calls a language model. The interpretation suites cover prompts, reply decoding, evidence, and the process boundary, using a stub backend or a replayed recording, with no model call. They check text inputs against their expected canonical Intents, cross-player cases (a joint Proposal described by both parties reconciles, and one described by a single party lapses), and inadmissible claims, which the engine rejects as a `Rejected` event ([04 §11.2](04_instantiated_game.md#112-live-session)). Accepted canonical Intents are the replay boundary.

---

# Part III: Run Store

## 8. Persistence

The Run Store is the persistent store of runs. Every run is persisted and survives restarts. Round writes, lifecycle writes, and reads go through separate effect operations ([02 §8.4](02_type_system.md#84-effects-and-handlers)), and the server reaches them only through the operations in §8.1.

| Field | Contents |
|---|---|
| **Run ID, name, status** | Identity and lifecycle state, with `created`, `reason` (for a failed run), and the derivation link (`source`, with its `kind` in the spec). |
| **Manifest** | The manifest defined in [04 Terms](04_instantiated_game.md#terms), stored with the run. The preset, player seed, round count, and overrides are in the spec ([04 §9](04_instantiated_game.md#9-runtime-state-and-lifecycle)). |
| **Round records** | Append-only Proposals, signatures, lapsed Proposals, proposed and executed actions, procedure Proposals with their support and outcome, stored Representation estimates, and events. |
| **Snapshots** | One per round. The initial World State is in the manifest. |
| **Assertions** | Failures per round, with their invariant IDs. |
| **Metrics** | Derived projections (for example `price:<market>` after each round), tagged with the version of the metric definitions (`v1`). |

**Source of truth ([INV-31](02_type_system.md#inv-31)).** The manifest, round records, and snapshots are authoritative. Metrics are reproducible projections of them: when a metric definition changes, its values are recomputed and the records stay untouched. Snapshots support fast inspection, and records explain outcomes. Neither replaces the other.

**Mutability.** Completed runs are immutable, and a changed configuration always gets a new run ID. Round records, snapshots, and metrics are never edited. A run's status, failure reason, archive flag, and spec change only through the operations in §8.1. Resume is the only one that adds rounds, and it keeps every existing round record. Delete removes a run with its rounds and metrics.

**Evidence.** A live session also keeps evidence of each language-model call: the effect, the model ID, template and input hashes, a pointer to the raw output, and the decoded result. It lives beside the run, outside the round records. Replay never re-invokes the model; the canonical Proposals and signatures in the round records are enough.

**Writes.** The initial record is written before Round 1. After each round, the round record, snapshot, assertions, and metrics are saved in one transaction. If Round 4 fails, Rounds 1 to 3 remain available.

**Rejection reasons.** A `RejectReason` is stored as a structured value, so a stored event reads back equal to the event that was recorded. Its display text is derived from the value when shown and is not stored.

**Schema.** The store is SQLite. Its schema version is the database's user version. There is one schema, version 1, and there are no migrations: a change to the schema starts a new store. The one exception is additive: `scenario_sets` (`name`, `baseline`, `runs` as a JSON array of run IDs) is created wherever it is absent, and the store keeps its version.

| Table | Columns | Key |
|---|---|---|
| `runs` | `id`, `name`, `status`, `spec`, `manifest`, `created`, `reason`, `archived` (default 0; 1 hides the run from the default list), `source` (the run it was derived from), `owner` (role, process id, start time) | `id` |
| `rounds` | `run`, `round`, `record`, `snapshot`, `assertions`, `transcripts` (JSON, the round's seat transcripts) | `run`, `round` |
| `metrics` | `run`, `metric`, `version`, `round`, `value` | `run`, `metric`, `version`, `round` |

**Access.** Reads and writes go only through the store interface. A store at another schema version is refused, a value that does not decode fails only its own field, and metric rows carry their definition's version so projections are never mixed.

**Replay.** Replay requires a record for every round from 1 to n. A missing round is an error that names it, never a silent replay with no seat inputs.

**Identity.** The manifest records the draw version, since a stored seed is not enough without the versioned random generator. Run IDs never influence sampling, and a new run's ID is never one that a stored run still cites as its source.

### 8.1 Read API and run operations

The server serves JSON through the `API` operations. Each operation below is one route of the endpoint table, which a browser client shares. Any other method or path under `/api/` is a 404, and a body over 1 MiB is a 413. Replies are 200 unless stated. The server loads every setting under the setting root; an operation on a run uses the run's own setting, and a run whose setting is not loaded is refused with the reason.

| Operation | Purpose | Replies |
|---|---|---|
| `GET /api/setting` | The launch form's settings: name, version, round range, default rounds, game seed, `defaultName` (the next run ID), and the seat presets in order. | 200 |
| `GET /api/runs` | The run list, one row per run, with its id, archived, readable, and source flags. Archived runs are left out unless `archived=true`. | 200 |
| `POST /api/runs` | Launch a run of one preset on every seat: setting, name, preset, player seed (null draws), rounds (null for the default). | 200 with the new id and seed used. 400 for a bad or refused launch, including rounds outside the range |
| `POST /api/batch` | One run of one preset on each named setting (every loaded one by default), so they can be read through Compare. Each run records its own setting. The batch gives no score of how well a setting plays. | 200 with a list of runs. 400 for a bad body or a setting that cannot start |
| `GET /api/runs/:id/:view` | One of 14 views: `overview`, `objectives`, `balances`, `markets`, `facilities`, `economy`, `representation`, `procedures`, `events`, `standings`, `networth`, `actions`, `drivers`, `balance`. `round` applies to `balances` (default: the final round) and `events` (default 1). | 200. 400 for a bad round. 404 for an unknown run or view. 409 when the view needs a setting that is not loaded. 500 for an unreadable run |
| `GET /api/runs/:id/entities` | The run's Entities, each marked as a seat or not, and the default Entity. | 200. 404. 500 |
| `GET /api/runs/:id/entity/:entity` | One Entity's balance sheet after every round, with a chart and its net worth. | 200. 400 for an Entity the run does not have, listing the valid names. 404. 500 |
| `POST /api/runs/:id/derive` | Derive a run of kind `duplicate`, `rerun`, `variant`, or `counterfactual`, with optional name, preset, player seed, rounds, and overrides. | 201 with the new id, source, and kind (plus `recordsMatch` for a rerun). 400 for a bad kind, field, preset, or override. 404 for an unknown source. 409 when the source cannot be compared or replayed. 500 |
| `POST /api/runs/:id/resume` | Extend a completed run to a total round count. | 200 with id and rounds. 400 when the run is not completed, is a live session, or the count is not above its completed rounds. 404. 500 |
| `POST /api/runs/:id/step` | Play one round of a completed run short of its target. | 200 with id and rounds. 400 when the run is not completed, is a live session, or has played all its rounds. 404 for an unknown id. |
| `POST /api/runs/:id/archive`, `…/unarchive` | Set or clear the archive flag. Idempotent. | 200 with id and archived. 404 |
| `DELETE /api/runs/:id?confirm=:id` | Delete the run with its rounds and metrics. `confirm` must name the run. | 200. 400 without a matching `confirm`. 404 |
| `GET /api/compare?runs=a,b[&market=m]` | Compare a scenario set. The first valid run is the baseline. Each table marks the cells that differ from the baseline. | 200. 400 when no run is valid or `market` is unknown |
| `GET /api/glossary` | The definitions and reasons the dashboard shows, generated from the columns the views use. Served whole, so the dashboard holds no glossary text. | 200 |
| `GET /api/agency` | Agency analysis of a setting (§10.4): every seat plays `preset`, then each Actor in turn plays `alternative`. | 200. 400 for an unknown setting, a bad seed, or an unknown preset |
| `GET /api/agency/detail` | The same analysis, beyond its table: each Actor's objective criteria under the baseline and the alternative. | 200 with the view model. 400 for a bad query. |
| `POST /api/experiments` | Run a Variant Experiment (§11) and save its runs as a named set. | 200 with the set and its run IDs. 400 for a name in use, an empty experiment, a bad round count, or a game that does not start. |
| `GET /api/sets` | The named scenario sets. | 200 with the sets. |
| `GET /api/sets/:name/summary` | The final measures across a set's runs (§11). | 200 with the table. 404 for an unknown set. 500 when one of its runs cannot be read. |
| `POST /api/sets` | Save a set: `name`, `baseline`, `runs`. | 200 with the set. 400 for a blank name, no runs, a baseline outside the runs, or an unknown run. 409 for a name in use. |
| `POST /api/sets/:name/rename` | Rename a set: `name`. | 200 with the new name. 400 for a blank name. 404 for an unknown set. 409 for a name in use. |
| `DELETE /api/sets/:name` | Delete a set. Its runs are untouched. | 200 with the name. 404 for an unknown set. |

Operation rules:

- **Derive.** `duplicate` and `rerun` take no spec fields beyond the name. A `variant` replaces the preset, the player seed, or the rounds, and takes no overrides. A `counterfactual` requires at least one override, which it adds to the source's, and takes no preset, seed, or rounds. The default name is `<source name> (<kind>)`. Every derived run stores `source` and `kind` in its spec, and `source` in its own column.
- **Rerun.** `recordsMatch` is `true` when the rerun's round records equal the source's. It is `false` for nondeterminism, a stored-versus-live mismatch, or an unrecorded seed, and that is a finding about reproducibility, not a reply error. A source that is not completed, or has no stored records, is refused with 409 and nothing is created.
- **Sessions.** A live session stores no preset, so `variant` and `counterfactual` from one are refused, and so is `resume`. `duplicate` and `rerun` replay it without a model call ([04 §11.1](04_instantiated_game.md#111-round-record)).
- **Resume.** Starts from the snapshot after the run's last completed round and updates the stored round count. The run keeps its ID.
- **Deleted sources.** A derived run whose source was deleted keeps its `source`, which then shows as deleted.

Dashboard controls exist for launch, resume, archive, delete, and the four derivations (duplicate, rerun, variant, and counterfactual). The store interface supports save, retrieve, list, full round histories, resume, and archive or delete.

---

# Part IV: Dashboard

## 9. Pages

The Dashboard has three pages, each with its own hash route and a top bar that links to them from every page: **Runs** (`#/runs`, the home page, and `#/runs/:id` with that run selected), **Prepare** (`#/prepare`), and **Compare** (`#/compare`, and `#/compare/:ids` with the ticked runs). It renders from persisted data and does not run the engine. Every number shows its display kind (quantity, price, or ratio) as [02 §9.1](02_type_system.md#91-numerics-and-randomness) defines it. Every visible column and field has a plain-language definition on hover, from the glossary (§8.1).

### 9.1 Simulation Runs

The run list is a sortable table, one row per stored run, in creation order. It shows the run's name, Gameboard, seeds, completed rounds, status, failing assertions, reason for a failure, archive flag, and source. A run that cannot be decoded shows status `unreadable` and its reason, and the other runs stay visible. A **Show archived runs** toggle adds archived runs. Selecting a row, or opening `#/runs/:id`, shows that run's results beside the list; with no selection, the latest readable run is shown. **Resume** appears only for a completed run that has a preset, and never for a failed or interrupted run. **Archive**, **Unarchive**, and **Delete** (which requires the run's id to be confirmed) are under "Manage this run" on the run page.
### 9.2 Run Details

The page opens with a header of the run's facts. Its sections follow in order: key figures; marginal prices of each Market; standings; net worth over time; actions; drivers (rules enacted and repealed, procedure outcomes, shortfalls); balance measures (spread and Gini); objectives; the final balance sheet of every Entity (`Initial`, `Current`, `Change`); and one Entity's balance sheet, selected from the run's Entities, defaulting to the first seat.

Standings rank 1 is the highest net worth, and equal net worths share a rank. Aggregates are residual representations, not players. An objective criterion is met, not met, or not evaluated; it is never reported as met when its series is empty.

The run page shows the representation and procedure records. The round-by-round action counts, the Market, facility, and economy views, and the raw event log are served by the API only. Representation records span two rounds: the observation, the estimate, and the Aggregate's purchase that applies the stored factor, from which the influence ratio is derived. The event inspector lists one round's events in engine order, with a text filter.

### 9.3 Prepare

Prepare launches a run of a setting. Its fields are a name (default: the next run id), a preset (default `Conservative`, the Policy every seat plays), the setting (shown only when the server offers more than one), the rounds (default: the setting's default, within its range), and a player seed (blank draws one at launch, and a whole number fixes it). The game seed is shown read-only, because the server takes no game seed at launch. A summary line states what Run will play. An accepted launch opens the new run; a refused one shows the server's message.

### 9.4 Compare

The Compare page lists every readable run as a tick box. The ticked runs, in the order ticked, form the scenario set, and the first is the baseline. The page shows prices by Market, net worth over time, final standings, the spread and Gini, a key-values table, and detail tabs (final state, baseline difference, configuration, compatibility, seed variation, and agency, §10.5). Unreadable runs are listed as skipped, with their reasons. Every statistic is computed in the views, not in the browser.

## 10. Comparison

Comparison supports both correctness testing and balancing. The baseline is the first ticked run.

### 10.1 Selection and compatibility

The Dashboard names each difference between runs that are not directly comparable (Gameboard or engine versions, round counts, units, Market definitions, seat presets). Incompatible configurations and metrics are flagged, never combined. Trajectories align by simulated time, without interpolation.

### 10.2 Views

- **Trajectory:** one metric across runs, such as each Market's price or each player's net worth.
- **Final state:** ending holdings side by side, with differing values highlighted.
- **Difference from baseline:** the holdings that differ from the baseline, as changes.
- **Configuration difference:** parameter differences, one row each, categorised as version, seat Policy, seed, or other. The other category holds deliberate changes such as overrides, so they are distinguished from the rest.

### 10.3 Counterfactual experiments

A **rerun** is the zero-change case: it reproduces the source's configuration and both seeds, so `recordsMatch: true` confirms that the source is reproducible (§8.1).

A **counterfactual** is the intervention case. It duplicates a run's resolved configuration and both seeds, changes exactly one thing by `overrides` (such as one seat's signature or one deployment), and stores the result as an ordinary immutable run linked to its source. The Compare page then sets the original as the baseline. A `variant` replaces the preset on every seat instead. Changing one seat's Policy alone is a pure analysis that stores no run.

With both seeds and every draw key fixed, the difference is the intervention's total modelled effect, including downstream consequences.

### 10.4 Agency analysis

For each Actor, the simulator replays the game with the alternative preset for that Actor and everything else fixed. It measures the change in prices, in other Actors' holdings, in Aggregate behaviour, in institutional decisions, and in the Actor's own objective criteria ([05 §9.3](05_test_instance.md#93-objectives)). An unknown preset name is an error, never a silent baseline.

The Compare page's **Agency (fixed baseline)** tab shows one row per Actor under the alternative. A row whose alternative makes negligible difference on every measure is marked `no: balancing finding`. Under it, each Actor's objective criteria are shown under the baseline and the alternative (`GET /api/agency/detail`). The agency profile per role gives one row per Actor: its measures under the alternative, and its criteria met either way.

### 10.5 Saved comparisons

The comparison's runs and baseline are carried in the route. A named scenario set stores a name, the IDs of immutable runs, and the baseline, and never copies run data. Deleting a run a set names is refused with a 409 naming the set. The Compare page opens, saves, renames, and deletes sets.

---

# Part V: Experimentation

## 11. Experiments

A **Variant Experiment** generates one specification for each declared parameter value, runs and saves every resulting game, and opens the runs as a named scenario set (`POST /api/experiments`, and `experiment <setting>` with `--parameter name --values a,b`, `--combine a=1,2;b=3,4`, `--seat id --alternatives A,B`, `--player-seeds 1,2`, or `--game-seeds 1,2` in the CLI). Every run uses one player seed, drawn once if none is given. Analysis functions also compose the simulation directly, scoring results with the checks and presenting them through the views, and `POST /api/batch` plays one run per setting. A Variant Experiment's kinds are single-parameter (one parameter over a list of values), multi-parameter (every combination of the values given), seat-Policy (parameters fixed, one deviating seat among otherwise identical seats), and player-seed and game-seed batches (§5). A set's **batch summary** gives the final measures across its runs: the mean, the spread, and how many runs met each Actor's objective (`GET /api/sets/:name/summary`, and a control on the Compare page). The Seed variation tab separates what depends on the seed from what depends on the specification.

**Whole-economy measures.** The `wholeeconomy` view gives these by round and Market, and the run page shows them:

- **Demand** on Market m in round r: the sum of every positive Trade quantity submitted on m, whether or not it filled.
- **Unfilled demand**: max(0, Demand − bought), where bought is the base quantity players bought on m that round.
- **Concentration**: the Herfindahl index of the players' positive net worth after the round, the sum of each one's share squared. It is 1/n when n players hold equal net worth and 1 when one holds all.
- **Money distribution**: the Gini coefficient of the players' holdings of the numeraire after the round. It is n/a when none is held or a holding is negative.

## 12. Correctness Versus Gameplay Quality

Correctness is established by the assertions (§7) and the mandatory tests (§14). The gameplay questions are in [01 Evaluation](01_overview.md#6-evaluation), and playtesting answers them. The Dashboard computes action counts, the balance measures (spread and Gini), objective criteria, and the whole-economy measures (§11).

---

# Part VI: Reference Test Suite

## 13. Reference Coverage

The suite runs the reference setting, *Meridian* ([05](05_test_instance.md)), with every seat bound to a simple, predictable preset.

## 14. Mandatory Tests

Each test passes when its cited fixture's expected values hold and its listed invariants pass. The suites share their fixtures and the capstone run through `testkit`.

| # | Name | What it checks |
|---|---|---|
| 1 | Full game | Five rounds under three presets (every seat `Conservative`, `Passive`, `RandomValid`), with the game seed fixed and `RandomValid` under two player seeds. Each round is valid, the implemented invariants of §7.1 hold, the same seeds reproduce, and different player seeds give different histories ([INV-13](02_type_system.md#inv-13)). |
| 2 | Replay | `RandomValid` run twice with the same seeds, then once under the replay handler from the recorded input. Round records and snapshots are identical ([INV-13](02_type_system.md#inv-13)). |
| 3 | Submission order | For the `Conservative`, `Aggressive`, and `RandomValid` runs, each round's recorded inputs in three reorderings. Each reordering settles to the recorded world ([INV-11](02_type_system.md#inv-11)). |
| 4 | Market exchange | [Fixture A](05_test_instance.md#21-fixture-a-direct-market-exchange) and its limit-price variant A2. Fixture values; [INV-04](02_type_system.md#inv-04), [INV-06](02_type_system.md#inv-06). |
| 5 | Germany and EU | [Fixture B](05_test_instance.md#22-fixture-b-germany-and-the-eu-collective-procedure) as a pair: Germany signs against not signing. The procedure executes in the same round only when support exceeds the threshold ([INV-10](02_type_system.md#inv-10)); signing moves no resources ([INV-03](02_type_system.md#inv-03)). |
| 6 | SME Representation | [Fixture C](05_test_instance.md#23-fixture-c-small-business-owner-representation), C1 to C3, and C1 with shrinkage 0. The factor applies in the following round, and shrinkage 0 leaves it at 1. Seeded-noise cases are checked in the settings suite ([INV-09](02_type_system.md#inv-09), [INV-13](02_type_system.md#inv-13)). |
| 7 | NPC response | [Fixture D](05_test_instance.md#24-fixture-d-china-responds-as-an-npc) as a pair, with and without the deployment ([INV-12](02_type_system.md#inv-12)). |
| 7b | Reservation | [Fixture R](05_test_instance.md#133-military-deployment) and R-alone: a deployment reserves a Resource, a sale of the same Resource is scaled, and the reservation is released at commit ([INV-04](02_type_system.md#inv-04), [INV-05](02_type_system.md#inv-05)). |
| 7c | Deployer | The shipped `Deployer` preset with the other seats Passive. The scaled deployment and sale occur in the rounds the reference price is above its threshold, with the exact values of [05 §13.3](05_test_instance.md#133-military-deployment). |
| 7d | Investor | Five seats play `Investor`. The capacity investment executes, and no Rejected event says the Proposal "must sign". |
| 8 | Scarcity | [Fixture E](05_test_instance.md#25-fixture-e-scarcity-and-trade-restriction): the round 3 disruption and regional prices. The expected propagation holds ([INV-05](02_type_system.md#inv-05), [INV-06](02_type_system.md#inv-06)). |
| 8b | Major shock | Every seat plays each preset, calm and with the [major shock](05_test_instance.md#136-energy-disruption), under the same seats and player seed. At least four presets flip their proposed kinds of action, deterministically ([INV-13](02_type_system.md#inv-13)). A second check pins the calm and shocked Conservative prices ([05 §13.6](05_test_instance.md#136-energy-disruption)). |
| 9 | Conflicting actions | [Fixture F](05_test_instance.md#26-fixture-f-simultaneous-conflicting-orders). Fixture fills ([INV-11](02_type_system.md#inv-11)). |
| 10 | Signed Proposals | [Fixture G](05_test_instance.md#27-fixture-g-proposals-and-signatures) cases G1, G2, G3, G3-ban, G5, and G5-unsigned. Fully signed Proposals settle atomically with the fixture values, unsigned ones lapse while the round resolves, and a joint Proposal under an `AccessBan` is rejected as a unit ([INV-10](02_type_system.md#inv-10)). |
| 11 (a) | Trade restriction, capstone | The DSL-authored US ban, EU tariff, and a banned shipment in the capstone run. The procedure executes, the tariff is in force, and a `Blocked` rejection is recorded. |
| 11 (b) | Trade restriction, Fixture E2 | [Fixture E2](05_test_instance.md#25-fixture-e-scarcity-and-trade-restriction): the US enacts an `AccessBan` on China in round 2, in force from round 3, and China reciprocates, effective round 4. The fixture's values hold. |
| 12 | Resolution consistency | [§7.3](#73-resolution-consistency-test): the reference setting instantiated with an implicit and an explicit government ([INV-14](02_type_system.md#inv-14)). |
| 13 | Persistence | Save a run to the Run Store, reload it, compare its stored records and snapshots with the pure run, and replay from the stored records. The run is identical ([INV-31](02_type_system.md#inv-31)). |
| 14 | Agency | [§10.4](#104-agency-analysis) across every seat of the reference setting. Under the `RandomValid` baseline, every Actor has a measurable effect and its objective criteria are reported. Under the `Conservative` baseline, the Actors with no measurable effect are pinned as balancing findings, so a setting change is noticed. |

Unnumbered tests in the same suites: removing every representative observation leaves each Aggregate's factors at their baseline with no estimate recorded; every preset plays 8 rounds with the invariants holding; a run resumed from its third snapshot matches the uninterrupted run; the INV-29 and INV-30 checks; and a player-seed batch keeps every game-stream draw identical while seat behaviour varies.

**Live interpretation check (one-off, billed, not in CI).** One real model call over fixed transcripts checks that the reply decodes and that a joint Proposal reconciles. It needs an explicit opt-in, and no suite runs it.
