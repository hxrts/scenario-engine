# Simulator and Dashboard Specification

**Reference PremiseComposition:** Meridian ([06](06_meridian.md))

---

## 1. Purpose

The simulator plays complete Runs automatically and deterministically. Developers use it to check implementation correctness and to improve gameplay through repeated experiments. It is a development component. It adds no economic model of its own: it runs the kernel, PremiseComposition, and Run machinery. Invariants are cited by ID ([02 §9](02_kernel.md#9-architectural-invariants-and-numerics)).

A developer configures a Run, binds a seat Strategy to every seat, plays it, saves it, inspects it, compares it with other runs (including controlled counterfactuals), and varies parameters to study correctness, balance, and player agency.

A failed invariant is reported on the run and never compensated for ([§7](#7-run-assertions)).

## 2. Position and Separation of Concerns

The simulator is a development component, not a layer of economic semantics.

**PremiseComposition → `assemble` → `PreparedRun` + player seed → Simulation Run → Run Store → Dashboard**

| Component | Responsibility |
|---|---|
| **Simulator** | Plays an assembled Run through the round runner and engine, records each round, and checks assertions (§7). |
| **Read API and Dashboard** | Serve and present run views. Read-only, except the run operations in §8.1. |

The simulator plays the same assembled Run that live play runs, through the same round runner and engine ([INV-29](02_kernel.md#inv-29)). A simulated Run is economically equivalent to a live Run in which humans submitted the same canonical Intents.

The simulator is a set of effect handlers ([02 §8.4](02_kernel.md#84-effects-and-handlers), [INV-36](02_kernel.md#inv-36)). A simulation handler answers seat inputs with the bound seat Strategies, drawing randomness from the player seed. A replay handler answers the same inputs from recorded rounds ([04 §11.1](04_run.md#111-round-record)). Round writes reach the Run Store through the `RECORD` operation.

The engine owns all economic behaviour ([02 §8.1](02_kernel.md#81-canonical-resolution-sequence), [04 §11](04_run.md#11-round-execution)). The simulator generates inputs, records results, and evaluates them. The Dashboard never alters authoritative state or run histories. Changing any parameter creates a new run. Resume (§8.1) is the one operation that adds rounds to an existing run.

---

# Part I: Simulation Model

## 3. Simulation Specification

A simulation is one complete automated Run: a call over an assembled `PreparedRun` ([08 §4](08_architecture.md#4-key-interfaces)). The seat bindings are chosen when the Run is assembled, not inside the simulator. The simulator rejects a Run with an `Agent` or `Human` seat (`AssemblyError.NotSimulable`). The player seed is an argument, and it drives all seat-Strategy randomness. The game seed is part of the World. The program boundary resolves the player seed to a concrete value before the call (§5), and the stored spec records that value.

The stored spec records how the run was made: the game id, the preset every seat plays (absent for a live session), the resolved player seed, the overrides, the round count, and, for a derived run, its `source` and `kind`. A live session also records its model and human seats. With the same engine version, the spec and the seeds reproduce the complete economic history.

### 3.1 Configuration inheritance

A specification may reference a baseline and apply declarative overrides: the game seed, a parameter, an Actor's capacity holding, a scheme's shrinkage, or making an Actor implicit. A seat's preset is not an override: a stored run plays one preset on every seat. The layer rules for which changes an override may make are in [03](03_premise.md) and [04 §2](04_run.md#2-instantiation-configuration).

Each run stores its overrides in its spec and its initial World in its manifest, so later changes to a baseline never alter historical runs.

## 4. Seat Strategies

The engine accepts only Intents ([03 §10.4](03_premise.md#104-available-actions)). In simulation each seat is bound, at assembly, to a seat Strategy ([04 §3.1](04_run.md#31-selection-and-control)). Swapping a seat's Strategy never changes the Actor's definition and never turns a seat into a non-playable Actor ([INV-30](02_kernel.md#inv-30)).

The simulator runs two machine bindings: a preset strategy and a script of literal candidate Transactions and signatures per round. A replay answers a live session's seats from such a script. Agent and human seats are answered only in live play, where each is briefed with its Actor's objective ([06 §9.3](06_meridian.md#93-objectives)). Presets run on the shared decision-language interpreter that non-playable Actor and Aggregate Strategies use ([03 §8.1](03_premise.md#81-shared-interpreter), [INV-29](02_kernel.md#inv-29)). A seat whose Actor has a routine plays it after any of these bindings except a script ([04 §6](04_run.md#6-strategy-initialization)).

Every kind sees only its Actor's View ([02 §4](02_kernel.md#4-strategies)) and maps it to Intents. Every accepted Intent is recorded, so every run replays from its round records. Seat Strategies never mutate the World: each Intent passes ordinary validation and settlement.

### 4.1 Bindings and coverage

The simulator never invents default decisions. A seat meant to stay silent is bound explicitly to an empty script. Which bindings an assembled Run must have, and the errors for a missing or misplaced one, are in [04 §3.1](04_run.md#31-selection-and-control). A seat Strategy that produces nothing submits nothing for the round, and nothing is recorded for it.

### 4.2 Presets

Presets are named parameter sets of the shared interpreter, not separate strategies. The executable definition is the premises package's preset file, compiled into each PremiseComposition, and each PremiseComposition offers its own. Meridian, and any composition built like it, binds the shared presets below to its own staple Resource and its money, build, investment, and deployment Transformations. "Cheapest" and "costliest" mean the staple Exchanges the seat can access with the lowest and highest opening price ([03 §8](03_premise.md#8-strategy-catalog)). A seat with access to none trades nowhere. Quantities are in Resource units per round.

| Preset | Behaviour |
|---|---|
| `Passive` | Submits nothing. |
| `Conservative` | Buys 5 of the staple at the cheapest Exchange, limit 2.5, while its staple holding is below 30 and that price is at most 2.5. |
| `Aggressive` | Runs the bound build Transformation at its bound quantity every round, with no restrictions. |
| `Accumulator` | Buys 10 of the staple at the cheapest Exchange, limit 3, while that price is at most 3. No holding target. |
| `Trader` | Buys 5 of the staple at the cheapest Exchange, limit 2.2, while that price is at most 2.2. Sells 5 at the costliest Exchange, limit 3, while that price is above 3. |
| `Cooperative` | Proposes nothing. Signs every candidate Transaction addressed to it. |
| `Opportunistic` | Sells 5 of the staple at the costliest Exchange, limit 3, while that price is above 3. Signs every candidate Transaction addressed to it. |
| `Investor` | Runs the bound investment Transformation for 1 unit while its Money holding is above 100, as a joint candidate Transaction co-signed by the capacity's owner. Signs every candidate Transaction addressed to it. |
| `CrisisResponse` | Buys 3 of the staple at the cheapest Exchange, limit 4, while that price is above 3.5 and its staple holding is below 20. |
| `RandomValid` | Each round, one seeded draw picks one option: no action, buy 5 of the staple (limit 3), or sell 5 at the costliest Exchange (limit 2). |
| `Deployer` | While the PremiseComposition's reference Exchange price is above its deployment threshold, orders the bound deployment and a sale of the bound Resource in the same round. Both are scaled to the seat's holding. |

`GET /api/premise` lists, for each PremiseComposition, the shared names it has in the order written, then its own names in name order. The launch form offers the same names, and choosing another composition keeps the chosen preset only if that composition offers it. Pacific's presets are in [05 §7.2](05_pacific.md#72-presets).

## 5. Deterministic Randomness

Randomness enters only as keyed draws on a seed ([02 §9.1](02_kernel.md#91-numerics-and-randomness)), where a PremiseComposition, an estimator, or a preset declares a stochastic choice ([INV-13](02_kernel.md#inv-13)).

**The seed rule.** A run has one player seed, resolved once, before the run starts, at the program boundary. An injected seed is used as given. An absent seed is drawn from the operating system exactly once. The concrete value is what the run uses and what its spec records; a stored spec never holds an absent value. Each entry point resolves it this way: the CLI draws when `--seed` is absent and prints the seed; the server draws when `playerSeed` is null or absent and names the seed in its reply; the Dashboard draws for a blank seed.

The simulator functions take a concrete seed and read no entropy, so the same seed gives the same run.

**The two streams.** The game seed and the player seed never share draws. Their keys and what each one draws are in [04 §7.3](04_run.md#73-seeds-and-randomness). A live session also records a player seed, but its agent and human seats draw nothing from it, and the model is not seeded.

Fixing one stream and varying the other isolates its effect: varying the player seed shows the spread caused by player behaviour alone, and varying the game seed shows in-world stochastic variation. Fixing both gives counterfactuals, whose differences come only from the intervention.

---

# Part II: Execution and Assertions

## 6. Lifecycle and Execution

A run's status is `running`, `completed`, or `failed`. A run is `running` from its creation until it finishes, and its final status is written when it finishes. A failed run keeps its last valid state and diagnostics. The Dashboard's run list adds `unreadable` for a run it cannot decode.

Each round follows the canonical sequence ([02 §8.1](02_kernel.md#81-canonical-resolution-sequence)). The simulator drives the same round runner and engine as live play, with seat Strategies bound. Its own work is supplying the seat inputs and saving each round's record, round-end World, assertion results, and metrics once the round is complete. It does not evaluate non-playable Actor or Aggregate Strategies, compute Representation estimates or procedure support, or repeat Aggregate actions.

Two operations drive a run: run to completion, and extend, which continues from a stored round-end World. A single-round step plays one round of a completed run short of its target, through extend's path.

### 6.1 Invalid actions and failures

An invalid action is an ordinary outcome. It is recorded with its reason and handled by the engine's deterministic reject or partial-fill rules ([02 §3.6](02_kernel.md#36-infeasible-actions)). The round record keeps both the proposed and the executed action.

An invariant violation is reported, not blocked. The round's assertions are stored beside it, the status stays `completed`, and the run list counts them. The store keeps the configuration, the seeds, every round's state and record, and the violated invariant, so a failure can be reproduced and turned into a regression test.

A run is never left `running` once its work has ended. It is marked `failed`, with a reason, in these cases:

- the engine returns an error (the reason is the error);
- an exception occurs while simulating or saving a round, such as a store fault (the reason is the exception);
- a live session stops early: a language-model backend cannot start, exits non-zero, or its input ends at a prompt (the reason names the cause). A backend failure is fatal: the round in progress is not completed, and the rounds already saved remain.

**Interruption.** A run records the process that created it (`owner`: its role, process id, and start time). When a server starts, runs still marked `running` are marked `failed` with a reason beginning `interrupted` if a server created them, if they have no owner, or if their creating process has gone. A run owned by another live process, such as a CLI session on the same store, is left alone.

### 6.2 Commands

| Command | Behaviour |
|---|---|
| `run <composition> [--preset P] [--rounds n] [--seed n]` (`just sim`) | Plays every seat with preset `P` (default `Passive`) for `n` rounds. `n` is a whole number of at least 1, and defaults to the PremiseComposition's configured count; `checkRounds` refuses a count outside the PremiseComposition's round range. Prints a header, one line per round, and the invariant results. Does not save to the Run Store. |
| `session <premise> [--humans S,…] [--model id] [--rounds n] [--window s] [--id name] [--db file] [--seed n]` (`just session`) | Live facilitator console. Seats named in `--humans` answer with a typed transcript each round; the rest are AI-agent seats. `--window` is whole seconds, where 0 gives no propose window. The id is 1 to 64 letters, digits, `-` or `_`, starting with a letter or digit. Defaults: window 0, id `session-1`, database `runs.db`, and the configured model. Prints each Actor's balance sheet path at the start. Saves the session as a run; raw model output and call evidence are kept beside it (§8). |
| `presets` | Prints the shipped preset names, one per line. |
| `server [--db file] [--port n] [--static dir] [--env file]` (`just serve`) | Serves the API, the landing page, and the dashboard at `/admin` (§8.3). Defaults: `runs.db`, port 8787 (1 to 65535), the dashboard build directory, and `.env`. |

An unknown, repeated, or valueless flag, or a bad value, is a usage error and exits 1. The composition argument is the PremiseComposition's directory name (`meridian`), and the CLI loads its version `v0`. The dashboard's dev proxy and its end-to-end server use separate ports, 8787 and 8799.

## 7. Run Assertions

Assertions check engine properties after every round, without reimplementing the engine. They are pure predicates over the records: the invariant checks applied to each round, the PremiseComposition's expectations ([03](03_premise.md)) checked against each round's observed record, and the objective scoring of each Actor ([§9.2](#92-run-details)). Initialization is not asserted separately.

### 7.1 Required assertions

Each assertion checks one invariant from the invariant table ([02 §9](02_kernel.md#9-architectural-invariants-and-numerics)). The status column says what is checked per round today.

| Assertion | Invariant | Status |
|---|---|---|
| Unified action semantics | [INV-01](02_kernel.md#inv-01) | Implemented (settlement events against Intents and declared flows) |
| Authority separation | [INV-02](02_kernel.md#inv-02) | Implemented |
| No authority amplification | [INV-03](02_kernel.md#inv-03) | Implemented |
| Resource consistency | [INV-04](02_kernel.md#inv-04) | Implemented |
| Stock-flow reconciliation | [INV-05](02_kernel.md#inv-05) | Implemented |
| Exchange solvency | [INV-06](02_kernel.md#inv-06) | Implemented |
| Price is not production | [INV-07](02_kernel.md#inv-07) | Implemented (no settled Flow an Exchange curve or a plain Transfer settles crosses the economy's boundary) |
| Account partition | [INV-08](02_kernel.md#inv-08) | Implemented per round for Resources drawn from their holder |
| Representation consistency | [INV-09](02_kernel.md#inv-09) | Implemented for the estimator bound |
| Consent completeness | [INV-10](02_kernel.md#inv-10) | Implemented |
| Order independence | [INV-11](02_kernel.md#inv-11) | Implemented as `orderIndependent`, which replays the round in other orders. Run by §14 test 3, not per round in play |
| Deterministic replay | [INV-13](02_kernel.md#inv-13) | Tested by §14 test 2 |
| Resolution consistency | [INV-14](02_kernel.md#inv-14) | Tested by §14 test 12 |
| Obligation consumption | [INV-37](02_kernel.md#inv-37) | Implemented (no due obligation stays pending; `checkAfter` also reports more boundary settlements than obligations due) |

Simulator invariants [INV-29](02_kernel.md#inv-29) and [INV-30](02_kernel.md#inv-30) are checked by the handler suite, and [INV-31](02_kernel.md#inv-31) by §14 test 13. Conservation checks read each Resource's declared properties and the Transformations that produce or consume it ([INV-04](02_kernel.md#inv-04), [INV-05](02_kernel.md#inv-05)). They hard-code no Resource, because a Resource can be produced, consumed, reserved, or issued by design, so a blanket "totals are constant" check would be wrong.

A failed assertion records its invariant ID and message with its round. The run list reports the number of failing assertions. The run page shows each failed assertion with its expected and observed values, when it compares two, and the round's events that concern it.

### 7.2 Metamorphic tests

These tests transform a configuration in a controlled way. Those marked with a test number are implemented.

- Reorder submissions ([INV-11](02_kernel.md#inv-11)), replay with the same seeds ([INV-13](02_kernel.md#inv-13)), withhold one signature ([INV-10](02_kernel.md#inv-10)), and change explicitness (§7.3): tests 3, 2 and 13, 10, and 12.
- Change one seat Strategy or one Exchange parameter: the `variant` and `counterfactual` derivations (§10.3).
- Remove all representative observations: Aggregates follow their baselines ([INV-09](02_kernel.md#inv-09)). An unnumbered test in §14.

### 7.3 Resolution-consistency test

This is the direct check of [INV-14](02_kernel.md#inv-14). It instantiates the reference PremiseComposition twice, once with a government's activity implicit and once with that government explicit ([06 §10.1](06_meridian.md#101-germany-and-the-eu), [06 §14.2](06_meridian.md#142-resolution-consistency)). At initialization the two instances must reconcile stocks, give every capability exactly one holder, and cover each behavioural responsibility exactly once; any difference is an accounting defect. The explicit instance is not played further, because an implicit government spends its members' flows internally, which no seat Strategy can reproduce action for action.

### 7.4 AI interpretation tests

Text-to-Intent interpretation is tested apart from the simulator, which consumes canonical Intents and never calls a language model. The interpretation suites cover prompts, reply decoding, evidence, and the process boundary, using a stub backend or a replayed recording, with no model call. They check text inputs against their expected canonical Intents, cross-player cases (a joint candidate Transaction described by both parties reconciles, and one described by a single party lapses), and inadmissible claims, which the engine rejects as a `Rejected` event ([04 §11.2](04_run.md#112-live-session)). Accepted canonical Intents are the replay boundary.

---

# Part III: Run Store

## 8. Persistence

The Run Store is the persistent store of runs. Every run is persisted and survives restarts. Round writes, lifecycle writes, and reads go through separate effect operations ([02 §8.4](02_kernel.md#84-effects-and-handlers)), and the server reaches them only through the operations in §8.1.

| Field | Contents |
|---|---|
| **Run ID, name, status** | Identity and lifecycle state, with `created`, `reason` (for a failed run), and the derivation link (`source`, with its `kind` in the spec). |
| **Manifest** | The Run manifest ([04 §9](04_run.md#9-runtime-state-and-lifecycle)), stored with the run. The preset, player seed, round count, and overrides are in the spec ([04 §9](04_run.md#9-runtime-state-and-lifecycle)). |
| **Round records** | Append-only candidate Transactions, signatures, lapsed candidate Transactions, proposed and executed actions, procedure candidate Transactions with their support and outcome, stored Representation estimates, and events. |
| **Round-end Worlds** | One per round, in the `snapshot` column. The initial World is in the manifest. A round-end World holds the World, including `pending` and `templates` ([02 §3.4](02_kernel.md#34-successors)). |
| **Assertions** | Failures per round, with their invariant IDs. |
| **Metrics** | Derived projections (for example `price:<exchange>` after each round), tagged with the version of the metric definitions (`v1`). |

**Source of truth ([INV-31](02_kernel.md#inv-31)).** The manifest, round records, and round-end Worlds are authoritative. Metrics are reproducible projections of them: when a metric definition changes, its values are recomputed and the records stay untouched. Round-end Worlds support fast inspection, and records explain outcomes. Neither replaces the other.

**Mutability.** Completed runs are immutable, and a changed configuration always gets a new run ID. Round records, round-end Worlds, and metrics are never edited. A run's status, failure reason, archive flag, and spec change only through the operations in §8.1. Resume is the only one that adds rounds, and it keeps every existing round record. Delete removes a run with its rounds and metrics.

**Evidence.** A live session also keeps evidence of each language-model call: the effect, the model ID, template and input hashes, a pointer to the raw output, and the decoded result. It lives beside the run, outside the round records. Replay never re-invokes the model; the canonical candidate Transactions and signatures in the round records are enough.

**Writes.** The initial record is written before Round 1. After each round, the round record, round-end World, assertions, and metrics are saved in one transaction. If Round 4 fails, Rounds 1 to 3 remain available.

**Rejection reasons.** A `RejectReason` is stored as a structured value, so a stored event reads back equal to the event that was recorded. Its display text is derived from the value when shown and is not stored.

**Schema.** The store is SQLite. Its schema version is the database's user version. There is one schema, version 9 (`schemaVersionCurrent`), and there are no migrations: a change to the schema starts a new store. Version 2 removed `deliveries` and `scheduled` from round-end Worlds, which `pending` replaces, version 3 added `laws`, version 4 renamed the stored kernel Law changes, version 5 renamed stored keys to the current terms, version 8 removed two unused fields from stored `pending`, and version 9 added the `Enter` Intent and the `OfAgreement` cause. The one exception is additive: `run_sets` (`name`, `baseline`, `runs` as a JSON array of run IDs) is created wherever it is absent, and the store keeps its version.

| Table | Columns | Key |
|---|---|---|
| `runs` | `id`, `name`, `status`, `spec`, `manifest`, `created`, `reason`, `archived` (default 0; 1 hides the run from the default list), `source` (the run it was derived from), `owner` (role, process id, start time) | `id` |
| `rounds` | `run`, `round`, `record`, `snapshot`, `assertions`, `transcripts` (JSON, the round's seat transcripts) | `run`, `round` |
| `metrics` | `run`, `metric`, `version`, `round`, `value` | `run`, `metric`, `version`, `round` |

**Access.** Reads and writes go only through the store interface. A store at another schema version is refused, a value that does not decode fails only its own field, and metric rows carry their definition's version so projections are never mixed.

**Replay.** Replay requires a record for every round from 1 to n. A missing round is an error that names it, never a silent replay with no seat inputs.

**Identity.** The manifest records the draw version, since a stored seed is not enough without the versioned random generator. Run IDs never influence sampling, and a new run's ID is never one that a stored run still cites as its source.

### 8.1 Read API and run operations

The server serves JSON through the `API` operations. Each operation below is one route of the endpoint table, which a browser client shares. Any other method or path under `/api/` is a 404, and a body over 1 MiB is a 413. Replies are 200 unless stated. Every operation is private by default and needs the admin session, unless its row says Open (§8.3). The server loads every PremiseComposition under the PremiseComposition root; an operation on a run uses the run's own PremiseComposition, and a run whose PremiseComposition is not loaded is refused with the reason.

| Operation | Purpose | Replies |
|---|---|---|
| `GET /api/runs/:id/balancesheets` | Every Actor's name, kind, and balance sheet path, for the admin page. | 200. 404 for an unknown run |
| `GET /api/runs/:id/balancesheet/:token` | Open. One Actor's balance sheet page ([§9.5](#95-balancesheet-pages)). | 200. 404 for an unknown run or token |
| `GET /api/runs/:id/rounds/:round/report` | Open. What one round produced (§8.2). | 200. 404 for an unknown run or a round not played |
| `POST /api/sessions`, `GET /api/sessions/:id`, `POST /api/sessions/:id/transcripts` | Start a hosted live session (admin); read its state and submit a seat's text (open). See §8.2. | See §8.2 |
| `GET /api/openapi/:service` | Open. The OpenAPI document of `transcription.json` or `narrative.json` (§8.2). | 200. 404 for any other name |
| `GET /api/premise` | The launch form's PremiseCompositions: name, version, its Premises with their versions, round range, default rounds, game seed, `defaultName` (the next run ID), and the seat presets in order. | 200 |
| `GET /api/runs` | The run list, one row per run, with its id, archived, readable, and source flags. Archived runs are left out unless `archived=true`. | 200 |
| `POST /api/runs` | Launch a run of one preset on every seat: `composition`, name, preset, player seed (null draws), rounds (null for the default). | 200 with the new id and seed used. 400 for a bad or refused launch, including rounds outside the range |
| `POST /api/batch` | One run of one preset on each named PremiseComposition (every loaded one by default), so they can be read through Compare. Each run records its own PremiseComposition. The batch gives no score of how well a PremiseComposition plays. | 200 with a list of runs. 400 for a bad body or a PremiseComposition that cannot start |
| `GET /api/runs/:id/:view` | One of 17 views: `overview`, `objectives`, `balances`, `exchanges`, `transformations`, `economy`, `representation`, `procedures`, `assertions`, `wholeeconomy`, `events`, `standings`, `networth`, `actions`, `drivers`, `balance`, `dependence`. `round` applies to `balances` (default: the final round) and `events` (default 1). | 200. 400 for a bad round. 404 for an unknown run or view. 409 when the view needs a PremiseComposition that is not loaded. 500 for an unreadable run |
| `GET /api/runs/:id/actors` | The run's Actors, each marked as a seat or not, and the default Actor. | 200. 404. 500 |
| `GET /api/runs/:id/actor/:actor` | One Actor's balance sheet after every round, with a chart and its net worth. | 200. 400 for an Actor the run does not have, listing the valid names. 404. 500 |
| `POST /api/runs/:id/derive` | Derive a run of kind `duplicate`, `rerun`, `variant`, or `counterfactual`, with optional name, preset, player seed, rounds, and overrides. | 201 with the new id, source, and kind (plus `recordsMatch` for a rerun). 400 for a bad kind, field, preset, or override. 404 for an unknown source. 409 when the source cannot be compared or replayed. 500 |
| `POST /api/runs/:id/resume` | Extend a completed run to a total round count. | 200 with id and rounds. 400 when the run is not completed, is a live session, or the count is not above its completed rounds. 404. 500 |
| `POST /api/runs/:id/step` | Play one round of a completed run short of its target. | 200 with id and rounds. 400 when the run is not completed, is a live session, or has played all its rounds. 404 for an unknown id. |
| `POST /api/runs/:id/archive`, `…/unarchive` | Set or clear the archive flag. Idempotent. | 200 with id and archived. 404 |
| `DELETE /api/runs/:id?confirm=:id` | Delete the run with its rounds and metrics. `confirm` must name the run. | 200. 400 without a matching `confirm`. 404 |
| `GET /api/compare?runs=a,b[&exchange=m]` | Compare a Run set. The first valid run is the baseline. Each table marks the cells that differ from the baseline. | 200. 400 when no run is valid or `exchange` is unknown |
| `GET /api/glossary` | The definitions and reasons the dashboard shows, generated from the columns the views use. Served whole, so the dashboard holds no glossary text. | 200 |
| `GET /api/agency` | Agency analysis of a PremiseComposition (§10.4): every seat plays `preset`, then each Playable Actor in turn plays `alternative`. | 200. 400 for an unknown PremiseComposition, a bad seed, or an unknown preset |
| `GET /api/agency/detail` | The same analysis, beyond its table: each Playable Actor's objective criteria under the baseline and the alternative. | 200 with the view model. 400 for a bad query. |
| `POST /api/experiments` | Run a Variant Experiment (§11) and save its runs as a named set. | 200 with the set and its run IDs. 400 for a name in use, an empty experiment, a bad round count, or a game that does not start. |
| `GET /api/sets` | The named Run sets. | 200 with the sets. |
| `GET /api/sets/:name/summary` | The final measures across a set's runs (§11). | 200 with the table. 404 for an unknown set. 500 when one of its runs cannot be read. |
| `POST /api/sets` | Save a set: `name`, `baseline`, `runs`. | 200 with the set. 400 for a blank name, no runs, a baseline outside the runs, or an unknown run. 409 for a name in use. |
| `POST /api/sets/:name/rename` | Rename a set: `name`. | 200 with the new name. 400 for a blank name. 404 for an unknown set. 409 for a name in use. |
| `DELETE /api/sets/:name` | Delete a set. Its runs are untouched. | 200 with the name. 404 for an unknown set. |

Operation rules:

- **Derive.** A derived Run starts from a stored one. `duplicate` and `rerun` take no spec fields beyond the name. A `variant` replaces the preset, the player seed, or the rounds, and takes no overrides. A `counterfactual` requires at least one override, which it adds to the source's, and takes no preset, seed, or rounds. The default name is `<source name> (<kind>)`. Every derived run stores `source` and `kind` in its spec, and `source` in its own column.
- **Rerun.** `recordsMatch` is `true` when the rerun's round records equal the source's. It is `false` for nondeterminism, a stored-versus-live mismatch, or an unrecorded seed, and that is a finding about reproducibility, not a reply error. A source that is not completed, or has no stored records, is refused with 409 and nothing is created.
- **Sessions.** A live session stores no preset, so `variant` and `counterfactual` from one are refused, and so is `resume`. `duplicate` and `rerun` replay it without a model call ([04 §11.1](04_run.md#111-round-record)).
- **Resume.** Starts from the round-end World after the run's last completed round and updates the stored round count. The run keeps its ID.
- **Deleted sources.** A derived run whose source was deleted keeps its `source`, which then shows as deleted.

Dashboard controls exist for launch, resume, archive, delete, and the four derivations (duplicate, rerun, variant, and counterfactual). The store interface supports save, retrieve, list, full round histories, resume, and archive or delete.

### 8.2 External services

Speech transcription and the newspaper are built outside this repository and reach it over open HTTPS routes. This repository does not transcribe speech and does not write a front page. Reading a round's transcripts into Intents, the joint pass, stays here ([04 §11.2](04_run.md#112-live-session)).

Their contract is two OpenAPI 3.1 documents, the source of truth for those routes: `GET /api/openapi/transcription.json` (session status, submitting a seat's text) and `GET /api/openapi/narrative.json` (a round's report). `just _premises` writes the same documents to `openapi/`. Each document takes its routes, summaries, paths, methods, parameters and request bodies from the route table, and its reply schemas, success statuses and refusals from the API's contract (`Handlers.Api.Contract`), which the routes answer through, so a route cannot be documented differently from how it behaves. A route's refusals are the labels of its entry's row; what reading its request can refuse comes from the table. `just openapi <premise>` writes the same documents for one PremiseComposition, where a reference to a kind the PremiseComposition names is an `enum` of its names.

**How to authenticate.** There is no authentication for these routes. They are open: anyone with the URL can read every Actor's report and submit any seat's text. Every other `/api` route needs the admin session cookie (§8.3), including `POST /api/sessions`.

| Route | Access | Request | Replies |
|---|---|---|---|
| `POST /api/sessions` | Admin | `humans`; optional `composition`, `rounds`, `window` (seconds, 0 for none), `playerSeed` | 200 with the Run id and each Actor's balance sheet path. 400 for an unknown PremiseComposition or seat, or rounds outside the range. 401 without the admin session |
| `GET /api/sessions/:id` | Open | | 200 with the session's state. 404 for no such session |
| `POST /api/sessions/:id/transcripts` | Open | `seat`, `round`, `text` | 200 accepted, with the session's state. 400 for an unknown seat or a body that does not decode. 404 for no such session. 409 for the wrong round, a seat that has already spoken this round, or a session that is over |
| `GET /api/runs/:id/rounds/:round/report` | Open | | 200 with the round report. 404 for an unknown run or a round it has not played |
| `GET /api/openapi/transcription.json`, `GET /api/openapi/narrative.json` | Open | | 200 with the document |

An unknown seat is one no person plays in this session: AI agent seats speak for themselves. A refusal's body is `{"error": "..."}`.

The reply to `POST /api/sessions`, for the body `{"humans":["US"],"rounds":4,"playerSeed":1}`:

```json
{"id":"run-1","balanceSheets":[{"actor":"US","kind":"Playable Actor","path":"/balancesheet/run-1/abc123"}]}
```

The session status reply, which is also the reply to an accepted submission:

```json
{"window":0,"submitted":[],"status":"running","round":1,"awaiting":["US"]}
```

A submission's body:

```json
{"seat":"US","round":1,"text":"We sell ten Energy to Germany."}
```

**The order of calls in a round.**

1. A facilitator starts the session with `POST /api/sessions`, under the admin session. The reply's `id` is the session id and the Run id.
2. The speech component reads `GET /api/sessions/:id` for the open `round` and the seats still `awaiting`.
3. It posts each seat's text once, with that round number. The round closes when every human seat has submitted or its deadline (`window`) passes, and a seat that has not spoken by then says nothing that round.
4. The joint pass reads the round's transcripts and the round resolves ([04 §11](04_run.md#11-round-execution)).
5. The newspaper reads `GET /api/runs/:id/rounds/:round/report` once the session reports a later `round`, or a `status` of `completed` for the last round. The report is a 404 until the round has been played.
6. A `status` of `failed` ends the session, and the rounds already played stay readable.

Sessions live in the server's memory. After a restart the session routes reply 404 and the Run is marked failed ([§6.1](#61-invalid-actions-and-failures)), while its saved rounds remain.

**What is stable.** The routes, their methods, the field names of their requests and replies, and the status codes above are the contract. In a report, `prices`, `balances`, `shortfalls`, `enacted`, `repealed`, `rejected` and `lapsed` are summaries meant for a downstream writer, with numbers as display strings ([02 §9.1](02_kernel.md#91-numerics-and-randomness)). `events` carries the engine's events in their stored form and changes with the store's schema version. Ids and balance sheet paths are opaque text. The wording of an `error` is not part of the contract.

### 8.3 Public and admin surfaces

Everything that shows more than one Actor's data sits behind a password.

| Surface | Access |
|---|---|
| `/` | Public. The landing page, a page of the dashboard app with its nav bar and themes, with no run data and a link to `/admin`. |
| `/balancesheet/:run/:token` | Public. One Actor's own page ([§9.5](#95-balancesheet-pages)). |
| `/admin` | The login page, and the dashboard once logged in. Its hashed assets under `/admin/assets` are public because the balance sheet page loads them, and they hold no data. |
| `/api` | Private by default ([§8.1](#81-read-api-and-run-operations)). |

**The API is private by default.** Each row of the route table is `Admin` unless the table says `Open`, so a new route is private until someone decides it is not. The open routes are the balance sheet route, the session status and transcript routes, the round report, and the OpenAPI documents. Every other route replies 401 without the admin session, and the gate checks this before a request's body is read. A test walks the whole route table to hold this.

**The password.** `ADMIN_PASSWORD` is read from a `.env` file, whose path is the server's `--env` flag (default `.env`). The file is not committed; `.env.example` names the variable with no value. A missing file or an empty password refuses every admin request. A correct login at `POST /admin/login` sets the session cookie: `HttpOnly`, `SameSite=Lax`, `Secure` when the request came over HTTPS. Its value is derived from the password, so a restart logs nobody out.

---

# Part IV: Dashboard

## 9. Pages

The Dashboard is served at `/admin`, behind the password (§8.3). It has four pages, each with its own hash route and a top bar that links to them from every page: Runs (`#/runs`, the home page, and `#/runs/:id` with that run selected), Prepare (`#/prepare`), Compare (`#/compare`, and `#/compare/:ids` with the ticked runs), and BalanceSheets (`#/balancesheets`, [§9.5](#95-balancesheet-pages)). It renders from persisted data and does not run the engine. Every number shows its display kind (quantity, price, or ratio) as [02 §9.1](02_kernel.md#91-numerics-and-randomness) defines it. Every visible column and field has a plain-language definition on hover, from the glossary (§8.1).

### 9.1 Simulation Runs

The run list is a sortable table, one row per stored run, in creation order. It shows the run's name, PremiseComposition, seeds, completed rounds, status, failing assertions, reason for a failure, archive flag, and source. A run that cannot be decoded shows status `unreadable` and its reason, and the other runs stay visible. A Show archived runs toggle adds archived runs. Selecting a row, or opening `#/runs/:id`, shows that run's results beside the list; with no selection, the latest readable run is shown. Resume appears only for a completed run that has a preset, and never for a failed or interrupted run. Archive, Unarchive, and Delete (which requires the run's id to be confirmed) are under "Manage this run" on the run page.

### 9.2 Run Details

The page opens with a header of the run's facts. Its sections follow in order: key figures; marginal prices of each Exchange; standings; net worth over time; actions; drivers (rules enacted and repealed, procedure outcomes, shortfalls); balance measures (spread and Gini); objectives; dependence (for each fixed requirement: the capacity covering it, coverage = min(1, capacity / requirement), exposure = max(0, requirement − capacity), imports, exports, receipts, shortfall, the bans in force against it, and the capacity on order); the final balance sheet of every Actor (`Initial`, `Current`, `Change`); and one Actor's balance sheet, selected from the run's Actors, defaulting to the first seat.

Net worth is Money plus each Resource that an Exchange trades against Money, valued at its marginal price. Where several Exchanges trade one Resource, its price is their mean, so a holding counts once, and a Resource with no Exchange is not counted. Spread is the highest minus the lowest final net worth of the players. The Gini coefficient of final net worths is 0 when every player holds the same and 1 − 1/n when one holds everything, and it is n/a when the total is zero or a value is negative.

Standings rank 1 is the highest net worth, and equal net worths share a rank. Aggregates are residual representations, not players. An objective criterion is met, not met, or not evaluated; it is never reported as met when its series is empty.

The run page shows the representation and procedure records. The round-by-round action counts, the Exchange, Transformation, and economy views, and the raw event log are served by the API only. Representation records span two rounds: the observation, the estimate, and the Aggregate's purchase that applies the stored factor, from which the influence ratio is derived: the next-round Aggregate change attributable to a represented Actor, divided by that Actor's executed purchase. The event inspector lists one round's events in engine order, with a text filter.

### 9.3 Prepare

Prepare launches a run of a PremiseComposition. Its fields are a name (default: the next run id), the PremiseComposition (shown only when the server offers more than one; default: the first the server lists, which is the first of the compiled order, `compositions/order.json`, written from `Premises.Catalog`: Pacific), a preset (the Strategy every seat plays; default: the PremiseComposition's own default preset, which `GET /api/premise` reports as `defaultPreset`, and choosing another PremiseComposition returns it to that one's default), the rounds (default: the PremiseComposition's default, within its range), and a player seed (blank draws one at launch, and a whole number fixes it). The game seed is shown read-only, because the server takes no game seed at launch. A summary line states what will be played. An accepted launch opens the new run; a refused one shows the server's message.

### 9.4 Compare

The Compare page lists every readable run as a tick box. The ticked runs, in the order ticked, form the Run set, and the first is the baseline. The page shows prices by Exchange, net worth over time, final standings, the spread and Gini, a key-values table, and detail tabs (final state, baseline difference, configuration, compatibility, seed variation, and agency, §10.5). Unreadable runs are listed as skipped, with their reasons. Every statistic is computed in the views, not in the browser.

### 9.5 BalanceSheet pages

At the start of a game each Actor, Playable, non-playable or Aggregate, gets a URL showing its own balance sheet: `/balancesheet/:run/:token`, read through `GET /api/runs/:id/balancesheet/:token`. `POST /api/sessions` and the CLI's `session` command report each Actor's path at the start, and the admin BalanceSheets page lists every path for a selected Run, through the admin-only `GET /api/runs/:id/balancesheets`. An unknown token is a 404. The Run id is in the path because a player seed can repeat across Runs.

The page has three parts, each from `Views.BalanceSheet`:

- **What it has done.** Every action by round: what it proposed and signed, what settled, what was rejected or lapsed and why, and the Transactions it was party to.
- **What it has available.** Every Resource held now and by round, net worth by round, and what it owes or is owed in `pending`.
- **What it can do.** Its Capabilities, the Exchanges and Transformations they open with their bounds and current prices, and its objective with progress where it has one.

The page shows only that Actor's data and has no navigation to `/admin`. Another Actor appears only as a counterparty on a Transaction this Actor was party to.

**The URL is not a security boundary.** The token (`Core.Draw.tokenFor`) is the FNV hash of the Actor id XORed with the player seed, as 16 hex digits. It is unguessable in casual use only: anyone who knows the seed and an Actor id can compute it, and the page needs no login. This is a game, and nothing on the page is secret from the facilitator.

## 10. Comparison

Comparison supports both correctness testing and balancing. The baseline is the first ticked run.

### 10.1 Selection and compatibility

The Dashboard names each difference between runs that are not directly comparable (PremiseComposition or engine versions, round counts, units, Exchange definitions, seat presets). Incompatible configurations and metrics are flagged, never combined. Trajectories align by simulated time, without interpolation.

### 10.2 Views

- **Trajectory:** one metric across runs, such as each Exchange's price or each player's net worth.
- **Final state:** ending holdings side by side, with differing values highlighted.
- **Difference from baseline:** the holdings that differ from the baseline, as changes.
- **Key values:** besides net worth, rank, actions, the balance measures, objectives and prices, each Actor's receipts from its Exchange sales over the run, the final coverage of each fixed requirement by the Actor's own capacity, and each requirement's shortfall over the run. Between a run and its counterfactual without a restriction, the shortfall row is the restriction's realized effect.
- **Configuration difference:** parameter differences, one row each, categorised as version, seat Strategy, seed, or other. The other category holds deliberate changes such as overrides, so they are distinguished from the rest.

### 10.3 Counterfactual experiments

A rerun is the zero-change case: it reproduces the source's configuration and both seeds, so `recordsMatch: true` confirms that the source is reproducible (§8.1).

A counterfactual is the intervention case. It duplicates a run's resolved configuration and both seeds, changes exactly one thing by `overrides` (such as one seat's signature or one deployment), and stores the result as an ordinary immutable run linked to its source. The Compare page then sets the original as the baseline. A `variant` replaces the preset on every seat instead. Changing one seat's Strategy alone is a pure analysis that stores no run.

With both seeds and every draw key fixed, the difference is the intervention's total modelled effect, including downstream consequences.

### 10.4 Agency analysis

For each Playable Actor, the simulator replays the Run with the alternative preset for that Actor and everything else fixed. It measures the change in prices, in other Actors' holdings, in Aggregate behaviour, in institutional decisions, and in the Actor's own objective criteria ([06 §9.3](06_meridian.md#93-objectives)). An unknown preset name is an error, never a silent baseline.

The Compare page's Agency (fixed baseline) tab shows one row per Playable Actor under the alternative. A row whose alternative makes negligible difference on every measure is marked `no: balancing finding`. Under it, each Playable Actor's objective criteria are shown under the baseline and the alternative (`GET /api/agency/detail`). The agency profile per role gives one row per Playable Actor: its measures under the alternative, and its criteria met either way.

### 10.5 Saved comparisons

The comparison's runs and baseline are carried in the route. A named Run set stores a name, the IDs of immutable runs, and the baseline, and never copies run data. Deleting a run a set names is refused with a 409 naming the set. The Compare page opens, saves, renames, and deletes sets.

---

# Part V: Experimentation

## 11. Experiments

A Variant Experiment generates one specification for each declared parameter value, runs and saves every resulting Run, and opens the runs as a named Run set (`POST /api/experiments`, and `experiment <premise>` with `--parameter name --values a,b`, `--combine a=1,2;b=3,4`, `--seat id --alternatives A,B`, `--player-seeds 1,2`, or `--game-seeds 1,2` in the CLI). Every run uses one player seed, drawn once if none is given. Analysis functions also compose the simulation directly, scoring results with the checks and presenting them through the views, and `POST /api/batch` plays one run per PremiseComposition. A Variant Experiment's kinds are single-parameter (one parameter over a list of values), multi-parameter (every combination of the values given), seat-Strategy (parameters fixed, one deviating seat among otherwise identical seats), and player-seed and game-seed batches (§5). A set's batch summary gives the final measures across its runs: the mean, the spread, and how many runs met each explicit Actor's objective (`GET /api/sets/:name/summary`, and a control on the Compare page). The Seed variation tab separates what depends on the seed from what depends on the specification.

**Whole-economy measures.** The `wholeeconomy` view gives these by round and Exchange, and the run page shows them:

- Demand on Exchange m in round r: the sum of every positive Trade quantity submitted on m, whether or not it filled.
- **Unfilled demand**: max(0, Demand − bought), where bought is the base quantity players bought on m that round.
- **Concentration**: the Herfindahl index of the players' positive net worth after the round, the sum of each one's share squared. It is 1/n when n players hold equal net worth and 1 when one holds all.
- **Money distribution**: the Gini coefficient of the players' holdings of the numeraire after the round. It is n/a when none is held or a holding is negative.

## 12. Correctness Versus Gameplay Quality

Correctness is established by the assertions (§7) and the mandatory tests (§14). The gameplay questions are in [01 Evaluation](01_overview.md#6-evaluation), and playtesting answers them. The Dashboard computes action counts, the balance measures (spread and Gini), objective criteria, and the whole-economy measures (§11).

---

# Part VI: Reference Test Suite

## 13. Reference Coverage

The suite runs the reference PremiseComposition, *Meridian* ([06](06_meridian.md)), with every seat bound to a simple, predictable preset.

## 14. Mandatory Tests

Each test passes when its cited fixture's expected values hold and its listed invariants pass. The suites share their fixtures and the capstone run through `testkit`.

| # | Name | What it checks |
|---|---|---|
| 1 | Full game | Five rounds under three presets (every seat `Conservative`, `Passive`, `RandomValid`), with the game seed fixed and `RandomValid` under two player seeds. Each round is valid, the implemented invariants of §7.1 hold, the same seeds reproduce, and different player seeds give different histories ([INV-13](02_kernel.md#inv-13)). |
| 2 | Replay | `RandomValid` run twice with the same seeds, then once under the replay handler from the recorded input. Round records and round-end Worlds are identical ([INV-13](02_kernel.md#inv-13)). |
| 3 | Submission order | For the `Conservative`, `Aggressive`, and `RandomValid` runs, each round's recorded inputs in three reorderings. Each reordering settles to the recorded world ([INV-11](02_kernel.md#inv-11)). |
| 4 | Direct Exchange | [Fixture A](06_meridian.md#21-fixture-a-direct-exchange) and its limit-price variant A2. Fixture values; [INV-04](02_kernel.md#inv-04), [INV-06](02_kernel.md#inv-06). |
| 5 | Germany and EU | [Fixture B](06_meridian.md#22-fixture-b-germany-and-the-eu-collective-procedure) as a pair: Germany signs against not signing. The procedure executes in the same round only when support exceeds the threshold ([INV-10](02_kernel.md#inv-10)); signing moves no resources ([INV-03](02_kernel.md#inv-03)). |
| 6 | SME Representation | [Fixture C](06_meridian.md#23-fixture-c-small-business-owner-representation), C1 to C3, and C1 with shrinkage 0. The factor applies in the following round, and shrinkage 0 leaves it at 1. Seeded-noise cases are checked in the premises suite ([INV-09](02_kernel.md#inv-09), [INV-13](02_kernel.md#inv-13)). |
| 7 | Non-playable Actor response | [Fixture D](06_meridian.md#24-fixture-d-china-responds-as-a-non-playable-actor) as a pair, with and without the deployment. |
| 7b | Reservation | [Fixture R](06_meridian.md#133-military-deployment) and R-alone: a deployment reserves a Resource, a sale of the same Resource is scaled, and the reservation is released at commit ([INV-04](02_kernel.md#inv-04), [INV-05](02_kernel.md#inv-05)). |
| 7c | Deployer | The shipped `Deployer` preset with the other seats Passive. The scaled deployment and sale occur in the rounds the reference price is above its threshold, with the exact values of [06 §13.3](06_meridian.md#133-military-deployment). |
| 7d | Investor | Five seats play `Investor`. The capacity investment executes, and no Rejected event says the candidate Transaction "must sign". |
| 8 | Scarcity | [Fixture E](06_meridian.md#25-fixture-e-scarcity-and-trade-restriction): the round 3 disruption and regional prices. The expected propagation holds ([INV-05](02_kernel.md#inv-05), [INV-06](02_kernel.md#inv-06)). |
| 8b | Major shock | Every seat plays each preset, calm and with the [major shock](06_meridian.md#136-energy-disruption), under the same seats and player seed. At least four presets flip their proposed kinds of action, deterministically ([INV-13](02_kernel.md#inv-13)). A second check pins the calm and shocked Conservative prices ([06 §13.6](06_meridian.md#136-energy-disruption)). |
| 9 | Conflicting actions | [Fixture F](06_meridian.md#26-fixture-f-simultaneous-conflicting-orders). Fixture fills ([INV-11](02_kernel.md#inv-11)). |
| 10 | Signed candidate Transactions | [Fixture G](06_meridian.md#27-fixture-g-candidate-transactions-and-signatures) cases G1, G2, G3, G3-ban, G5, and G5-unsigned. Fully signed candidate Transactions settle atomically with the fixture values, unsigned ones lapse while the round resolves, and a joint candidate Transaction under an `AccessBan` is rejected as a unit ([INV-10](02_kernel.md#inv-10)). |
| 11 (a) | Trade restriction, capstone | The DSL-authored US ban, EU tariff, and a banned shipment in the capstone run. The procedure executes, the tariff is in force, and a `Blocked` rejection is recorded. |
| 11 (b) | Trade restriction, Fixture E2 | [Fixture E2](06_meridian.md#25-fixture-e-scarcity-and-trade-restriction): the US enacts an `AccessBan` on China in round 2, in force from round 3, and China reciprocates, effective round 4. The fixture's values hold. |
| 12 | Resolution consistency | [§7.3](#73-resolution-consistency-test): the reference PremiseComposition instantiated with an implicit and an explicit government ([INV-14](02_kernel.md#inv-14)). |
| 13 | Persistence | Save a run to the Run Store, reload it, compare its stored records and round-end Worlds with the pure run, and replay from the stored records. The run is identical ([INV-31](02_kernel.md#inv-31)). |
| 14 | Agency | [§10.4](#104-agency-analysis) across every seat of the reference PremiseComposition. Under the `RandomValid` baseline, every Playable Actor has a measurable effect and its objective criteria are reported. Under the `Conservative` baseline, the Playable Actors with no measurable effect are pinned as balancing findings, so a PremiseComposition change is noticed. |
| 15 | Pacific golden scenarios | [05 §11](05_pacific.md#11-golden-scenarios) A to H: free trade, each ban, each investment, a ban met by substitution in both directions, no phantom trade on an Exchange with no reserves, and the invariants, replay, submission order, repeated enactment and the bans that cannot be bypassed ([INV-04](02_kernel.md#inv-04), [INV-05](02_kernel.md#inv-05), [INV-07](02_kernel.md#inv-07), [INV-11](02_kernel.md#inv-11), [INV-13](02_kernel.md#inv-13)). |
| 16 | Routine beside a player | [05 §11](05_pacific.md#11-golden-scenarios) Routine: a human seat composed with its routine, whose commerce reads what the human's speech put forward. |
| 17 | Sensitivity | [05 §12](05_pacific.md#12-sensitivity): the strategic tension holds over each dial changed alone, the private sector's included. |
| 18 | Pacific private sector | [05 §11.1](05_pacific.md#111-the-private-sector) I to O: rebalancing absorbs output at home, a cautious firm lowers its private sector's spending, directed investment leaves output unsold, the tariff, the US mirror, the representation pipeline, and replay and submission order over four seats ([INV-09](02_kernel.md#inv-09), [INV-11](02_kernel.md#inv-11), [INV-13](02_kernel.md#inv-13)). |

The premises suite also runs the authored agreements of [03 §15](03_premise.md#15-authoring-laws-and-agreements), which are not a mandatory test. The core suite covers only the kernel, and the rest of the former core tests are in `packages/laws/test`. Unnumbered tests in the same suites: removing every representative observation leaves each Aggregate's factors at their baseline with no estimate recorded; every preset plays 8 rounds with the invariants holding; a run resumed from its third round-end World matches the uninterrupted run; the INV-29 and INV-30 checks; and a player-seed batch keeps every game-stream draw identical while seat behaviour varies.

**Live interpretation check (one-off, billed, not in CI).** One real model call over fixed transcripts checks that the reply decodes and that a joint candidate Transaction reconciles. It needs an explicit opt-in, and no suite runs it.
