# Simulator and Dashboard Specification

**Reference PremiseComposition:** Pacific ([05](05_pacific.md))

---

## 1. Purpose

The simulator plays complete Runs automatically and deterministically. Developers use it to check implementation correctness and to improve gameplay through repeated experiments. It is a development component. It adds no economic model of its own: it runs the kernel, PremiseComposition, and Run machinery. Invariants are cited by ID ([02 §9](02_kernel.md#9-architectural-invariants-and-numerics)).

A developer configures a Run, binds a seat Strategy to every seat, plays it, saves it, inspects it, compares it with other runs, and varies parameters to study correctness, balance, and player agency.

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

The stored spec records how the run was made: the game id, the preset every seat plays (absent for a live session), the resolved player seed, the overrides, the round count, the cast ([04 §2](04_run.md#2-instantiation-configuration)), and its class (simulated or interactive). A live session also records its model and human seats. A simulated run records its suite cache key, and an interactive run its tag and metadata (§8.2). With the same engine version, the spec and the seeds reproduce the complete economic history.

**Simulated and interactive runs.** A simulated run is one entry of the static suite, played at build time. An interactive run is a live run (§8.2). Runs are never launched one by one from the dashboard.

**The static suite.** For each compiled PremiseComposition, at its configured rounds and game seed and under player seed 1, the suite (`Premises.Suite`) holds:

- one preset run per preset, with every configured seat cast to it;
- one agency run per seat that has an alternative preset in the configuration ([04 §2](04_run.md#2-instantiation-configuration)): the default preset's run with that seat alone cast to its alternative (§10.3).

`just suite [--db file]` plays the suite into the store (`suite` in §6.2), and `just serve` and the dashboard tests run it into the store they use.

**The cache key.** Each entry is stored under a digest of three things:

- the engine build: the compiled output of the modules that decide how a run is assembled and played (the kernel, laws, Strategy interpreter, World, assembly, the round runner and the simulation fold);
- the composition's compiled files, its presets apart;
- the entry's own text: its cast, the presets that cast uses, its rounds and both seeds.

An entry whose key is stored is reused, and one whose key is not is played and stored under it. A stored simulated run whose key the suite no longer has is deleted. A change to the engine or to a dial replays every run of the composition, a change to one preset replays only the runs that use it, and nothing changed plays nothing.

### 3.1 Configuration inheritance

A specification may reference a baseline and apply declarative overrides: the game seed, a parameter, an Actor's capacity holding, a scheme's shrinkage, or making an Actor implicit. A seat's preset is not an override: a stored run plays one preset on every seat. The layer rules for which changes an override may make are in [03](03_premise.md) and [04 §2](04_run.md#2-instantiation-configuration).

Each run stores its overrides in its spec and its initial World in its manifest, so later changes to a baseline never alter historical runs.

## 4. Seat Strategies

The engine accepts only Intents ([03 §10.3](03_premise.md#104-available-actions)). In simulation each seat is bound, at assembly, to a seat Strategy ([04 §3.1](04_run.md#31-selection-and-control)). Swapping a seat's Strategy never changes the Actor's definition and never turns a seat into a non-playable Actor ([INV-30](02_kernel.md#inv-30)).

The simulator runs two machine bindings: a preset strategy and a script of literal candidate Transactions and signatures per round. A replay answers a live session's seats from such a script. Agent and human seats are answered only in live play, where each is briefed with its Actor's description and objective (for Pacific, [05 §8](05_pacific.md#8-objectives-and-reporting)). Presets run on the shared decision-language interpreter that non-playable Actor and Aggregate Strategies use ([03 §8.1](03_premise.md#81-shared-interpreter), [INV-29](02_kernel.md#inv-29)). A seat whose Actor has a routine plays it after any of these bindings except a script ([04 §6](04_run.md#6-strategy-initialization)).

Every kind sees only its Actor's View ([02 §4](02_kernel.md#4-strategies)) and maps it to Intents. Every accepted Intent is recorded, so every run replays from its round records. Seat Strategies never mutate the World: each Intent passes ordinary validation and settlement.

### 4.1 Bindings and coverage

The simulator never invents default decisions. A seat meant to stay silent is bound explicitly to an empty script. Which bindings an assembled Run must have, and the errors for a missing or misplaced one, are in [04 §3.1](04_run.md#31-selection-and-control). A seat Strategy that produces nothing submits nothing for the round, and nothing is recorded for it.

### 4.2 Presets

A preset is a StrategyRules a PremiseComposition names, run on the shared interpreter, not a separate strategy. Each PremiseComposition authors and compiles its own presets, and may name one its default (`InstantiationConfig.preset`, [04 §2](04_run.md#2-instantiation-configuration)). Pacific's presets are in [05 §7.3](05_pacific.md#73-presets).

`GET /api/premise` lists, for each PremiseComposition, its preset names in name order and its default preset. Where a caller names no preset, the composition's default is played, else the first it offers (`Assembly.Composition.presetOr`). The launch form offers the same names, and choosing another composition keeps the chosen preset only if that composition offers it.

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
- a live run stops early: a language-model backend cannot start, exits non-zero, or its input ends at a prompt (the reason names the cause). A backend failure is fatal: the round in progress is not completed, and the rounds already saved remain.

**Interruption.** A run records the process that created it (`owner`: its role, process id, and start time). When a server starts, runs still marked `running` are marked `cleared` (the live run it held, §8.2) with a reason saying the server stopped, if a server created them, if they have no owner, or if their creating process has gone. A run owned by another live process, such as a CLI session on the same store, is left alone.

### 6.2 Commands

| Command | Behaviour |
|---|---|
| `run <composition> [--preset P] [--rounds n] [--seed n]` (`just sim`) | Plays every seat with preset `P` (default: the composition's own default) for `n` rounds. `n` is a whole number of at least 1, and defaults to the PremiseComposition's configured count; `checkRounds` refuses a count outside the PremiseComposition's round range. Prints a header, one line per round, and the invariant results. Does not save to the Run Store. |
| `session <premise> [--humans S,…] [--model id] [--rounds n] [--window s] [--id name] [--db file] [--seed n]` (`just session`) | Live facilitator console: a mock live run through the same host and lifecycle as the server's (§8.2). A human seat's text is typed as `SEAT: text`, and a line starting with `/` is a control: `/advance`, `/pause`, `/resume`, `/end` or `/clear`. Seats named in `--humans` are human; the rest are AI-agent seats. `--window` is whole seconds, where 0 gives no propose window. The id is 1 to 64 letters, digits, `-` or `_`, starting with a letter or digit. Defaults: window 0, id `session-1`, database `runs.db`, and the configured model. Prints each Actor's balance sheet path at the start. Saves the session as a run; raw model output and call evidence are kept beside it (§8). |
| `suite [--db file]` (`just suite`) | Plays the static suite into the store (§3): every entry whose cache key is not stored, then deletes the simulated runs that have left the suite, and prints how many were played, reused and dropped. |
| `presets <composition>` | Prints the composition's preset names, one per line. |
| `server [--db file] [--port n] [--static dir] [--env file]` (`just serve`) | Serves the API, the landing page, and the dashboard at `/admin` (§8.3). Defaults: `runs.db`, port 8787 (1 to 65535), the dashboard build directory, and `.env`. |

An unknown, repeated, or valueless flag, or a bad value, is a usage error and exits 1. The composition argument is the PremiseComposition's directory name (`pacific`), and the CLI loads its version `v0`; `presets <composition>` prints its preset names. The dashboard's dev proxy and its end-to-end server use separate ports, 8787 and 8799.

## 7. Run Assertions

Assertions check engine properties after every round, without reimplementing the engine. They are pure predicates over the records: the invariant checks applied to each round, the PremiseComposition's expectations ([03](03_premise.md)) checked against each round's observed record, and the objective scoring of each Actor ([§9.3](#93-run-details)). Initialization is not asserted separately.

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

- Reorder submissions ([INV-11](02_kernel.md#inv-11)) and replay with the same seeds ([INV-13](02_kernel.md#inv-13)): tests 3 and 2. Withholding one signature ([INV-10](02_kernel.md#inv-10)) and changing explicitness (§7.3) have no Pacific case.
- Change one seat Strategy or one Exchange parameter: the agency runs of the suite (§10.3).
- Remove all representative observations: Aggregates follow their baselines ([INV-09](02_kernel.md#inv-09)). An unnumbered test in §14.

### 7.3 Resolution-consistency test

This is the direct check of [INV-14](02_kernel.md#inv-14). It instantiates a PremiseComposition twice, once with an Actor's activity implicit, covered by its Aggregate, and once with that Actor explicit. Every seat of Pacific is required, so it has no such pair, and the suite has no case of this check until a composition with an optional seat is added. At initialization the two instances must reconcile stocks, give every capability exactly one holder, and cover each behavioural responsibility exactly once; any difference is an accounting defect. The explicit instance is not played further, because an implicit government spends its members' flows internally, which no seat Strategy can reproduce action for action.

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

**Mutability.** Completed runs are immutable, and a changed configuration always gets a new run ID. Round records, round-end Worlds, and metrics are never edited. A run's status and failure reason change only as it plays. The suite deletes a simulated run that has left the suite, with its rounds and metrics.

**Evidence.** A live session also keeps evidence of each language-model call: the effect, the model ID, template and input hashes, a pointer to the raw output, and the decoded result. It lives beside the run, outside the round records. Replay never re-invokes the model; the canonical candidate Transactions and signatures in the round records are enough.

**Writes.** The initial record is written before Round 1. After each round, the round record, round-end World, assertions, and metrics are saved in one transaction. If Round 4 fails, Rounds 1 to 3 remain available.

**Rejection reasons.** A `RejectReason` is stored as a structured value, so a stored event reads back equal to the event that was recorded. Its display text is derived from the value when shown and is not stored.

**Schema.** The store is SQLite. Its schema version is the database's user version. There is one schema, version 9 (`schemaVersionCurrent`), and there are no migrations: a change to the schema starts a new store. Version 2 removed `deliveries` and `scheduled` from round-end Worlds, which `pending` replaces, version 3 added `laws`, version 4 renamed the stored kernel Law changes, version 5 renamed stored keys to the current terms, version 8 removed two unused fields from stored `pending`, and version 9 added the `Enter` Intent and the `OfAgreement` cause. The one exception is additive: `run_sets` (`name`, `baseline`, `runs` as a JSON array of run IDs) is created wherever it is absent, and the store keeps its version.

| Table | Columns | Key |
|---|---|---|
| `runs` | `id`, `name`, `status`, `spec`, `manifest`, `created`, `reason`, `owner` (role, process id, start time) | `id` |
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
| `GET /api/runs/:id/balancesheet/:token` | Open. One Actor's balance sheet page ([§9.4](#94-balancesheet-pages)). | 200. 404 for an unknown run or token |
| `GET /api/runs/:id/rounds/:round/report` | Open. What one round produced (§8.2). | 200. 404 for an unknown run or a round not played |
| `POST /api/live`, `GET /api/live`, `POST /api/live/transcripts`, `POST /api/live/submissions`, `POST /api/live/advance`, `…/pause`, `…/resume`, `…/end`, `…/clear` | The live run: start it with its cast, tag and metadata (admin), read its state and submit a seat's text (open), a facilitator's submission and the lifecycle controls (admin). See §8.2. | See §8.2 |
| `GET /api/openapi/:service` | Open. The OpenAPI document of `transcription.json` or `narrative.json` (§8.2). | 200. 404 for any other name |
| `GET /api/premise` | The launch form's PremiseCompositions: name, version, its Premises with their versions, round range, default rounds, game seed, `defaultName` (the next run ID), and the seat presets in order. | 200 |
| `GET /api/runs` | The run list, one row per run, with its id, class (`simulated` or `interactive`) and readable flag. `class=` lists one class. | 200. 400 for an unknown class |
| `GET /api/runs/:id/:view` | One of 21 views: `overview`, `objectives`, `balances`, `exchanges`, `transformations`, `economy`, `representation`, `procedures`, `assertions`, `wholeeconomy`, `events`, `standings`, `networth`, `actions`, `drivers`, `balance`, `dependence`, `finance`, `indicators`, `regimes`, `clearing`. `round` applies to `balances` (default: the final round) and `events` (default 1). | 200. 400 for a bad round. 404 for an unknown run or view. 409 when the view needs a PremiseComposition that is not loaded. 500 for an unreadable run |
| `GET /api/runs/:id/actors` | The run's Actors, each marked as a seat or not, and the default Actor. | 200. 404. 500 |
| `GET /api/runs/:id/actor/:actor` | One Actor's balance sheet after every round, with a chart and its net worth. | 200. 400 for an Actor the run does not have, listing the valid names. 404. 500 |
| `GET /api/compare?runs=a,b[&exchange=m]` | Compare a Run set; with no `runs`, every simulated run. The first valid run is the baseline. Each table marks the cells that differ from the baseline. | 200. 400 when no run is valid or `exchange` is unknown |
| `GET /api/glossary` | The definitions and reasons the dashboard shows, generated from the columns the views use. Served whole, so the dashboard holds no glossary text. | 200 |
| `GET /api/agency` | Agency analysis of a PremiseComposition (§10.3): every seat plays `preset`, then each Playable Actor in turn plays `alternative`. | 200. 400 for an unknown PremiseComposition, a bad seed, or an unknown preset |
| `GET /api/agency/detail` | The same analysis, beyond its table: each Playable Actor's objective criteria under the baseline and the alternative. | 200 with the view model. 400 for a bad query. |

Runs are not launched, derived, resumed, archived or deleted through the API. Simulated runs are played by the static suite (§3), and interactive runs by the live routes (§8.2). The store interface supports save, retrieve, list, full round histories, and delete, which the suite uses to drop a run that has left it.

### 8.2 External services

Speech transcription and the newspaper are built outside this repository and reach it over open HTTPS routes. This repository does not transcribe speech and does not write a front page. Reading a round's transcripts into Intents, the joint pass, stays here ([04 §11.2](04_run.md#112-live-session)).

Their contract is two OpenAPI 3.1 documents, the source of truth for those routes: `GET /api/openapi/transcription.json` (the live run: starting it, its state, a seat's text, and its lifecycle controls) and `GET /api/openapi/narrative.json` (a round's report). `just _premises` writes the same documents to `openapi/`. Each document takes its routes, summaries, paths, methods, parameters and request bodies from the route table, and its replies and statuses from the contract. A route that needs the admin session says so: the document declares an `adminSession` security scheme (the `admin` cookie that `POST /admin/login` sets) and lists it on that route, with a 401 among its refusals.

**One live run.** A server holds at most one live run (an interactive run, §3). It is tagged `mock` or `production`, and starting one while another is live is refused unless the request clears the live one first. Its lifecycle:

| State or step | Meaning |
|---|---|
| `running` | Rounds are played; each round closes when every human seat has spoken, its window passes, or it is advanced. |
| paused | Still `running`, with the window's clock stopped; submissions are taken. |
| ending | Still `running`; the run completes after the round open. |
| `completed` | Played to its rounds, or ended. |
| `cleared` | Stopped at once by a facilitator, or by a server restart; the rounds it played stay stored. |
| `failed` | A device failed; the rounds already played stay readable. |

**How to authenticate.** The live run's state, a seat's own text, the round report and the documents are open: anyone with the URL can read every Actor's report and submit any human seat's text. Starting the run, a facilitator's submission and the lifecycle controls need the admin session cookie (§8.3).

| Route | Access | Request | Replies |
|---|---|---|---|
| `POST /api/live` | Admin | `tag` (`mock` or `production`), `metadata` (`name`, `facilitator`, `date`, `venue`, `players` by seat id, `notes`); optional `composition`, `cast` ([04 §2](04_run.md#2-instantiation-configuration); absent casts every seat as an agent), `rounds`, `window` (seconds, 0 for none), `playerSeed`, `replace` | 200 with the Run id and each Actor's balance sheet path. 400 for an unknown PremiseComposition, a cast that does not assemble, rounds outside the range, an unknown tag, or a production run whose metadata is incomplete, naming each missing field (`players.<seat>` for a human seat with no player). 409 while a run is live, unless `replace` is true, which clears it first. 401 without the admin session |
| `GET /api/live` | Open | | 200 with the live run's state, or the last run's when none is live. 404 when the server has held none |
| `POST /api/live/transcripts` | Open | `seat`, `round`, `text` | 200 accepted, with the state. 400 for a seat no person plays or a body that does not decode. 404 with no run. 409 for a round not open (or advanced), a seat that has already spoken this round, or a run that is over |
| `POST /api/live/submissions` | Admin | `seat`, `round`, `text` | The same, for a facilitator entering a human seat's text; the stored transcript records it as entered by the facilitator |
| `POST /api/live/advance` | Admin | | 200 with the state: the open round closes now, every human seat not yet heard says nothing, and the round resolves. 404 with no run. 409 for a run that is over |
| `POST /api/live/pause`, `POST /api/live/resume` | Admin | | The same: the window's clock stops, or restarts |
| `POST /api/live/end` | Admin | | The same: the run completes after the round open |
| `POST /api/live/clear` | Admin | | 200 with `{"cleared": id}`: the run stops now and is recorded as cleared. 404 when no run is live |
| `GET /api/runs/:id/rounds/:round/report` | Open | | 200 with the round report. 404 for an unknown run or a round it has not played |
| `GET /api/openapi/transcription.json`, `GET /api/openapi/narrative.json` | Open | | 200 with the document |

A refusal's body is `{"error": "..."}`. The reply to `POST /api/live`:

```json
{"id":"run-23","balanceSheets":[{"actor":"US","kind":"Playable Actor","path":"/balancesheet/run-23/abc123"}]}
```

for the body:

```json
{"cast":{"US":"human","CN":"agent","US-firm":"preset:Builder","CN-firm":"nonplayer:FreeTrade"},"rounds":4,"playerSeed":1,"tag":"mock","metadata":{"name":"rehearsal"}}
```

The live run's state, which is also the reply to an accepted submission and to a control:

```json
{"id":"run-23","tag":"mock","round":1,"status":"running","paused":false,"ending":false,"window":0,"submitted":[],"awaiting":["US"]}
```

A submission's body:

```json
{"seat":"US","round":1,"text":"We sell ten Energy to Germany."}
```

**The order of calls in a round.**

1. A facilitator starts the live run with `POST /api/live`, under the admin session. The reply's `id` is the Run id.
2. The speech component reads `GET /api/live` for the open `round` and the seats still `awaiting`.
3. It posts each seat's text once, with that round number. The round closes when every human seat has submitted, its window passes, or the facilitator advances it, and a seat that has not spoken by then says nothing that round.
4. The joint pass reads the round's transcripts and the round resolves ([04 §11](04_run.md#11-round-execution)).
5. The newspaper reads `GET /api/runs/:id/rounds/:round/report` once the run reports a later `round`, or a `status` of `completed` for the last round. The report is a 404 until the round has been played.
6. A `status` of `failed` or `cleared` ends the run, and the rounds already played stay readable.

The live run is held in the server's memory. After a restart `GET /api/live` replies 404 and the Run is marked cleared ([§6.1](#61-invalid-actions-and-failures)), while its saved rounds remain.

**What is stable.** The routes, their methods, the field names of their requests and replies, and the status codes above are the contract. In a report, `prices`, `balances`, `shortfalls`, `enacted`, `repealed`, `rejected` and `lapsed` are summaries meant for a downstream writer, with numbers as display strings ([02 §9.1](02_kernel.md#91-numerics-and-randomness)). `events` carries the engine's events in their stored form and changes with the store's schema version. Ids and balance sheet paths are opaque text. The wording of an `error` is not part of the contract.

### 8.3 Public and admin surfaces

Everything that shows more than one Actor's data sits behind a password.

| Surface | Access |
|---|---|
| `/` | Public. The landing page, a page of the dashboard app with its nav bar and themes, with no run data and a link to `/admin`. |
| `/balancesheet/:run/:token` | Public. One Actor's own page ([§9.4](#94-balancesheet-pages)). |
| `/admin` | The login page, and the dashboard once logged in. Its hashed assets under `/admin/assets` are public because the balance sheet page loads them, and they hold no data. |
| `/api` | Private by default ([§8.1](#81-read-api-and-run-operations)). |

**The API is private by default.** Each row of the route table is `Admin` unless the table says `Open`, so a new route is private until someone decides it is not. The open routes are the balance sheet route, the live run's state and a seat's own transcript route, the round report, and the OpenAPI documents. Every other route replies 401 without the admin session, and the gate checks this before a request's body is read. A test walks the whole route table to hold this.

**The password.** `ADMIN_PASSWORD` is read from a `.env` file, whose path is the server's `--env` flag (default `.env`). The file is not committed; `.env.example` names the variable with no value. A missing file or an empty password refuses every admin request. A correct login at `POST /admin/login` sets the session cookie: `HttpOnly`, `SameSite=Lax`, `Secure` when the request came over HTTPS. Its value is derived from the password, so a restart logs nobody out.

---

# Part IV: Dashboard

## 9. Pages

The Dashboard is served at `/admin`, behind the password (§8.3). It has two pages, each with its own hash route and a top bar that links to them from every page: Simulations (`#/simulations`, the home page, and `#/simulations/:id` with that run selected) and Live (`#/live`, and `#/live/:id` with a past run selected). Runs are never launched, derived, resumed, archived or deleted from the dashboard. It renders from persisted data and does not run the engine. Every number shows its display kind (quantity, price, or ratio) as [02 §9.1](02_kernel.md#91-numerics-and-randomness) defines it. Every visible column and field has a plain-language definition on hover, from the glossary (§8.1).

### 9.1 Simulations

Simulations shows the static suite (§3), every run compared with every other:

- **Suite runs.** A sortable table, one row per simulated run, in the order the suite stored them: its name, PremiseComposition, seeds, completed rounds, status, failing assertions, reason for a failure, and class. A run that cannot be decoded shows status `unreadable` and its reason, and the other runs stay visible. Selecting a row, or opening `#/simulations/:id`, opens that run; with no selection, the latest readable run is open.
- **Every run compared.** The comparison of every simulated run (§10): prices by Exchange, net worth over time, final standings, the spread and Gini, the key values, and detail tabs (final state, baseline difference, configuration, compatibility, seed variation, and agency, §10.3, read from the suite's agency runs).
- **Balance sheets.** Each Actor's page for the open run, as its whole address (§9.4).
- **The open run.** Its results (§9.3).

### 9.2 Live

Live shows the one live run (§8.2) and the past interactive runs. The live run's state is read every second and a half:

- **Live run.** Its id, tag, round, status, whether it is paused or ending after this round, its window, and which human seats have spoken and which are awaited. While it runs, the admin controls drive it: advance the round, pause or resume, end after this round, and clear, each confirmed. A text box for each awaited human seat submits a facilitator's text for it.
- **Start a live run.** The composition; the cast, one choice per castable Actor of human, agent, a preset, or a non-player on a preset ([04 §2](04_run.md#2-instantiation-configuration)); the rounds, window and player seed; the tag; and the metadata (name, facilitator, date, venue, a player for each human seat, notes), the fields a production run needs marked required. The server's refusal is shown beside the form, naming each missing field. When a run is live the form says so and starts with "Clear and start", which clears it first.
- **The live run's balance sheets and results so far** (§9.4, §9.3).
- **Past interactive runs.** A table of every interactive run with its tag; selecting one opens its balance sheets and results. The runs of each tag are compared together.

### 9.3 Run Details

The page opens with a header of the run's facts, then four sections by economic object, each a generic view over any composition: the headline (`Indicators`: for each country its substitute gap, net external position and domestic absorption, and for each seat its debt service and viability margins, by round, [05 §8.2](05_pacific.md#82-what-is-derived-not-stored)); the national regimes (`Regimes`: for each country with households, three panels, its productive capability (capacity, competence, units run and idle), its effective demand (household income, spending and saving, sales at home and abroad) and its financial capacity (owed to it, what it owes, debt service paid)); international clearing (`Clearing`: goods and capital across each border and the Rules at the borders, beside the Exchanges' prices and each country's current account); and the seat scorecards (objectives). An engine-detail tab holds every other view, in order: key figures; marginal prices of each Exchange; standings; net worth over time; actions; drivers (rules enacted and repealed, procedure outcomes, shortfalls); balance measures (spread and Gini); objectives; dependence (for each fixed requirement: the capacity covering it, coverage = min(1, capacity / requirement), exposure = max(0, requirement − capacity), imports, exports, receipts, shortfall, the bans in force against it, and the capacity on order); finance (the bonds open after each round, with lender, borrower, currency, principal and rate; and each country's external accounts by round and currency, with goods, currency trades, levies and interest across its border and a residual that is zero when every flow is accounted, and its current account, financial account and valuation in the numeraire, [05 §4.4](05_pacific.md#44-accounting)); the final balance sheet of every Actor (`Initial`, `Current`, `Change`); and one Actor's balance sheet, selected from the run's Actors, defaulting to the first seat.

Net worth is the numeraire plus each other Resource valued in it: a Resource an Exchange quotes in the numeraire at its marginal price; a second currency, in which an Exchange quotes the numeraire, at one over that price; and a Resource quoted only in such a currency at its price there times the currency's value. Where several Exchanges trade one Resource, its price is their mean, so a holding counts once, and a Resource with none of these is not counted. Spread is the highest minus the lowest final net worth of the players. The Gini coefficient of final net worths is 0 when every player holds the same and 1 − 1/n when one holds everything, and it is n/a when the total is zero or a value is negative.

Standings rank 1 is the highest net worth, and equal net worths share a rank. Aggregates are residual representations, not players. An objective criterion is met, not met, or not evaluated; it is never reported as met when its series is empty.

The run page shows the representation and procedure records. The round-by-round action counts, the Exchange, Transformation, and economy views, and the raw event log are served by the API only. Representation records span two rounds: the observation, the estimate, and the Aggregate's purchase that applies the stored factor, from which the influence ratio is derived: the next-round Aggregate change attributable to a represented Actor, divided by that Actor's executed purchase. The event inspector lists one round's events in engine order, with a text filter.

### 9.4 BalanceSheet pages

At the start of a game each Actor, Playable, non-playable or Aggregate, gets a URL showing its own balance sheet: `/balancesheet/:run/:token`, read through `GET /api/runs/:id/balancesheet/:token`. `POST /api/live` and the CLI's `session` command report each Actor's path at the start, and the balance sheets panel of the Simulations and Live pages lists every path for the open run, each as its whole address, through the admin-only `GET /api/runs/:id/balancesheets`. An unknown token is a 404. The Run id is in the path because a player seed can repeat across Runs.

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
- **Key values:** besides net worth, rank, actions, the balance measures, objectives and prices, each Actor's receipts from its Exchange sales over the run, the final coverage of each fixed requirement by the Actor's own capacity, each requirement's shortfall over the run, each seller's sales over the run to Actors of its own group (at home) and to the rest (abroad), and how many of its orders a Rule blocked. Between a run and its counterfactual without a restriction, the shortfall row is the restriction's realized effect.
- **Configuration difference:** parameter differences, one row each, categorised as version, seat Strategy, seed, or other. The other category holds deliberate changes such as overrides, so they are distinguished from the rest.

### 10.3 Agency analysis

Agency is read from the suite's agency runs (§3): each seat alone cast to its alternative preset, with its routine kept, against the default preset's run, everything else and both seeds fixed. A comparison finds them among the runs it compares: a run whose cast differs from a preset run's in exactly one seat, cast to another preset, is that seat's agency run against that preset run (`Views.Agency`). It measures the change in prices, in other Actors' holdings, in Aggregate behaviour, in institutional decisions, and in the Actor's own objective criteria. An unknown alternative preset fails the composition's checks.

The comparison's Agency tab shows one row per agency run. A row whose alternative makes negligible difference on every measure is marked `no: balancing finding`. Under it, each seat's objective criteria are shown under the baseline and the alternative.

---

# Part V: Experimentation

## 11. Experiments

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

The suite runs the reference PremiseComposition, *Pacific* ([05](05_pacific.md)), with every seat bound to a preset.

## 14. Mandatory Tests

Each test passes when its cited fixture's expected values hold and its listed invariants pass. The suites share their fixtures and the composition's loading through `testkit`.

| # | Name | What it checks |
|---|---|---|
| 1 | Full game | Six rounds with every seat `FreeTrade`, and with every seat `Hawk`. Each round is valid, the implemented invariants of §7.1 hold, and the same seeds reproduce ([INV-13](02_kernel.md#inv-13)). |
| 2 | Replay | `Hawk` run, then replayed under the replay handler from the recorded input. Round records and round-end Worlds are identical ([INV-13](02_kernel.md#inv-13)). |
| 3 | Submission order | For the `FreeTrade`, `Hawk`, `Populist` and `Hedger` runs, each round's recorded inputs in every reordering settle to the recorded world ([INV-11](02_kernel.md#inv-11)). |
| 4 | Engine fixtures | [05 §10.6](05_pacific.md#106-engine-fixtures) P-A to P-F through `resolveRound` alone, the currency dealer and a foreign loan under the asset fee among them, and the order independence of P-A, P-H and P-C ([INV-04](02_kernel.md#inv-04), [INV-05](02_kernel.md#inv-05), [INV-11](02_kernel.md#inv-11)). |
| 5 | Counterfactual pair | Free trade against the same run with China's export control from round 1: China's state sells industrial inputs after round 1 in the first, and none to America in the second. |
| 6 | Persistence | Save a run to the Run Store, reload it, compare its stored records and round-end Worlds with the pure run, and replay from the stored records. The run is identical ([INV-31](02_kernel.md#inv-31)). |
| 7 | Agency | [§10.3](#103-agency-analysis) across every seat of the reference PremiseComposition, `FreeTrade` against each seat's alternative (the states on `Hawk`, the firms on `Builder`), each keeping its routine: every Playable Actor has a measurable effect and its objective criteria are reported, and the table read from the suite's stored agency runs equals the analysis. |
| 8 | Pacific golden scenarios | [05 §10.1](05_pacific.md#101-trade-barriers-and-substitution) A to H: free trade in two currencies, each export control, substitution held back by competence, a tariff its importer pays, a barrier met by substitution in both directions, and the invariants, replay, submission order, each currency's total, repeated enactment and the controls that cannot be bypassed ([INV-04](02_kernel.md#inv-04), [INV-05](02_kernel.md#inv-05), [INV-07](02_kernel.md#inv-07), [INV-11](02_kernel.md#inv-11), [INV-13](02_kernel.md#inv-13)). |
| 9 | Routine beside a player | [05 §10.1](05_pacific.md#101-trade-barriers-and-substitution) Routine: a human seat composed with its routine, whose commerce reads what the human's speech put forward. |
| 10 | Sensitivity | [05 §12](05_pacific.md#12-sensitivity): each structural difference between the countries keeps its effect over each dial changed alone. |
| 11 | Pacific firms, money and balance | [05 §10.2](05_pacific.md#102-firms-loans-and-the-households) I to O: China's loans at the repression setting's rate, dividends, a firm selling only at home, the levies, how differently the two countries save and bear an export control, a stockpile against one; [05 §10.3](05_pacific.md#103-money-between-the-countries), the dollar moving with trade, reserves and the asset fee; and [05 §10.3](05_pacific.md#104-balance), each seat's score over sampled settings of every tool, with every external account balancing in each ([INV-04](02_kernel.md#inv-04), [INV-11](02_kernel.md#inv-11), [INV-13](02_kernel.md#inv-13)). |
| 12 | Circuits, accounting and loops | [05 §10.5](05_pacific.md#105-circuits-accounting-and-loops): every leg of the four circular flows carries a flow; every open bond is one claim and one liability; every external account balances in every preset; the repression transfer; and each feedback loop F1 to F12 under the conditions [05 §6](05_pacific.md#6-how-the-objects-interact) states, with its counter-case ([INV-05](02_kernel.md#inv-05)). |
| 13 | Experiments | [05 §11](05_pacific.md#11-experiments): the political trap, the dollar trap, lost knowledge, international leverage, mutually reinforcing dependence, and the status quo against rebalancing. |
| 14 | Derived indicators | [05 §10.5](05_pacific.md#105-circuits-accounting-and-loops) Indicators: the substitute gap, net external position, debt service and viability margin of a Pacific run, as literals; and INV-38 tripped by a tampered run. |
| 15 | Static suite | `Premises.Suite`: one run per preset and one agency run per seat; against the cache keys a fresh store plays every entry, an unchanged one none, a changed dial or engine every run, a changed preset only the runs that use it, and a removed preset drops its run. |
| 16 | Cast | [04 §2](04_run.md#2-instantiation-configuration): every seat cast to one preset plays as the preset run; a non-player plays as the seat did; a mixed cast assembles; each refusal (an Aggregate, an unknown id or preset, a forbidden seat, a seat left out). |

The premises suite also runs the authored agreements of [03 §15](03_premise.md#15-authoring-laws-and-agreements), which are not a mandatory test. The core suite covers only the kernel, and the rest of the former core tests are in `packages/laws/test`. Unnumbered tests in the same suites: every preset plays 8 rounds with the invariants holding; a run resumed from its third round-end World matches the uninterrupted run; an order from outside an Exchange's access is rejected; the INV-29 and INV-30 checks; and player seeds leave a game with no seeded choice unchanged.

**Live interpretation check (one-off, billed, not in CI).** One real model call over fixed transcripts checks that the reply decodes and that a joint candidate Transaction reconciles. It needs an explicit opt-in, and no suite runs it.
