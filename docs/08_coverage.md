# 08 Feature Coverage Audit

This audit maps each feature of the game to the tests that exercise it, and records whether the
capstone run exercises it. The capstone is a test-only, eight-round game on the shipped Meridian
premise. It is authored in the premise DSL (`packages/premises/src/Premises/Capstone.purs`),
compiled to `packages/premises/capstone/run.json`, and decoded through the Policy and Schema
decoders.

Status values: **covered** (a named test checks it end to end or at unit level), **partial**
(unit level only, hand-built data, or no direct assertion), **gap** (no test). The capstone column:
**yes** (the run exercises it and a capstone test checks it), **partial**, **no**, or **n/a**.

Test references are `file :: test name`, relative to the repository root. Test modules are
`Test.<Pkg>.*`, and each package's `test/Main.purs` holds the suite's main.

## Language and data

| Feature | Tested today | Status | Capstone |
|---|---|---|---|
| Premise format round trips: each builder's output decodes and encodes back | `packages/schema/test/Main.purs` :: "Round trips: decode (encode x) == x for each premise shape" | covered | yes (every seat strategy and expectation) |
| DSL withholding and access-ban enactments decode | `packages/premises/test/Main.purs` :: "a withholding rule and an enactment of one decode to the Withholding rule", "enactAccessBanI decodes to an AccessBan enactment" | covered | yes |
| DSL capability tags match the decoder | `packages/premises/test/Main.purs` :: "every capability the DSL writes is a tag the Gameboard decoder reads" | covered | yes |
| DSL `deal` decodes, with transforms | `packages/premises/test/Main.purs` :: "a deal decodes, and the parties and Resource it names are reported" | covered | yes |
| Premise loader: duplicate keys, bad sections, bad policy refs | `packages/premises/test/Main.purs` :: "a key that two section files both define is an error, not an overwrite", "a section that is not a JSON object is an error", "a policy that names an unknown market fails the load" | covered | n/a |
| Circulation flow amounts are non-negative at decode, so a negative flow is a typed error and never reaches the `allocate` guard | `packages/schema/test/Main.purs` :: "Circulation flows": "a negative flow amount is a typed validation error at decode", "a zero flow amount is valid" | covered | n/a |
| Production list is derived from the capacities the Entities hold, not restated | `packages/premises/test/Main.purs` :: "the production list states exactly the energy capacity the Entities hold" | covered | n/a |
| Decoded Gameboard world carries no seed; only instantiation sets it | `packages/board/test/Main.purs` :: "a decoded Gameboard's world carries no seed; instantiation sets it" | covered | n/a |
| Typed decode rejections (capability, rule, intent, round range) | `packages/schema/test/Main.purs` :: "rejects an unknown rule key with a typed error", "rejects an unknown capability kind", "rejects an intent with two keys" | covered | n/a |
| Gameboard validation (each `ValidationError`) | `packages/premises/test/Main.purs` :: "validates, and its shipped presets resolve against it", "ResolutionInconsistent: an implicit Actor whose authority its Aggregate would not keep" | covered | n/a |
| Capstone expectations and fixtures, as typed values | `packages/simulator/test/Test/Simulator/Capstone.purs` :: "every round's DSL expectations hold". `packages/testkit/src/Testkit/Capstone.purs` reads them from `Premises.Capstone` | covered | yes |

## Policies and strategies

| Feature | Tested today | Status | Capstone |
|---|---|---|---|
| Templates `trade`, `transform`, `enactTariff` | `packages/policy/test/Main.purs` (propose tests) | covered | yes |
| Template `choose`, `noAction` | `packages/policy/test/Main.purs` :: "two Choose blocks…", "noAction proposes nothing" | covered | no |
| Template `reciprocate` | `packages/policy/test/Main.purs` :: "reciprocate enacts the mirror…" | covered | no (no restriction against CN in the run) |
| Template `procedure` | `packages/policy/test/Main.purs` :: "a Procedure drafts its intents…" | covered | yes (EU solidarity and trade) |
| Template `deal` (transfers and transforms) | `packages/policy/test/Main.purs` :: "a Deal drafts its transfers…", "a Deal bundles its transforms with its transfers…" | covered | yes |
| Template `enactAccessBan` | `packages/policy/test/Main.purs` :: "enactAccessBan drafts a ban…" | covered | yes (US, round 2) |
| Template `enactWithholding` | `packages/policy/test/Main.purs` :: "enactWithholding drafts a withholding…" | covered | yes (CHIP, round 4) |
| Template `repeal` | `packages/policy/test/Main.purs` :: "repeal drafts a repeal of the named rule…" | covered | yes (US round 6, CHIP round 6) |
| Nested procedure, deal, or choose rejected | `packages/policy/test/Main.purs` :: "a Procedure may not contain a nested…" | covered | n/a |
| Sign rules `procedures`, `facility`, `all` | `packages/policy/test/Main.purs` :: "signs by rule…" | covered | yes |
| Sign rule `signIf` (conditional signing) | `packages/policy/test/Main.purs` :: "signIf signs a Proposal only while its conditions hold" | covered | yes (Germany, rounds 4 and 5) |
| Conditions `restrictionInForce`, `transformedLastRound` | `packages/policy/test/Main.purs` | covered | yes (EU tariff, China's procurement) |
| Condition `restrictedLastRound` | `packages/policy/test/Main.purs` :: "restrictedLastRound holds only…" | covered | no |
| Presets (eleven, incl. Deployer) | `packages/simulator/test/Main.purs` :: "Every preset plays 8 rounds…". "7c Deployer". `packages/premises/test/Main.purs` :: "the Deployer preset proposes a reserving deployment and a Mil sale only while the Atlantic price is above 3" | covered | partial (the US seat uses an inline copy of the Deployer clauses) |
| Market access rules | `packages/core/test/Main.purs` :: "rejects a Trade by an Entity outside the Market's access" | covered | partial (access is set by the shipped Markets, no rejection in the run) |

## Engine: markets, facilities, capacity

| Feature | Tested today | Status | Capstone |
|---|---|---|---|
| Market exchange, limit prices | `packages/simulator/test/Main.purs` :: "4 Market exchange: Fixture A and A2" | covered | yes (SME and the US trade each round) |
| Facility `ownerSigns` | none directly. Enforced in `packages/core/src/Core/Engine/Screen.purs` | partial | yes (SHIP signs the shipment) |
| Facility `effects` (capacity change, holder, rounds) | `packages/simulator/test/Main.purs` :: "Soft Power is produced, spent, and decays" | partial | no |
| Capacity escrow and pooled facility ceiling | `packages/core/test/Main.purs` :: "caps an uncapped facility's fills…" | covered | partial |
| Reservations (Mil escrow, scaling, release at commit) | `packages/core/test/Main.purs`. `packages/simulator/test/Main.purs` :: "7b Reservation", "7c Deployer" | covered | yes (US round 4: 60 reserved, 38 of 40 ordered, 85.5 Mil held) |
| Fixed flows | fixture and full-game runs only | partial | yes (every shipped flow runs each round) |

## Collective procedure, tariffs, bans, repeal, withholding

| Feature | Tested today | Status | Capstone |
|---|---|---|---|
| Collective procedure passes only above threshold | `packages/simulator/test/Main.purs` :: "5 Germany and EU" (Fixture B pair) | covered | yes: solidarity lapses in round 4 (support 0.4) and executes in round 5 (support 0.6) |
| Major shock flips preset behaviour. Disruption offers never below zero | `packages/simulator/test/Test/Simulator/Shock.purs` :: "Major shock (docs/05 §13.6)" | covered | no |
| Solidarity pool: Germany's transfer in, and the pool drawn first by the EU's procedure | `packages/premises/test/Test/Premises/Golden.purs` :: "Golden fixtures" runs each fixture of `packages/premises/src/Premises/Fixtures/GermanyEu.purs`, named "B1" and "B2-pool" (Fixture B) | covered | no (pool stays empty in the capstone) |
| Tariff enactment and in-force condition | `packages/simulator/test/Main.purs` :: "11 Trade restriction…" | covered | yes (EU tariff on the US, in force from round 4) |
| Access ban enacted by a seat's strategy | `packages/policy/test/Main.purs` :: "enactAccessBan drafts a ban…" | covered | yes: US bans shipments into India in round 2, in force from round 3 |
| Access ban blocks a real shipment | `packages/simulator/test/Main.purs` :: "11 Trade restriction…" | covered | yes: ENCO's round-4 shipment rejected, "shipment banned". Round 7 clears |
| Repeal of a Rule | `packages/core/test/Main.purs` :: "rejects repealing an unknown rule" | partial | yes: US ban and CHIP withholding repealed in round 6 (rules in force drop from 3 to 1 by round 7) |
| Withholding changes capacity | decode only (`packages/premises/test/Main.purs` :: "a withholding rule and an enactment of one decode…") | partial | yes: CHIP withholds 5 semiconductor units from round 4 (enacted, scheduled, then repealed) |

## Deals and signed Proposals

| Feature | Tested today | Status | Capstone |
|---|---|---|---|
| Fully signed Proposal settles atomically. Unsigned lapse (Fixture G) | `packages/simulator/test/Main.purs` :: "10 Signed Proposals" | covered | yes (lapse of solidarity in round 4) |
| Two-party DSL deal executes in a run | capstone only | covered | yes: JP and IN sign a 20-Energy deal in round 3. India signs by its sign-all rule |
| Deal with transport (transform + transfer) in one Proposal | capstone only | covered | yes: ENCO and SHIP sign a cross-region shipment in round 7 |
| Joint-proposal rule: a Proposal needs its author to have spoken, signers are speakers, known and other than the author | `packages/core/test/Main.purs` :: "Joint pass rule (docs/04 §11)": "drops a Proposal whose author did not speak, and one with no Intents", "keeps a speaker's Proposal, and signs for speakers only", "keeps as signers only known Entities other than the author" | covered | no |
| Submission order independence | `packages/simulator/test/Main.purs` :: "3 Submission order…" | covered | n/a |

## Representation and estimators

| Feature | Tested today | Status | Capstone |
|---|---|---|---|
| Estimator, shrinkage, one-round lag | `packages/simulator/test/Main.purs` :: "6 SME Representation…" | covered | yes: GLOBAL_SMES factor moves from 0.9358 (round 1) to 0.9406 (round 8) |
| Seeded noise spread | `packages/premises/test/Test/Premises/Golden.purs` :: "C5: seeded noise…" | covered | yes (noise is drawn each round) |
| Representation moves an aggregate's policy in a run | Fixture C only | partial | yes (SME's purchases feed the estimate, which scales GLOBAL_SMES' Atlantic orders) |

## Shortfalls and penalties

| Feature | Tested today | Status | Capstone |
|---|---|---|---|
| Unmet requirement is a shortfall | `packages/core/test/Main.purs` :: "an overdrawing Transfer records a shortfall…" | covered | yes (JP short 4 in round 3 after its deal, 8 in round 4) |
| Penalty reduces incoming flows per unit unmet | none directly | partial | no (the shortfall is recorded, the penalty's size is not asserted) |

## Objectives

| Feature | Tested today | Status | Capstone |
|---|---|---|---|
| Objective outcomes computed | `packages/experiments/test/Test/Experiments/Mandatory.purs` :: "14 Agency: every role is analysed and its objectives reported" (count only) | partial | yes |
| Explicit met / not met / not evaluated. None met by default | `packages/views/test/Test/Views/Views.purs` :: "a run with no rounds: every objective is not evaluated, never met (§10.5)". `packages/views/test/Test/Views/Capstone.purs` :: "every objective has an explicit outcome, and the run's are the DSL's (none met by default)" | covered | yes: all 16 Actor objectives asserted in the base run (none met) and in the counterfactual (US met) |
| Objective met in one case, missed in another | capstone only | covered | yes: the US objective is met without the deployment and missed with it |

## Invariants

| Feature | Tested today | Status | Capstone |
|---|---|---|---|
| INV-02, 04, 05, 06, 10 each round | `packages/core/src/Core/Invariants.purs` (predicates), applied through `checkRun` in `packages/simulator/test/Main.purs` :: "1 Full game" and `packages/simulator/test/Test/Simulator/Capstone.purs` :: "the checked invariants hold in every round" | covered | yes |
| Other INV-xx in [02 §9](02_type_system.md#9-architectural-invariants-and-numerics) | no predicate in `Core.Invariants`. The rows above and the tests named there | partial | partial |
| A tampered result trips INV-05 | `packages/premises/test/Test/Premises/Golden.purs` :: "a deliberately unbalanced result trips INV-05" | covered | n/a |

## Simulation: replay, store, counterfactual, resume, archive

| Feature | Tested today | Status | Capstone |
|---|---|---|---|
| Replay from records | `packages/simulator/test/Main.purs` :: "2 Replay…". `packages/simulator/test/Test/Simulator/Capstone.purs` :: "replays byte-identically from its records" | covered | yes |
| Store round-trip (save, reload, replay) | `packages/adapters/test/Main.purs` :: "13 Persistence: save, reload byte-identically, and replay from stored records". `packages/adapters/test/Test/Adapters/Capstone.purs` :: "the capstone run round-trips through the store…" | covered | yes |
| Store backend carries typed values; a stored value that does not read back is reported in its own field, and the other runs stay readable | `packages/adapters/test/Main.purs` :: "a stored spec that does not read back…" | covered | no |
| Store migration, busy timeout, failing running runs at startup (`failRunning`) | `packages/adapters/test/Main.purs` | covered | n/a |
| Counterfactual (a deployment removed, measured) | `packages/experiments/test/Test/Experiments/Mandatory.purs` :: "7 NPC response: Fixture D as a pair, with and without the deployment" | covered | yes ("the counterfactual without the deployment changes US Mil…") |
| Resume from snapshot matches uninterrupted | `packages/simulator/test/Main.purs` :: "a run resumed from its third snapshot…". `packages/simulator/test/Test/Simulator/Capstone.purs` :: "a run resumed from round 4's snapshot matches the uninterrupted run" | covered | yes |
| Archive and delete | `apps/server/test/Main.purs` :: "archive hides a run…" | covered | no |
| Derive kinds (duplicate, rerun, variant, counterfactual) | `apps/server/test/Main.purs` :: "derive: duplicate, rerun, variant…" | covered | no |

## Dashboard and views

| Feature | Tested today | Status | Capstone |
|---|---|---|---|
| View models: overview, balances, events, facilities, pressure, economy, representation, procedures, objectives, final state, markets | `packages/views/test/Test/Views/Views.purs` (edge cases only) | partial | yes (`packages/views/test/Test/Views/Capstone.purs` :: "every dashboard view model is non-empty for the run") |
| Trajectories, baseline and config diffs, compatibility, agency table | `packages/views/test/Test/Views/Views.purs` | partial | no |
| Baseline diff reports a resource the baseline holds and the other run lacks | `packages/views/test/Test/Views/Views.purs` :: "a resource the baseline holds and the other run lacks is reported by the baseline diff (§10.2)" | covered | no |
| Dashboard tabs (UI) | `apps/dashboard/test/Main.purs`. Playwright smoke in `apps/dashboard` | partial | no |

## Live and AI seats

| Feature | Tested today | Status | Capstone |
|---|---|---|---|
| Joint pass, stubbed model: a transcript-pair deal reconciles into one Proposal with both as signers; a one-sided deal keeps its author as the only signer; an undecodable reply submits nothing | `packages/handlers/test/Test/Handlers/Llm.purs` :: "two transcripts describing one deal reconcile into one Proposal with both as signers", "a one-sided description keeps only its author as signer". `packages/handlers/test/Test/Handlers/Live.purs` :: "an undecodable joint reply submits nothing…" | covered | yes: the capstone's recording (`packages/premises/capstone/transcript.json`, generated by `just _premises`) replayed through a stub backend (`Test.Handlers.Capstone` :: "each recorded line, in its recorded order, maps to the canonical Intents it records (replayed)") |
| Recorded model calls replay from their typed requests; a request the recording does not hold is an error, never a model call | `packages/handlers/test/Test/Handlers/Llm.purs` :: "a recorded exchange replays…". `packages/handlers/test/Test/Handlers/Capstone.purs` :: "a call the recording does not hold in its next place is an error, never a model call" | covered | yes |
| Capstone recording is generated: each DSL exchange's Intents are checked against the DSL before the file is written | `apps/cli/src/Transcript.purs` (run by `just _premises`) | covered | yes |
| Prompts, reply decoding, and evidence, with no model | `packages/handlers/test/Test/Handlers/Llm.purs` :: "Prompts", "Reply decoding", "Evidence and recording" | covered | no |
| The `pi` process boundary, with no model (a fake `pi` on the PATH); seed, clock, and premise reads | `packages/adapters/test/Test/Adapters/Pi.purs`, `packages/adapters/test/Test/Adapters/Primitives.purs` | covered | no |
| Round's one-sided deal lapses through the signature rule; a matched deal is signed by both | `packages/handlers/test/Test/Handlers/Live.purs` :: "a one-sided deal lapses; a deal both sides describe is signed by both" | covered | no |
| Joint pass against the real model (billed, one-off) | `packages/adapters/test/Test/Adapters/LiveInterpret.purs`, run by `just live-interpret` | manual (not in any suite) | no |
| Audio submission and transcription | none yet | gap: not implemented | no |
| Backend failure is a run failure | `packages/handlers/test/Test/Handlers/Llm.purs` :: "a backend failure is fatal to the call: the error propagates with its cause". `packages/handlers/test/Test/Handlers/Capstone.purs` :: "a recorded backend failure is a run failure, naming the model and its cause" | covered | yes |
| Scripted live session replays exactly | `packages/handlers/test/Test/Handlers/Live.purs` :: "a scripted live session equals replaying…" | covered | no |

## CLI and server API

| Feature | Tested today | Status | Capstone |
|---|---|---|---|
| CLI argument handling | `apps/cli/test/Main.purs` | partial | no |
| Server API: runs, run detail, comparison, launch | `apps/server/test/Main.purs` | covered | no |

## Summary

- Each feature appears once. Rows marked "no" or "partial" in the capstone column are the remaining
  gaps, listed below.
- The capstone adds no economics. It uses the decision-language templates and rules above (access
  ban, withholding, repeal, `signIf`, transforms inside a deal), and each has a policy unit test.

## Gaps in this repo

1. **Penalty size.** The capstone records shortfalls but does not assert the penalty's effect on
   incoming flows.
2. **Entity effects.** Facility `effects` (capacity changes by holder and round) are used only by
   the shipped Soft Power test, not by the capstone.
3. **Archive, delete and derive kinds.** These are server behaviours (`apps/server`) and are not
   exercised by the capstone.
4. **CLI and dashboard tabs.** Not exercised by the capstone. The view models are checked for
   non-emptiness only, not for content.
5. **Preset Deployer.** The capstone's US seat is an inline copy of the Deployer clauses, so the
   shipped preset is exercised only by its own test.
6. **Agency.** The capstone does not compute the agency table. That stays in
   `Test.Experiments.Mandatory` :: "14 Agency…".
7. **Invariants.** Only INV-02, 04, 05, 06 and 10 have predicates. The other IDs in
   [02 §9](02_type_system.md#9-architectural-invariants-and-numerics) are checked, if at all, by the
   tests named in their rows.

## Expected inputs

This repo expects the following to exist.

- **Audio front end.** A separate layer turns each player's audio submission into a transcript.
  The session and the joint interpretation take those transcripts as input, and the scripted live
  session is exercised once that layer exists.

## Expected numbers

The capstone's expectations are a snapshot of the current run. Each value is explained by its
mechanism in `packages/premises/src/Premises/Capstone.purs`. A change to the shipped economics or
to the engine requires regenerating them.
