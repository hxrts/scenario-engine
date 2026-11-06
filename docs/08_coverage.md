# 08 Feature Coverage Audit

This audit maps each feature of the game to the tests that exercise it, and records whether the
capstone run exercises it. The capstone is a test-only, eight-round game on the shipped Fractured
World, authored in the scenario DSL (`packages/scenarios/src/Scenarios/Capstone.purs`), compiled to
`packages/scenarios/capstone/run.json`, and decoded through the Policy and Board decoders.

Status values: **covered** (a named test checks it end to end or at unit level), **partial**
(unit level only, hand-built data, or no direct assertion), **gap** (no test). The capstone column:
**yes** (the run exercises it and a capstone test checks it), **partial**, **no**, or **n/a**.

Test references are `file :: test name`, relative to the repository root.

## Language and data

| Feature | Tested today | Status | Capstone |
|---|---|---|---|
| DSL record builders, `:=`, `q` quantities | `packages/scenarios/test/Main.purs` :: "Scenario DSL builders" | covered | yes (every seat program and expectation) |
| DSL withholding and access-ban enactments decode | `packages/scenarios/test/Main.purs` :: "a withholding rule and an enactment of one decode…" | covered | yes |
| DSL capability tags match the decoder | `packages/scenarios/test/Main.purs` :: "every capability the DSL writes is a tag the Gameboard decoder reads" | covered | yes |
| DSL `deal` decodes (now with transforms) | `packages/scenarios/test/Main.purs` :: "a deal decodes, and the parties and Resource it names are reported" | covered | yes |
| Scenario loader: duplicate keys, bad sections, bad policy refs | `packages/scenarios/test/Main.purs` :: "Scenario loader: files and policies fail at load time" | covered | n/a |
| Gameboard validation (each `ValidationError`) | `packages/scenarios/test/Main.purs` :: "Gameboard validation…" | covered | n/a |
| Board decoding of run file, expectations, rules, fixtures | `packages/harness/src/Harness/Capstone.purs` (`decodeCapstone`); `packages/board/test/Main.purs` | covered | yes |

## Policies and programs

| Feature | Tested today | Status | Capstone |
|---|---|---|---|
| Templates `trade`, `transform`, `enactTariff` | `packages/policy/test/Main.purs` (propose tests) | covered | yes |
| Template `choose`, `noAction` | `packages/policy/test/Main.purs` :: "two Choose blocks…", "noAction proposes nothing" | covered | no |
| Template `reciprocate` | `packages/policy/test/Main.purs` :: "reciprocate enacts the mirror…" | covered | no (no restriction against CN in the run) |
| Template `procedure` | `packages/policy/test/Main.purs` :: "a Procedure drafts its intents…" | covered | yes (EU solidarity and trade) |
| Template `deal` (transfers and transforms) | `packages/policy/test/Main.purs` :: "a Deal drafts its transfers…", "a Deal bundles its transforms with its transfers…" | covered | yes |
| Template `enactAccessBan` | `packages/policy/test/Main.purs` :: "enactAccessBan drafts a ban…" (new) | covered | yes (US, round 2) |
| Template `enactWithholding` | `packages/policy/test/Main.purs` :: "enactWithholding drafts a withholding…" (new) | covered | yes (CHIP, round 4) |
| Template `repeal` | `packages/policy/test/Main.purs` :: "repeal drafts a repeal of the named rule…" (new) | covered | yes (US round 6, CHIP round 6) |
| Nested procedure, deal, or choose rejected | `packages/policy/test/Main.purs` :: "a Procedure may not contain a nested…" | covered | n/a |
| Sign rules `procedures`, `facility`, `all` | `packages/policy/test/Main.purs` :: "signs by rule…" | covered | yes |
| Sign rule `signIf` (conditional signing, new) | `packages/policy/test/Main.purs` :: "signIf signs a Proposal only while its conditions hold" | covered | yes (Germany, rounds 4 and 5) |
| Conditions `restrictionInForce`, `transformedLastRound` | `packages/policy/test/Main.purs` | covered | yes (EU tariff; China's procurement) |
| Condition `restrictedLastRound` | `packages/policy/test/Main.purs` :: "restrictedLastRound holds only…" | covered | no |
| Presets (eleven, incl. Deployer) | `packages/harness/test/Main.purs` :: "Every preset plays 8 rounds…"; "7c Deployer" | covered | partial (the US seat uses an inline copy of the Deployer clauses) |
| Market access rules | `packages/core/test/Main.purs` :: "rejects a Trade by an Entity outside the Market's access" | covered | partial (access is set by the shipped Markets; no rejection in the run) |

## Engine: markets, facilities, capacity

| Feature | Tested today | Status | Capstone |
|---|---|---|---|
| Market exchange, limit prices | `packages/harness/test/Main.purs` :: "4 Market exchange: Fixture A and A2" | covered | yes (SME and the US trade each round) |
| Facility `ownerSigns` | `packages/core/test` :: "a Transform… owner must sign" (engine) | covered | yes (SHIP signs the shipment) |
| Facility `effects` (capacity change, holder, rounds) | `packages/harness/test/Main.purs` :: "Soft Power is produced, spent, and decays" | partial | no |
| Capacity escrow and pooled facility ceiling | `packages/core/test/Main.purs` :: "caps an uncapped facility's fills…" | covered | partial |
| Reservations (Mil escrow, scaling, release at commit) | `packages/core/test/Main.purs`; `packages/harness/test/Main.purs` :: "7b Reservation", "7c Deployer" | covered | yes (US round 4: 60 reserved, 38 of 40 ordered; 85.5 Mil held) |
| Fixed flows | fixture and full-game runs only | partial | yes (every shipped flow runs each round) |

## Collective procedure, tariffs, bans, repeal, withholding

| Feature | Tested today | Status | Capstone |
|---|---|---|---|
| Collective procedure passes only above threshold | `packages/harness/test/Main.purs` :: "5 Germany and EU" (Fixture B pair) | covered | yes: solidarity lapses in round 4 (support 0.4) and executes in round 5 (support 0.6) |
| Tariff enactment and in-force condition | `packages/harness/test/Main.purs` :: "11 Trade restriction…" (now DSL-driven) | covered | yes (EU tariff on the US, in force from round 4) |
| Access ban enacted by a seat's program | not expressible before this change; test 11 used a Haskell-built Proposal | covered now | yes: US bans shipments into India in round 2, in force from round 3 |
| Access ban blocks a real shipment | `packages/harness/test/Main.purs` :: "11…" | covered | yes: ENCO's round-4 shipment rejected, "shipment banned"; round 7 clears |
| Repeal of a Rule | `packages/core/test/Main.purs` :: "rejects repealing an unknown rule" only | partial | yes: US ban and CHIP withholding repealed in round 6 (rules in force drop from 3 to 1 by round 7) |
| Withholding changes capacity | decode only before this change | partial | yes: CHIP withholds 5 semiconductor units from round 4 (enacted, scheduled, then repealed) |

## Deals and signed Proposals

| Feature | Tested today | Status | Capstone |
|---|---|---|---|
| Fully signed Proposal settles atomically; unsigned lapse (Fixture G) | `packages/harness/test/Main.purs` :: "10 Signed Proposals" | covered | yes (lapse of solidarity in round 4) |
| Two-party DSL deal executes in a run | none before this change | covered now | yes: JP and IN sign a 20-Energy deal in round 3; India signs by its sign-all rule |
| Deal with transport (transform + transfer) in one Proposal | none before this change | covered now | yes: ENCO and SHIP sign a cross-region shipment in round 7 |
| Submission order independence | `packages/harness/test/Main.purs` :: "3 Submission order…" | covered | n/a |

## Representation and estimators

| Feature | Tested today | Status | Capstone |
|---|---|---|---|
| Estimator, shrinkage, one-round lag | `packages/harness/test/Main.purs` :: "6 SME Representation…" | covered | yes: GLOBAL_SMES factor moves from 0.9358 (round 1) to 0.9406 (round 8) |
| Seeded noise spread | `packages/scenarios/test/Main.purs` :: "C5: seeded noise…" | covered | yes (noise is drawn each round) |
| Representation moves an aggregate's policy in a run | Fixture C only | partial | yes (SME's purchases feed the estimate, which scales GLOBAL_SMES' Atlantic orders) |

## Shortfalls and penalties

| Feature | Tested today | Status | Capstone |
|---|---|---|---|
| Unmet requirement is a shortfall | `packages/core/test/Main.purs` :: "an overdrawing Transfer records a shortfall…" | covered | yes (JP short 4 in round 3 after its deal, 8 in round 4) |
| Penalty reduces incoming flows per unit unmet | none directly | partial | no (the shortfall is recorded; the penalty's size is not asserted) |

## Objectives

| Feature | Tested today | Status | Capstone |
|---|---|---|---|
| Objective outcomes computed | `packages/harness/test/Main.purs` :: "14 Agency…" (count only) | partial | yes |
| Explicit met / not met / not evaluated; none met by default | `packages/harness/test/Test/Harness/Views.purs` :: "a run with no rounds…" | covered | yes: all 16 Actor objectives asserted in the base run (none met) and in the counterfactual (US met) |
| Objective met in one case, missed in another | none before this change | covered now | yes: the US objective is met without the deployment and missed with it |

## Invariants

| Feature | Tested today | Status | Capstone |
|---|---|---|---|
| INV-01..16 each round | `packages/core/src/Core/Invariants.purs`, applied in `packages/harness/test/Main.purs` :: "1 Full game" | covered | yes ("invariants hold in every round") |
| A tampered result trips INV-05 | `packages/scenarios/test/Main.purs` :: "a deliberately unbalanced result trips INV-05" | covered | n/a |

## Simulation: replay, store, counterfactual, resume, archive

| Feature | Tested today | Status | Capstone |
|---|---|---|---|
| Replay from records | `packages/harness/test/Main.purs` :: "2 Replay…" | covered | yes ("replays byte-identically from its records") |
| Store round-trip (save, reload, replay) | `packages/adapters/test/Main.purs` :: "13 Persistence…" | covered | yes ("the capstone run round-trips through the store…") |
| Store migration, busy timeout, startup failure of running runs | `packages/adapters/test/Main.purs` | covered | n/a |
| Counterfactual (a deployment removed, measured) | `packages/harness/test/Main.purs` :: "7 NPC response…" | covered | yes ("the counterfactual without the deployment changes US Mil…") |
| Resume from snapshot matches uninterrupted | `packages/harness/test/Main.purs` :: "a run resumed from its third snapshot…"; `apps/server/test` | covered | yes ("a run resumed from round 4's snapshot…") |
| Archive and delete | `apps/server/test/Main.purs` :: "archive hides a run…" | covered | no |
| Derive kinds (duplicate, rerun, variant, counterfactual) | `apps/server/test/Main.purs` :: "derive: duplicate, rerun, variant…" | covered | no |

## Dashboard and views

| Feature | Tested today | Status | Capstone |
|---|---|---|---|
| View models: overview, balances, events, facilities, pressure, economy, representation, procedures, objectives, final state, markets | `packages/harness/test/Test/Harness/Views.purs` (edge cases only) | partial | yes ("every dashboard view model is non-empty for the run") |
| Trajectories, baseline and config diffs, compatibility, agency table | `packages/harness/test/Test/Harness/Views.purs` | partial | no |
| Dashboard tabs (UI) | `apps/dashboard/test/Main.purs`; Playwright smoke in `apps/dashboard` | partial | no |

## Live and AI seats

| Feature | Tested today | Status | Capstone |
|---|---|---|---|
| Transcript to canonical Intents, stubbed model | `packages/adapters/test/Test/Adapters/Live.purs` :: "text → expected canonical Intents" (inline stub) | covered | yes: a recorded transcript (`packages/scenarios/capstone/transcript.json`) replayed through the stub ("each recorded line maps…") |
| Ambiguous transcript gives a clarification | `Live.purs` :: "ambiguous → clarification"; capstone "an ambiguous recorded line…" | covered | yes |
| Backend failure is a run failure | `Live.purs` :: "a backend failure is fatal to the call…"; capstone "a recorded backend failure…" | covered | yes |
| Scripted live session replays exactly | `Live.purs` :: "a scripted live session equals replaying…" | covered | no |

## CLI and server API

| Feature | Tested today | Status | Capstone |
|---|---|---|---|
| CLI argument handling | `apps/cli/test/Main.purs` | partial | no |
| Server API: runs, run detail, comparison, launch | `apps/server/test/Main.purs` | covered | no |

## Summary

- The table lists each feature once, with where it is tested today and whether the capstone
  exercises it. The rows marked "no" or "partial" in the capstone column are the honest remaining
  gaps, listed below.
- The capstone adds no economics. It adds two decision-language templates (access ban, withholding),
  one repeal template, one conditional sign rule (`signIf`), and transforms inside a deal. Each has
  a policy unit test. The engine already supported all of them.

## Gaps not closed

1. **Penalty size.** The capstone records shortfalls but does not assert the penalty's effect on
   incoming flows.
2. **Entity effects.** Facility `effects` (capacity changes by holder and round) are used only by
   the shipped Soft Power test, not by the capstone.
3. **Archive, delete and derive kinds.** These are server behaviours (`apps/server`). They are out
   of this change's scope, since another agent is changing that code.
4. **CLI and dashboard tabs.** Not exercised by the capstone. The view models are checked for
   non-emptiness only, not for content.
5. **Scripted live session.** The stubbed transcript is replayed, but the full scripted session is
   not extended to the capstone.
6. **Preset Deployer.** The capstone's US seat is an inline copy of the Deployer clauses, so the
   shipped preset is exercised only by its own test.
7. **Agency.** The capstone does not compute the agency table; that stays in
   `Test.Harness.Main` :: "14 Agency…".

## Expected numbers

The capstone's expectations are regenerated from the post-economics world (Mil market, DSL split).
Each value is explained by its mechanism in `packages/scenarios/src/Scenarios/Capstone.purs`. The
values are a snapshot of this run, not an independent derivation; a change to the shipped economics
or to the engine will need them regenerated.
