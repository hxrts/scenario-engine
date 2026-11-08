# Gameboard Specification

**Version:** 0.3
**Scope:** Gameboard Layer
**Implementation language:** PureScript

---

## 1. Purpose

The Gameboard defines a reusable economic and strategic world: the Resources, Markets, Facilities, Entities, Policies, Representation schemes, and Rules that games are instantiated from. It is the Gameboard part of a premise ([§2](#2-gameboard-structure)). It defines the **persistent conditions and tensions** of that world, not the outcomes of any playthrough. One Gameboard supports many games.

Instantiation ([04](04_instantiated_game.md)) chooses the **resolution** (Type System [§2.5](02_type_system.md#25-glossary)) and must preserve the world's initial economic meaning ([INV-14](02_type_system.md#inv-14)). Terms and invariants are defined in the Type System ([§9](02_type_system.md#9-architectural-invariants-and-numerics)). The Gameboard is declarative: it holds no player assignments, Intents, or evolving state, and its checks run before any game starts ([§12](#12-validation-and-compilation)).

**Gameboard files → Validated Gameboard** (for one Instantiation Configuration)

**Validated Gameboard + Instantiation Configuration → Initial World State** ([04 §10](04_instantiated_game.md#10-initialization-pipeline))

## 2. Gameboard Structure

A premise is a directory of JSON section files. The loader merges every file except `instantiation.json` into one Gameboard object and reads `instantiation.json` as the Instantiation Configuration ([04 §2](04_instantiated_game.md#2-instantiation-configuration)). The Meridian premise uses these files under `packages/premises/meridian/v0/` (compiled, see [§12.5](#125-compilation)):

| File | Sections | Keys |
|---|---|---|
| `premise.json` | Premise header (§3) | `id`, `version`, `numeraire`, `horizon`, `roundRange`, `label` (optional) |
| `resources.json` | Resources (§5.1) | `resources` |
| `markets.json` | Markets, flow gates, groups, Rule kinds (§5.2, §5.5) | `markets`, `flowGates`, `groups`, `ruleKinds` |
| `transformations.json` | Facilities (§5.3) | `transformations` |
| `circulation.json` | Recurring cycle (§5.4) | `circulation` |
| `entities.json` | Entity catalog (§6) | `entities` |
| `representations.json` | Representation schemes (§9) | `schemes` |
| `game-rules.json` | Named parameters (§11) and Rules in force at the start (§5.5) | `parameters`, `rules` |
| `policies.json` | Policy catalog (§8) | `policies` |
| `instantiation.json` | Instantiation Configuration ([04 §2](04_instantiated_game.md#2-instantiation-configuration)) | `seed`, `rounds`, `seats`, `npcs`, `fallbacks`, `representations` |

Outside the Gameboard: `packages/premises/presets.json` holds the seat-policy presets shared by all premises ([06 §4.2](06_simulator.md#42-presets)). `packages/premises/fixtures/*.json` are test fixtures ([05 Part IV](05_test_instance.md#part-iv-playtest-fixtures)); their `rounds` list fixture rounds, not the round count.

A `representations` key in a Gameboard section is decoded but ignored: instantiation sets the Representations from `instantiation.json` ([04 §5](04_instantiated_game.md#5-representation-assignment)). The schema has no fields for verticals, relationships, provenance, or visibility (§4, §7, §13).

## 3. Premise

`premise.json` defines:

- `id`, `version`: the stable identifier and version ([§13](#13-provenance-and-versioning)).
- `numeraire`: the unit of account, read by the views for net worth and units. The engine does not read it; the DSL fills it into payments and levies.
- `horizon`: simulated years.
- `roundRange`: `[min, max]`, the supported round counts. Decoding requires `1 <= min <= max`.
- `label` (optional): the test-instance label. A run's game id is `<id>/<label>`.
- `name` (optional): the premise's name, decoded from the merged Gameboard (`nameF`, `Schema.Premise.World`). It may sit in any section file, since the loader merges them; the Meridian premise sets neither `name` nor `round`, so its name comes from `id`.
- `round` (optional, default 1): the starting round, the index the first round of a game is numbered from (`roundF`, `Schema.Premise.World`).

The premise says which forces are relevant, not how they resolve. Its narrative is not a schema field. Meridian's values are in [05 §6](05_test_instance.md#6-premise-meridian).

### 3.1 Strategic tensions

A [strategic tension](02_type_system.md#25-glossary) appears in the Gameboard only through mechanically consequential data: Resources, capacities, Facilities, flows, Market access, or Rules. Each tension must connect to data that participants can act on. Validation does not check this ([§12.3](#123-not-yet-checked-planned)). A tension never requires Actors to act antagonistically. Cooperation and alliances must remain possible.

## 4. Verticals

[Verticals](02_type_system.md#25-glossary) are documented in vertical reports. No schema field or file records them. Meridian's sectors appear only as the Resources, Markets, Facilities, and Entities they produce ([05 §7](05_test_instance.md#7-economic-verticals)).

A report compiles into explicit configuration in [§5](#5-economic-structure)–[§9](#9-representation-schemes). Sources, uncertainty, simplifications, and findings that cannot run mechanically stay in the report. Verticals share Resources, Markets, and Entities rather than duplicating them.

## 5. Economic Structure

### 5.1 Resources

`resources.json` maps each Resource id to:

- `transferability`: `Free`, `Restricted`, or `NonTransferable` (required).
- `reservable` (default `false`): whether a Facility may reserve it.
- `decay` (optional): the per-round decay rate, a plain quantity.

The engine attaches no behavior to a Resource's name. Its effects come from the Facilities, Markets, and cycle entries that reference it.

### 5.2 Exchange Markets

`markets.json` maps each Market id to:

- `base`, `quote`: the traded Resources.
- `cost`: a linear CostFunction `{ intercept, slope, lo, hi }` over the domain `lo` to `hi` (Type System [§5.1](02_type_system.md#51-costfunction)).
- `reserves`: the initial reserves by Resource. Reserves are the only liquidity. There is no separate liquidity parameter.
- `access`: the Entity ids allowed to trade. An Entity with access and no `trade` capability is an account rather than an Actor. EU_POOL is the only one in the reference premise, and its authority comes from the EU's procedure ([05 §10](05_test_instance.md#10-aggregate-catalog)).
- `position` (default `0`): the initial position on the cost curve.

Limit-price orders and clearing are Intents and clearing rules (Type System [§3](02_type_system.md#3-intents-and-economic-actions), [§5.2](02_type_system.md#52-exchange-markets)), not Market fields. Tariffs and access bans are Rules ([§5.5](#55-geography-access-and-rules)).

### 5.3 Facilities

`transformations.json` maps each Facility (Transformation) id to:

- `inputs`, `outputs`: Resource to quantity per unit.
- `escrowPerUnit`: reservable Resources the Facility locks per unit. A non-reservable Resource fails when the premise document is read (the flag stays in the document).
- `payment` (optional): `{ curve, resource, to }`, a CostFunction giving the price per unit, the Resource paid in, and the Entity paid.
- `capacity` (optional): the named capacity that limits throughput. `capacityUse` (default `1`) is the capacity drawn per unit ordered.
- `delay` (default `0`): rounds before output is delivered. With `0`, output lands in the round the order settles. Capacity `effects` always land at least one round after the order.
- `minimum` (default `0`) and `maximum` (optional): bounds on one Entity's order per round.
- `ownerSigns` (default `false`): the capacity's owner must sign every order.
- `effects` (optional): `[{ capacity, perUnit, holder?, rounds? }]`, changes to capacities per unit ordered. `holder` defaults to the ordering Entity. `rounds`, if given, reverses the change after that many rounds.

Defense procurement, public campaigns, energy shipping, and military deployment are all Facilities. Facilities are not a separate type.

### 5.4 Recurring cycle

`circulation.json` holds the cycle that the engine evaluates each round on the batch result (Type System [§6](02_type_system.md#6-economic-circulation)):

- `production`: `[{ entity, resource, capacity }]`. Output from the capacity is delivered in the next round.
- `flows`: `[{ from, to, resource, amount }]`. Fixed payments per round.
- `shares`: `[{ from, to, share, source }]`. A share of a `LegQuery` read without its party, the payer being it (`{ window, side, resource?, cause, aggregate }`): the DSL's `sales <resource>` is net sales this round, and `payee` is payments received this round.
- `requirements`: `[{ entity, resource, amount?, penalty? }]`. Operating requirements consumed each round. `amount` defaults to the round's purchases, and `penalty` defaults to `0`. A shortfall accrues `penalty` against that Entity's incoming flows.

The authored forms and a Resource's `decay` are compiled (`Schema.Recurring`) into the one `RecurringFlow` rule ([02 §6](02_type_system.md#6-economic-circulation)).

Decay follows each Resource's `decay`. Capacity changes come from Facility `effects` and Rules. No renewal or depreciation rule exists. Rates are per round ([§10.2](#102-temporal-structure)).

### 5.5 Geography, access, and Rules

- **Regions** are separate Markets. There is no node hierarchy.
- **`flowGates`** are `[{ resource, exemptSharedMarket, bundled? }]`: `exemptSharedMarket` frees parties who share a Market for the Resource from the gate and from flows Rules, and `bundled` names the Transformation a sender's orders of which must carry the Transfer. How that movement is signed is in Type System [§3.3](02_type_system.md#33-proposals-and-signatures).
- **`groups`** maps an Entity to the Entity it counts as for flows Rules, when different from itself.
- **`ruleKinds`** declares each kind of Rule: `{ form, text }`, the form being `levy:<who>`, `block:<who>` (who governs: `destination`, `origin`, or `either`, which fixes the Rule's direction) or `reduce` (governed by the capacity's owner), the text being how a Rule of the kind is displayed. A premise that declares none has no Rules.
- **`rules`** are institutional conditions, each of a declared kind: `{ flows: { kind, scope: { governor, counterparty, direction, over }, effect } }`, where `over` names the Resources it covers (at least one; a Rule needs no gate) with `effect` `{ levy: { rate, basis, in, to } }` or `block`; `basis` is `perUnit` or `{ adValorem: <market> }`, `in` is the Resource it is paid in, and `to` is `destination`, `governor`, or `sink`, or `{ capacity: { kind, capacity, reduce } }`. Meridian declares `tariff` (a levy, inbound, governed by the destination), `accessBan` (a block, either way), and `withholding` (a reduction of a capacity). A Rule in force at the start is listed in `game-rules.json`. A Rule enacted during play needs the matching `enact:<kind>` capability (Type System [§3](02_type_system.md#3-intents-and-economic-actions)).

### 5.6 Limits

The schema has no separate constraints section. Limits are expressed through reserves, capacities, Market access, and Rules. A negative holding or reserve fails the round at commit ([04 §11](04_instantiated_game.md#11-round-execution)). That is a runtime invariant, not a validation check.

## 6. Entity Catalog

`entities.json` maps each Entity id to:

- `accounts`: Resource to holding (required).
- `capabilities`: a list of capability tags (required). The tags are `trade:<market>`, `transfer:<resource>`, `transform:<transformation>`, and `enact:<tariff|accessBan|withholding>`, each a pattern over Intents (Type System [§2.4](02_type_system.md#24-capability)).
- `capacities` (optional): capacity name to quantity.
- `definition` (an Actor, [§6.2](#62-actor-definitions)) or `aggregate` (an Aggregate, [§6.3](#63-aggregates)). An Entity should have exactly one. Validation does not check this.

### 6.1 Projections

The catalog configures the three projections of Type System [§2.2](02_type_system.md#22-three-projections-of-an-entity) separately:

- **Members**: `aggregate.members`. Descriptive. Lists may overlap. Membership confers nothing.
- **Accounts and capacities**: each account is held by its Entity, and each capacity has exactly one owner ([§12.2](#122-checks-implemented)).
- **Capabilities and procedures**: `capabilities`, and `authorizes` in each procedure ([§6.3](#63-aggregates)).

Membership does not imply ownership, and ownership does not imply authority ([INV-03](02_type_system.md#inv-03)).

### 6.2 Actor definitions

An Actor is an Entity with a `definition`:

- `required` (default `false`): the configuration must select this Actor.
- `seat` (default `true`): the Actor may be controlled by a seat.
- `npc` (default `[]`): the NPC Policy ids that may drive it.
- `fallback` (optional): used when the Actor is unselected ([§6.5](#65-fallbacks)). Required for every Actor that is not `required` (`MissingFallback`).
- `objective` (`{ description, criteria }`, [§10.5](#105-evaluation)): required by validation (`MissingObjective`).
- `description`, `dependency` (default `""`): prose. `dependency` is not checked.

[INV-19](02_type_system.md#inv-19) requires a capability, a dependency, and a measurable objective for every Actor. Validation checks the objective only ([§12.3](#123-not-yet-checked-planned)).

### 6.3 Aggregates

An Aggregate is an Entity with an `aggregate` block:

- `policy`: the baseline Policy id (required, must exist in `policies`).
- `members`: Entity ids.
- `procedures`: named collective procedures, each `{ weights: { member: weight }, residualWeight, threshold, authorizes: [capability tags] }`.

The Aggregate's accounts are its **residual**: the holdings that remain when its coverage Actors are explicit. An unselected coverage Actor's holdings are added to them at instantiation ([04 §4.2](04_instantiated_game.md#42-partitioning)).

A collective procedure is a weighted-signature rule on the Aggregate's own Proposals (Type System [§3.3](02_type_system.md#33-proposals-and-signatures)). A procedure is not a Representation ([§9](#9-representation-schemes)).

### 6.4 Overlap

Memberships may overlap. Capacities have one owner ([§12.2](#122-checks-implemented)). Accounts move between Entities only through instantiation's coverage merge ([04 §4.2](04_instantiated_game.md#42-partitioning)), which gives each account one owner in every resolution ([INV-08](02_type_system.md#inv-08)).

### 6.5 Fallbacks

A fallback is a tag, in an Actor's `fallback` or in the configuration's `fallbacks` ([04 §3.2](04_instantiated_game.md#32-fallback-resolution)):

| Tag | Effect when the Actor is unselected |
|---|---|
| `coverage:<aggregate>` | Holdings, capacities, procedure weight, Market access, flows, groups, facility payees and effect holders, Rules in force and scheduled, and Aggregate member lists and other Aggregates' procedure weights that name the Actor move to the named Aggregate. The target must be an Aggregate ([§12.2](#122-checks-implemented)). |
| `policy:<policy>` | The Entity stays and is driven by that in-world Policy as an NPC. Its capabilities stay with it. |
| `omission` | No change. Its holdings and capabilities remain in World State with no controller, so it submits nothing. |

The three are not interchangeable: a government's regulatory authority never moves to a household or business Aggregate ([INV-03](02_type_system.md#inv-03)).

## 7. Structural Relationships

The schema has no relationship list. Mechanical links are the fields above (dependency: Facility inputs and outputs, `requirements`, `flows`, `shares`. Competition: `access`. Authority: `groups`, Rules, `enact`. Membership: `members`, `procedures`). Descriptive relationships are context in the premise report ([05](05_test_instance.md)) and do not alter outcomes. A typed relationship list is planned.

## 8. Policy Catalog

`policies.json` maps each Policy id to a Strategy, `{ rules, sign }`, in the shared decision language (Type System [§4](02_type_system.md#4-policies)). Strategies are declarative and fixed for a game. The loader checks that every name a Strategy uses (Entities, Markets, Resources, Facilities, parameters) exists. A Market position is a Market id, or a selector object over a Resource (`{ "each": "E" }`, `{ "cheapest": "E" }`, `{ "costliest": "E" }`), whose Resource is checked; `self` and the Markets a selector picks resolve at run time (`Policy.Interpret`).

Policies are referenced by an Actor's `npc` list (chosen in the configuration's `npcs`) and by `aggregate.policy`. Seat presets are bound by the simulator or session ([06 §4](06_simulator.md#4-seat-policies)).

A Policy reads last round's record through `Recorded` conditions (`transformedLastRound`, `restrictedLastRound`, `restrictionInForce`, in the DSL), the stored Representation factor (`Factor`), and Gameboard parameters ([§11](#11-initialization-constraints)). Planned, not in the schema: per-category View permissions ([INV-16](02_type_system.md#inv-16)), parameter ranges and defaults, and declared behavioral domains for Aggregates.

## 9. Representation Schemes

`representations.json` lists schemes. Each has:

- `id`: the scheme's identifier.
- `eligibleActors`: Actor ids that may represent the Aggregate (must exist).
- `target`: the Aggregate represented (must be an Aggregate).
- `domain`: the behaviour domain adjusted.
- `observes` (default: the Resource named like the domain, which must exist): the Resource whose purchases are observed.
- `estimator`: `reference` and `shrinkage` are required. `bound` (optional) clamps each deviation to `±bound`. `noiseSpread` (default `0`) sets the amplitude of sampling noise.

A scheme is a template. Which representatives are active, and their weights, are set at instantiation ([04 §5](04_instantiated_game.md#5-representation-assignment)). Schemes declare no permitted ranges for those values. Range checks are planned. The estimator formula and adjustment pipeline are in Type System [§7.2](02_type_system.md#72-representation-and-estimators)–[§7.4](02_type_system.md#74-estimation-semantics).

## 10. Game Rules

### 10.1 Proposal rules

Agreements reach the engine as Proposals that name more than one signer, such as joint Proposals and procedure Proposals ([INV-10](02_type_system.md#inv-10)). Two Gameboard rules govern what they can move: Resource transferability ([§5.1](#51-resources)) and each Entity's capabilities ([§6.2](#62-actor-definitions)). A switch restricting multi-signer Proposals by Resource or Entity category is planned.

Aggregates do not sign Proposals in the round runner. Procedure Proposals are signed by their members ([04 §11](04_instantiated_game.md#11-round-execution)).

### 10.2 Temporal structure

The Gameboard fixes `horizon`, `roundRange`, and each Facility's `delay` ([§5.3](#53-facilities)). Rule changes take effect at the next round boundary. A procedure Proposal is published, signed, and executed within one round. Rates are per round.

The Gameboard has no round-to-time mapping, no scheduled-shock declaration, and no per-round duration. Scaling annual rates to round length is planned ([INV-28](02_type_system.md#inv-28)). Shocks are named parameters that Policies read ([05 §13.6](05_test_instance.md#136-energy-disruption)).

### 10.3 Information visibility

Planned, not implemented. The Gameboard declares no public, private, or capability-gated information. Every Policy's View currently holds the same prices, Rules, and events ([04 §6](04_instantiated_game.md#6-policy-initialization)), so [INV-16](02_type_system.md#inv-16) is not enforced yet.

### 10.4 Available actions

An Actor acts through its capabilities ([§6](#6-entity-catalog)), the Markets it can access, and the Facilities and capacities it holds. Intents and their rules are in Type System [§3](02_type_system.md#3-intents-and-economic-actions).

### 10.5 Evaluation

Each Actor's objective is data, evaluated from the recorded game. It never affects the economy:

```
Objective = { description, criteria: [Criterion] }
Criterion = { metric: MetricRef, over: final | everyRound | cumulative, atLeast: number | MetricRef }
MetricRef = { entities: [EntityId], measure, negated? }
```

`measure` is one of `holding:<resource>`, `capacity:<capacity>`, `shortfall:<resource>`, `shortfallRounds:<resource>`, `payeeReceipts`, or `volume:<transformation>`, summed over the listed entities. `negated: true` expresses "at most", with a negative `atLeast`. Each criterion is reported as met, not met, or not evaluated ([06 §9.2](06_simulator.md#92-run-details)). System-level measures are planned.

## 11. Initialization Constraints

The Gameboard fixes every economic value: holdings, capacities, Market cost curves and reserves, Facility parameters, cycle entries, Rules in force, Policy strategies, named parameters, and scheme estimators. The configuration ([04 §2](04_instantiated_game.md#2-instantiation-configuration)) may choose only:

- seats, NPC Policies, and fallbacks, within each Actor's `seat`, `npc`, and `fallback` declarations.
- each Representation's weight (default `1`).
- the game seed. And
- the round count, within `roundRange`.

Gameboard `parameters` are fixed for a Gameboard. A simulation may override them for one run ([06 §3.1](06_simulator.md#31-configuration-inheritance)). Initial distributions, reserve and liquidity ranges, and per-instance Policy parameters are planned. Partial explicitness is resolved by instantiation ([04 §4](04_instantiated_game.md#4-aggregate-initialization)), with the consistency rule in [§12.4](#124-resolution-consistency).

## 12. Validation and Compilation

### 12.1 Load pipeline

`Premises.Load` decodes a premise's files. `Game.Premise.checkPremise` then validates them, in order:

1. Decode `instantiation.json` as the configuration.
2. Merge the other sections (a duplicate key fails) and decode the Gameboard, including its parameters and Policy catalog.
3. Decode the seat presets.
4. `validateGameboard gameboard config` (checks in [§12.2](#122-checks-implemented)–[§12.4](#124-resolution-consistency)).
5. Check every Policy's names against the Gameboard.
6. Check every seat preset's names against the Gameboard.

Load failures are `LoadError` values (`Premises.Load`): `Unreadable`, `NotAnObject`, `DuplicateKey`, and `Undecodable` (missing or ill-typed keys, unknown tags, a `roundRange` that fails `1 <= min <= max`). `checkPremise` returns `GameError` values: `Invalid` (validation errors) and `Unresolved` (a Policy or preset names something the Gameboard lacks).

```purescript
validateGameboard :: Gameboard -> InstantiationConfig -> Either (Array ValidationError) Gameboard
```

Validation is always against one configuration. The Gameboard has no separate "validated" type, so [INV-21](02_type_system.md#inv-21) is checked for the configuration supplied.

### 12.2 Checks implemented

`ValidationError` (`Board.Error`) enumerates each check:

| Constructor | Fails when |
|---|---|
| `UnknownReference kind name` | A name used in a Market, Facility, effect, cycle entry, Rule, scheme, Aggregate member, procedure weight, `npc` list, capability, procedure `authorizes`, objective metric or Measure, coverage fallback, `config.fallbacks` entry, or the `numeraire` does not exist. A capacity that is used but unheld, or held but unused, is reported this way too. |
| `DuplicateCapacityOwner` | A capacity, other than a producer's production capacity, has more than one owner. |
| `ProductionWithoutCapacity` | A `production` entry's Entity does not hold that capacity. |
| `MissingFallback` | A non-required Actor has no fallback. |
| `MissingObjective` | An Actor has no objective. |
| `ProcedureExceedsAuthority` | A procedure authorizes a capability its Aggregate does not hold. |
| `CoverageNotAggregate`, `SchemeTargetNotAggregate` | A coverage fallback or scheme target is not an Aggregate. |
| `RoundsOutOfRange` | `rounds` is outside `roundRange`. |
| `RequiredNotSelected` | A required Actor is not selected. |
| `ControllerNotAllowed` | A seat is given to an Actor whose `seat` is false, or an NPC Policy not in its `npc` list. |
| `IneligibleRepresentative` | A representing Actor is not in `eligibleActors`, or is not selected. |
| `ResolutionInconsistent` | Making a coverage Actor implicit changes stocks, capacities, procedure weights, or authority ([§12.4](#124-resolution-consistency)). |

Instantiation errors ([04 §10](04_instantiated_game.md#10-initialization-pipeline)) are reported as the same `ValidationError` list.

### 12.3 Not yet checked (planned)

These documented requirements are not validated:

- Units, Resource properties beyond `transferability`, CostFunction suitability, and Facility input and output accounting against Resource properties.
- Reconciliation of the recurring cycle and absence of inert accumulation ([INV-05](02_type_system.md#inv-05)). Starting reserve and capacity feasibility ([INV-27](02_type_system.md#inv-27)).
- Liabilities and issuance semantics for financial claims.
- That every required responsibility has an owner ([INV-25](02_type_system.md#inv-25)), beyond [§12.4](#124-resolution-consistency).
- Estimator and Representation weight ranges, and duplicate Representation adjustments ([INV-24](02_type_system.md#inv-24)).
- That capability lists are non-empty and that dependencies are declared ([INV-19](02_type_system.md#inv-19)).
- Scope definitions and information visibility ([§10.3](#103-information-visibility)).

### 12.4 Resolution consistency

For each Actor whose definition declares a coverage fallback, validation compares two configurations: the given one, and the same one with that Actor unselected. The two must agree on stocks (Entity accounts and Market reserves), capacities, procedure weights, and enacting authority (`Board.Gameboard.Reconcile`). A difference is `ResolutionInconsistent`.

Liabilities and recurring flows are not yet compared, and only these pairs are checked, not every pair of explicitness configurations. The check runs only when the other checks pass.

### 12.5 Compilation

Premises are authored with the typed builders of `schema` (`Schema.Build.*`), in the DSL under `packages/premises/src/Premises/`. `keyed`, `ref`, `matching`, `atLeast`, `objective`, and `collective` add nothing to the value they wrap. They are kept as the authoring vocabulary. `just _premises` runs `apps/cli`'s `Compile`, which checks each premise with `checkPremise` before writing it, and encodes it into the Meridian files, presets, and fixtures. The same step regenerates the capstone recording, with no model call. The compiled JSON is a build artifact, gitignored and generated: edit the DSL, never the JSON. The capstone recording is generated and gitignored like the rest.

## 13. Provenance and Versioning

- A run's manifest records `<id>@<version>` and the full initial World State ([04 §9](04_instantiated_game.md#9-runtime-state-and-lifecycle)), so Instantiated Games reference the exact version.
- Changes to Markets, Facilities, the cycle, Policies, Entities, or relationships are a new version. Version immutability ([INV-22](02_type_system.md#inv-22)) is a convention. Nothing hashes or compares Gameboard contents.
- The schema has no provenance fields. `description` and `dependency` are prose. Per-parameter provenance, linked to vertical reports, is planned.

## 14. Reference Gameboard

Meridian is the reference Gameboard, specified in [05 Part II](05_test_instance.md#part-ii-gameboard-configuration). Its files are listed in [§2](#2-gameboard-structure).

---

## 15. Summary

The Gameboard is a declarative, versioned set of files, checked against an Instantiation Configuration before play. It does not decide who is controlled by a human, what anyone does, or how events unfold.
