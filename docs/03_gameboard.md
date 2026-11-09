# Gameboard Specification

## Terms

The authoring terms of this layer. The type-system terms are in the [Type System glossary](02_type_system.md#25-glossary), and the scenario-planning terms in [01 §1](01_overview.md#1-concept).

| Term | Definition |
|---|---|
| **Gameboard** | The authored economic world of a setting: Resources, Markets, Facilities, Entities, Policies, Representation schemes, and Rules. |
| **Premise** | A reusable piece a Setting combines, such as energy, shipping, or defence. |
| **Composition** | Building a Setting from generic definitions and Premises ([§12a](#12a-how-settings-are-made)). |
| **Derivation of a Setting** | Changing values of a base Setting, through dials and named patches, to make a variant ([§12a](#12a-how-settings-are-made)). It is not the derivation of a run ([06 Terms](06_simulator.md#terms)). |
| **Filtering** | Taking a subset of a Setting ([§12a](#12a-how-settings-are-made)). |
| **Vertical** | A researched sector, documented in a vertical report. No schema field records it ([§4](#4-verticals)). |
| **Fallback** | The declared resolution of an Actor definition that is not selected ([§6.5](#65-fallbacks)). |
| **Strategic tension** | A tension among the forces a setting models. It appears in the Gameboard only as mechanically consequential data ([§3.1](#31-strategic-tensions)). |

## 1. Purpose

The Gameboard is the authored economic world that a setting configures, and games are instantiated from it. It is declarative: it holds no player assignments, Intents, or evolving state. Its checks run before any game starts ([§12](#12-validation-and-compilation)).

**Gameboard files → Validated Gameboard** (for one Instantiation Configuration)

**Validated Gameboard + Instantiation Configuration → Initial World State** ([04 §10](04_instantiated_game.md#10-initialization-pipeline))

## 2. Gameboard Structure

This section lists what a setting is made of. A setting is a directory of section files. The loader merges them into one Gameboard and reads the configuration file as the Instantiation Configuration ([04 §2](04_instantiated_game.md#2-instantiation-configuration)).

| Section file | Configures |
|---|---|
| Setting header (`setting.json`) | Identity, unit of account, focus, horizon, round range, and label ([§3](#3-setting)). |
| `resources.json` | Resources ([§5.1](#51-resources)). |
| `markets.json` | Markets, flow gates, Entity groups, and Rule kinds ([§5.2](#52-exchange-markets), [§5.5](#55-geography-access-and-rules)). |
| `transformations.json` | Facilities ([§5.3](#53-facilities)). |
| `circulation.json` | The recurring cycle ([§5.4](#54-recurring-cycle)). |
| `entities.json` | The Entity catalog ([§6](#6-entity-catalog)). |
| `representations.json` | Representation schemes ([§9](#9-representation-schemes)). |
| `game-rules.json` | Named parameters and the Rules in force at the start ([§5.5](#55-geography-access-and-rules), [§11](#11-initialization-constraints)). |
| `policies.json` | The Policy catalog ([§8](#8-policy-catalog)). |
| `instantiation.json` | The Instantiation Configuration ([04 §2](04_instantiated_game.md#2-instantiation-configuration)). |

Seat-policy presets belong to the simulator ([06 §4.2](06_simulator.md#42-presets)). The test fixtures are described in [05 Part IV](05_test_instance.md#part-iv-playtest-fixtures).

Representations are never assigned in a Gameboard section. Assignments come from the configuration ([04 §5](04_instantiated_game.md#5-representation-assignment)).

## 3. Setting

The setting header names the scenario and its time scale. It says which forces are relevant, not how they resolve. Meridian's values are in [05 §6](05_test_instance.md#6-setting-meridian).

- **Identity.** A stable identifier and a version ([§13](#13-provenance-and-versioning)). An optional label names a test instance.
- **Unit of account.** The numeraire is the Resource in which shares are paid.
- **Focus.** An optional Resource that the market views centre on.
- **Time.** The horizon in simulated years, and the supported round counts. A round range starts at one round or more, and its minimum is no greater than its maximum.
- **First round.** The first round is numbered 1 unless the setting says otherwise.

### 3.1 Strategic tensions

A strategic tension, a scenario-planning concept ([01 §1](01_overview.md#1-concept)), appears in the Gameboard only through mechanically consequential data: Resources, capacities, Facilities, flows, Market access, or Rules.

## 4. Verticals

A vertical is an authoring concept. A vertical report compiles into explicit configuration in [§5](#5-economic-structure) to [§9](#9-representation-schemes). Meridian's sectors appear only as the Resources, Markets, Facilities, and Entities they produce ([05 §7](05_test_instance.md#7-economic-verticals)).

## 5. Economic Structure

This section sets what can be held, traded, produced, and paid. Instantiation may change these values only through coverage merges ([04 §4.2](04_instantiated_game.md#42-partitioning)).

### 5.1 Resources

Each Resource has:

- a **transferability**: Free, Restricted, or NonTransferable. It governs whether the Resource can move between Entities ([§10.1](#101-proposal-rules)).
- a **reservable** flag. A Facility may reserve only a reservable Resource ([§12.2](#122-checks-implemented)).
- an optional **decay** rate per round ([§5.4](#54-recurring-cycle)).
- an optional **category**, a grouping that only the views read.

A capacity is a use of a Resource, not a kind of one ([02 §2.5](02_type_system.md#25-glossary)). A setting usually declares one as a non-transferable Resource with no Market. It is held in Entity accounts and named by Facility draws or the recurring cycle. To make it tradeable, the setting gives it another transferability and a Market for it.

### 5.2 Exchange Markets

A Market trades a base Resource for a quote Resource. Its fields are:

- a **cost curve**: a linear CostFunction over a domain ([02 §5.1](02_type_system.md#51-costfunction)).
- **reserves**: the initial stock of each Resource. Reserves are the only liquidity; there is no separate liquidity parameter.
- **access**: the Entities allowed to trade. An Entity with access and no trade capability is an account, not an Actor ([§6](#6-entity-catalog)).
- a **position**: the starting point on the cost curve, default 0.

A routed order names a base, a quote, a quantity, and a limit, without naming a Market. Limit prices and clearing are not Market fields ([02 §3](02_type_system.md#3-intents-and-economic-actions)). Tariffs and access bans are Rules ([§5.5](#55-geography-access-and-rules)).

### 5.3 Facilities

A Facility (a Transformation) turns inputs into outputs per unit ordered. A Facility may also:

- **draw** on Resources. A draw is held for the round and not consumed. It is taken from the ordering Entity's holding or from the holder of the Resource ([02 §5.3](02_type_system.md#53-facilities-costed-transformations)).
- **charge** a price per unit, read from a cost curve, paid in a Resource to an Entity.
- **delay** its effects by a number of rounds. With no delay, effects apply in the round the order settles.
- set a **minimum** order and a **maximum** total per Entity per round.
- require the holder of a drawn Resource to **sign** each order. Default: no ([02 §5.3](02_type_system.md#53-facilities-costed-transformations)).
- produce **effects**: delayed flows per unit ordered, to a holder that defaults to the ordering Entity.

### 5.4 Recurring cycle

The recurring cycle holds the forms that the engine evaluates each round on the batch result ([02 §6](02_type_system.md#6-economic-circulation)). It is empty by default. There are four kinds:

- **production**: output from a held capacity, delivered in the next round.
- **flows**: fixed payments per round.
- **shares**: a share of a per-round query over legs, such as net sales or payments received, paid to a named recipient.
- **requirements**: operating needs consumed each round. A requirement's amount defaults to the Entity's purchases of the Resource that round. A penalty per unit short is charged against the Entity's incoming fixed flows, and defaults to 0.

All forms compile to one recurring rule ([02 §6](02_type_system.md#6-economic-circulation)), and a Resource's decay is one of them. A capacity's holding changes only through Facility effects. A holding Rule limits only what can be drawn. Rates are per round ([§10.2](#102-temporal-structure)). **Planned**: renewal and depreciation rules.

### 5.5 Geography, access, and Rules

- **Regions** are separate Markets. Each commodity keeps one Resource, and a regional node is a Market with its access. There is no node hierarchy. Movement between regions uses flow gates and flows Rules ([02 §5.4](02_type_system.md#54-flow-gates-and-flows-rules)).
- **Flow gates** mark a Resource whose movement is gated. A gate exempts parties that share a Market for that Resource. It can also require that a sender's Facility orders carry the Transfer ([02 §3.3](02_type_system.md#33-proposals-and-signatures)).
- **Entity groups** map an Entity to the Entity it counts as for flows Rules.
- **Rule kinds** are declared by the setting. A kind is a levy or a block, governed by the destination, the origin, or either (which fixes the Rule's direction), or a reduction, governed by the Resource's holder. Each kind has display text. A setting with no kinds has no Rules.
- **Rules** are institutional conditions, each of a declared kind. A flows Rule scopes a governor, a counterparty, a direction, and at least one Resource it covers. Its effect is either a levy (a rate per unit or ad valorem on a Market, paid in a Resource to the destination, the governor, or a sink) or a block. A holding Rule reduces what can be drawn from a holder and changes no holding or flow. Rules in force at the start are listed with the game rules. A Rule enacted during play needs the matching enactment capability ([02 §3](02_type_system.md#3-intents-and-economic-actions)). Meridian's kinds are in [05 §10](05_test_instance.md#10-aggregate-catalog).

### 5.6 Limits

The schema has no separate constraints. Limits come from reserves, capacities, Market access, and Rules. A negative holding or reserve is a runtime failure ([04 §11](04_instantiated_game.md#11-round-execution)), not a validation check.

## 6. Entity Catalog

The Entity catalog sets who holds what, who can act, and how each Entity is governed. Each Entity has accounts (its holdings, by Resource) and capabilities. An Entity should have exactly one of a definition, which makes it an Actor, or an aggregate block, which makes it an Aggregate. An Entity with neither is an account only.

A capability is the authority to trade in a Market, transfer a Resource, run a Facility, or enact a Rule kind. Each is a pattern over Intents ([02 §2.4](02_type_system.md#24-capability)).

### 6.1 Projections

The catalog configures the three projections of an Entity ([02 §2.2](02_type_system.md#22-three-projections-of-an-entity)) separately:

- **Members.** Descriptive. Member lists may overlap, and membership confers nothing.
- **Accounts.** Each account is held by its Entity. A Resource drawn from its holder has one holder ([§12.2](#122-checks-implemented)).
- **Capabilities and procedures.** The authority to act, and what a collective procedure may grant.

### 6.2 Actor definitions

An Actor's definition sets:

- whether the Actor is **required**. A required Actor must be selected.
- whether a seat may control it. Default: yes.
- the **NPC Policies** that may drive it. The list may be empty.
- its **fallback**, which covers it when unselected ([§6.5](#65-fallbacks)). Every Actor that is not required needs one ([§12.2](#122-checks-implemented)).
- an **objective**, a description with criteria ([§10.5](#105-evaluation)). Only its presence is checked.
- a description and a dependency. Both are prose, and the dependency is not checked.

[INV-19](02_type_system.md#inv-19) requires a capability, a dependency, and a measurable objective for every Actor. **Planned**: validation of capability lists and dependencies ([§12.3](#123-not-yet-checked-planned)).

### 6.3 Aggregates

An Aggregate's block sets:

- a **baseline Policy** ([§8](#8-policy-catalog)).
- its **members**: the Entities it groups ([§6.1](#61-projections)).
- named **procedures**: collective rules, each with a weight for each member, a residual weight, a threshold, and the capabilities it authorizes. A procedure may not authorize a capability its Aggregate lacks ([§12.2](#122-checks-implemented)).

### 6.4 Overlap

Memberships may overlap. Holdings move between Entities only through instantiation's coverage merge ([04 §4.2](04_instantiated_game.md#42-partitioning)).

### 6.5 Fallbacks

A fallback is the declared resolution of an Actor that is not selected. It is set in the Actor's definition, and the configuration may override it. The three fallbacks are:

| Fallback | Effect when the Actor is unselected |
|---|---|
| Coverage by an Aggregate | The Actor is merged into the named Aggregate, which must be an Aggregate ([§12.2](#122-checks-implemented)). |
| Policy | The Entity stays and is driven by the named Policy as an NPC. Its capabilities stay with it. |
| Omission | The Entity stays with no controller. |

The order in which fallbacks resolve is in [04 §3.2](04_instantiated_game.md#32-fallback-resolution).

## 7. Structural Relationships

This section says how the schema links Entities and Resources. The schema has no relationship list. Its links are the fields above: dependency (Facility inputs and outputs, requirements, flows, shares), competition (Market access), authority (Entity groups, Rules, enactment capabilities), and membership (Aggregate members and procedures). Descriptive relationships belong in the setting report ([05](05_test_instance.md)) and do not change outcomes. **Planned**: a typed relationship list.

## 8. Policy Catalog

The Policy catalog is a reference for what a Policy can express. It maps each Policy id to a Strategy, a list of rules and a sign clause, written in the shared decision language ([02 §4](02_type_system.md#4-policies)). Strategies are declarative and fixed for a game. The names a Strategy uses are checked against the Gameboard ([§12.1](#121-load-pipeline)).

- **Conditions** read only the Policy's View: prices, its own holdings, stored Representation factors, the round, the parameters, last round's events and records, and the Rules in force.
- **Proposals** can place market and Facility orders, propose joint Proposals that bundle transfers and transforms, and propose collective procedures. They can enact a declared Rule kind, repeal a Rule the Entity governs, mirror each Rule a condition finds back onto its governor, or make a seeded choice among options.
- **Sign clauses** cover procedures, Facilities, or everything addressed to the Entity. Each may be guarded by conditions.
- **Market references** name a Market by id, or select among the Markets the Entity can access that have a Resource as base: each one, the cheapest, or the costliest at the beginning-of-round price. Ties go to the first Market by id, and a Market with no price is not a candidate. A clause with selectors is instantiated once per Market picked, in id order. Several selectors range over the product of their picks. A selector that picks nothing instantiates nothing, which is not an error. A sign condition holds when any instantiation holds.

Policies are referenced by an Actor's NPC list, by an Aggregate's baseline, and by a Policy fallback ([§6.5](#65-fallbacks)). Seat presets are bound outside the Gameboard ([06 §4](06_simulator.md#4-seat-policies)).

**Planned**, not in the schema: parameter ranges and defaults, and declared behavioural domains for Aggregates.

## 9. Representation Schemes

A scheme is a template for a Representation of an Aggregate. Each scheme has:

- an identifier.
- **eligible Actors**: the Actors that may represent the Aggregate.
- a **target**: the Aggregate represented.
- a **domain**: the behaviour domain it adjusts.
- an **observed Resource**: the Resource whose purchases are observed. It defaults to the Resource named like the domain.
- an **estimator**: a reference and a shrinkage, with an optional bound on each deviation and an amplitude of sampling noise (default 0).

Which representatives are active, and their weights, are set at instantiation ([04 §5](04_instantiated_game.md#5-representation-assignment)). The estimator formula and adjustment pipeline are in [02 §7.2](02_type_system.md#72-representation-and-estimators) to [§7.4](02_type_system.md#74-estimation-semantics). The rationale for the representation design is in [01](01_overview.md).

## 10. Game Rules

These are the rules of the game itself, not the institutional Rules of [§5.5](#55-geography-access-and-rules).

### 10.1 Proposal rules

Resource transferability ([§5.1](#51-resources)) and each Entity's capabilities ([§6](#6-entity-catalog)) govern what multi-signer Proposals can move. These are joint and procedure Proposals ([INV-10](02_type_system.md#inv-10)). **Planned**: a switch that restricts multi-signer Proposals by Resource or Entity category.

### 10.2 Temporal structure

The Gameboard fixes the horizon, the round range, and each Facility's delay ([§3](#3-setting), [§5.3](#53-facilities)). Rates are per round. **Planned**: a round-to-time mapping, a per-round duration, and scaling annual rates to round length ([INV-28](02_type_system.md#inv-28)). The Gameboard declares no scheduled shocks. A shock is a named parameter that Policies read ([§11](#11-initialization-constraints)).

### 10.3 Information visibility

The Gameboard declares no public, private, or capability-gated information. Every Policy's View currently holds the same prices, Rules, and events ([04 §6](04_instantiated_game.md#6-policy-initialization)), so [INV-16](02_type_system.md#inv-16) is not enforced. **Planned**: visibility declarations and scope definitions.

### 10.4 Available actions

An Actor acts through its capabilities ([§6](#6-entity-catalog)), the Markets it can access ([§5.2](#52-exchange-markets)), and the Facilities and capacities it holds. The Intents are in [02 §3](02_type_system.md#3-intents-and-economic-actions).

### 10.5 Evaluation

Each Actor's objective is data, evaluated from the recorded game. It never affects the economy. An objective is a description with criteria. Each criterion compares a measure, summed over listed Entities, with a threshold. The threshold is a number or another measure. The criterion is evaluated on the final round by default, on every round, or cumulatively. It can be negated to express "at most".

The measures are a holding of a Resource, a shortfall in a Resource, the rounds with a shortfall, payee receipts, and the volume of a Facility. **Planned**: system-level measures.

## 11. Initialization Constraints

This section sets what a configuration may choose. The Gameboard fixes every economic value: holdings, capacities, Market cost curves and reserves, Facility parameters, cycle entries, Rules in force, Policy strategies, named parameters, and scheme estimators. The Instantiation Configuration ([04 §2](04_instantiated_game.md#2-instantiation-configuration)) may choose only:

- seats, NPC Policies, and fallbacks, within each Actor's declared seat, NPC, and fallback options;
- each Representation's weight;
- the game seed;
- the round count, within the round range.

Named parameters are fixed for a Gameboard. **Planned**: initial distributions, and reserve and liquidity ranges. Partial explicitness is resolved by instantiation ([04 §4](04_instantiated_game.md#4-aggregate-initialization)), subject to [§12.4](#124-resolution-consistency).

## 12. Validation and Compilation

This section says what validation checks and how a setting is built.

### 12.1 Load pipeline

Loading decodes a setting's files and then checks them in this order:

1. decode the configuration;
2. merge the other sections (a duplicate key fails) and decode the Gameboard, including its parameters and Policy catalog;
3. decode the seat presets;
4. validate the Gameboard against the configuration ([§12.2](#122-checks-implemented) to [§12.4](#124-resolution-consistency));
5. check that every name a Policy uses exists in the Gameboard;
6. check that every name in a seat preset exists in the Gameboard.

Load failures name the file or the decoding error: an unreadable file, a section that is not an object, a duplicate key, or a missing, ill-typed, or unknown key or tag. A checked setting reports three kinds of error: validation errors, unresolved names (a Policy or preset names something the Gameboard lacks), and an unknown preset.

Validation is always against one configuration. The Gameboard has no separate validated type, so [INV-21](02_type_system.md#inv-21) is checked for the configuration supplied.

### 12.2 Checks implemented

Validation checks each of the following:

- Every name a Market, Facility, effect, cycle entry, flow gate, Entity group, Rule, scheme, Aggregate member, procedure weight, Policy reference, capability, procedure authorization, objective measure, fallback, focus, or the numeraire refers to exists. A held capacity that no draw, effect, or production entry uses is reported too.
- A Representation names a declared scheme.
- A Facility's delay is not negative, and a flow's amount is not negative.
- A reservation names a reservable Resource.
- A Resource that a holder draw names, and that no production entry reads, has accounts at no more than one Entity. A holder draw names a Resource that some Entity holds.
- A Facility has at most one holder draw.
- A production entry's Entity holds the Resource it reads.
- A non-required Actor has a fallback. An Actor has an objective.
- A procedure authorizes only capabilities its Aggregate holds.
- A coverage fallback and a scheme target are Aggregates.
- The rounds of a launch are within the round range.
- A required Actor is selected.
- A seat is given only to an Actor that allows seats. An NPC Policy is given only if it is in the Actor's NPC list.
- A representing Actor is eligible for its scheme and is selected.
- Resolution is consistent ([§12.4](#124-resolution-consistency)).

### 12.3 Not yet checked (planned)

**Planned**, not validated:

- Units, Resource properties beyond transferability, CostFunction suitability, and Facility input and output accounting against Resource properties.
- Reconciliation of the recurring cycle, and absence of inert accumulation ([INV-05](02_type_system.md#inv-05)). Starting reserve and capacity feasibility ([INV-27](02_type_system.md#inv-27)).
- Liabilities and issuance semantics for financial claims.
- That every required responsibility has an owner ([INV-25](02_type_system.md#inv-25)), beyond [§12.4](#124-resolution-consistency).
- Estimator and Representation weight ranges, and duplicate Representation adjustments ([INV-24](02_type_system.md#inv-24)).
- That capability lists are non-empty and dependencies are declared ([INV-19](02_type_system.md#inv-19)).
- That each Entity has exactly one of a definition or an aggregate block ([§6](#6-entity-catalog)).
- Objective measures that are system-level ([§10.5](#105-evaluation)).
- That each strategic tension connects to consequential data ([§3.1](#31-strategic-tensions)).
- Scope definitions and information visibility ([§10.3](#103-information-visibility)).

### 12.4 Resolution consistency

For each Actor whose definition declares a coverage fallback, validation compares two configurations: the given one, and the same one with that Actor unselected. The two must agree on stocks (Entity accounts and Market reserves), procedure weights, and enacting authority. A difference fails validation. The check runs only when the other checks pass.

**Planned**: comparing liabilities and recurring flows, and checking every pair of explicitness configurations rather than only these pairs.

## 12a. How settings are made

A setting is authored with typed builders in the setting DSL, and the compiler writes it as the setting files. Three operations build a setting from the authored source, and all three are read-only over that source:

- **Composition** combines generic definitions and Premises. Definitions name no setting. A key that two Premises both define fails.
- **Derivation** changes the values of a base setting, through dials and named patches. A patch fails when its key is missing.
- **Filtering** keeps a subset of a setting. It fails if what is kept refers to what was dropped.

The compiler checks each setting ([§12.1](#121-load-pipeline)) before writing it. Each compiled setting has its own directory and its own presets file. Compiled JSON is generated, so edit the authored source and never the output.

## 13. Provenance and Versioning

- Changes to Markets, Facilities, the cycle, Policies, or Entities are a new version. Version immutability ([INV-22](02_type_system.md#inv-22)) is a convention: nothing hashes or compares Gameboard contents. A run's manifest records the Gameboard's identifier and version ([04 §9](04_instantiated_game.md#9-runtime-state-and-lifecycle)).
- The schema has no provenance fields. Descriptions and dependencies are prose. **Planned**: per-parameter provenance, linked to vertical reports.
