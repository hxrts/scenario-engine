# Premise Specification

## 1. Purpose

The kernel says what any World can be made of. A Premise describes part of an economy: an energy shock, a set of Actors, a set of Transformations. A PremiseComposition is an ordered list of Premises that together describe a set of possible Worlds, and a Run realizes one of them ([04](04_run.md)). This document says how Premises are authored and how a PremiseComposition is made of them. Premises are declarative: they hold no player assignments, Intents, or evolving state. Their checks run before any Run starts ([§12](#12-validation-and-compilation)).

**Premise files + composition file → validated PremiseComposition** (for one Instantiation Configuration)

**Validated PremiseComposition + Instantiation Configuration → initial World** ([04 §10](04_run.md#10-initialization-pipeline))

## 2. Premises and Compositions

A PremiseComposition is compiled to one composition directory and one file per Premise, under the premise root:

| File | Holds |
|---|---|
| `compositions/<id>/<version>/composition.json` | The composition's id and version, and its Premises in order, each with its version. |
| `compositions/<id>/<version>/instantiation.json` | The Instantiation Configuration ([04 §2](04_run.md#2-instantiation-configuration)). |
| `compositions/<id>/<version>/presets.json` | The seat-strategy presets ([07 §4.2](07_simulator.md#42-presets)), each a name and its `StrategyRules`. A composition writes its own. |
| `premises/<id>/<version>.json` | One Premise: its id, its version, and the sections it writes. Every section is optional. |
| `compositions/order.json` | The compositions in the order a launch offers them, each `<id>/<version>`; the first is the default. A server lists the ones it names in this order, then any others by name. |

The loader reads the composition, reads each Premise, composes them in order into the one document the Premise format decodes, and checks the result ([§12.1](#121-load-pipeline)).

A Premise's sections are the keys of the document:

| Section | Configures | How Premises combine |
|---|---|---|
| `numeraire`, `focus`, `horizon`, `roundRange`, `label`, `round` | The header ([§3](#3-premise)). | One value. Two Premises giving different values fail. |
| `resources`, `exchanges`, `transformations`, `actors`, `strategies`, `ruleKinds`, `parameters`, `groups` | Named things ([§5](#5-economic-structure), [§6](#6-actor-catalog), [§8](#8-strategy-catalog)). | By name. A name defined differently by two Premises fails, naming both; defined identically it is one definition. |
| `schemes`, `representations`, `templates` | Named things in a list ([§9](#9-representation-schemes), [§15](#15-authoring-laws-and-agreements)). | As named things. |
| `flowGates`, `rules`, `laws`, `circulation`'s lists | Things with no name ([§5.4](#54-recurring-cycle), [§5.5](#55-geography-access-and-rules), [§15](#15-authoring-laws-and-agreements)). | In order: a later Premise's items follow the earlier ones. An item an earlier Premise already wrote is that one item, not a second. |
| `extends` | What a Premise adds to something another defines, by name. | `actors.<name>.accounts` (new holdings; a holding given differently fails, naming both), `actors.<name>.capabilities`, `exchanges.<name>.access`. A name no Premise in the composition defines fails. |

Nothing is overwritten: there is no last-one-wins. A Premise adds Laws by writing them in its `rules` or `laws` sections, and a Premise may name what another Premise defines: every reference is checked on the composed result, so a name no Premise in the composition defines fails at load ([§12.2](#122-checks-implemented)).

Representations are never assigned in a Premise section. Assignments come from the configuration ([04 §5](04_run.md#5-representation-assignment)). The test fixtures are described in [05 Part IV](06_meridian.md#part-iv-playtest-fixtures).

## 3. Premise

The header names the scenario and its time scale, and is written by one of a composition's Premises. It says which forces are relevant, not how they resolve. Meridian's values are in [06 §6](06_meridian.md#6-premisecomposition-meridian).

- **Identity.** A stable identifier and a version ([§13](#13-provenance-and-versioning)) for the composition and for each Premise. An optional label names one configuration of the composition, and a Run records the composition id and this label as its game id.
- **Unit of account.** The numeraire is the Resource in which shares are paid.
- **Focus.** An optional Resource that the exchange views centre on.
- **Time.** The horizon in simulated years, and the supported round counts. A round range starts at one round or more, and its minimum is no greater than its maximum.
- **First round.** The first round is numbered 1 unless a Premise says otherwise.

### 3.1 Strategic tensions

A strategic tension among the forces a Premise models appears in it only through mechanically consequential data: Resources, capacities, Transformations, flows, Exchange access, or Rules.

## 4. Verticals

A vertical is an authoring concept. A vertical report compiles into explicit configuration in [§5](#5-economic-structure) to [§9](#9-representation-schemes). Meridian's sectors appear only as the Resources, Exchanges, Transformations, and Actors they produce ([06 §7](06_meridian.md#7-economic-verticals)).

## 5. Economic Structure

This section sets what can be held, traded, produced, and paid. Instantiation may change these values only through coverage merges ([04 §4.2](04_run.md#42-partitioning)).

### 5.1 Resources

Each Resource has:

- a transferability: Free, Restricted, or Non-transferable. It governs whether the Resource can move between Actors ([§10.1](#101-candidate-transaction-rules)).
- a reservable flag. A Transformation may reserve only a reservable Resource ([§12.2](#122-checks-implemented)).
- an optional decay rate per round ([§5.4](#54-recurring-cycle)).
- an optional category, a grouping that only the views read.

Capacity is a use of a Resource, not a kind of one: a Resource that Transformations draw on from its holder, or that a recurring flow reads as an amount held, rather than consume. A Premise usually declares one as a non-transferable Resource with no Exchange. It is held in Actor accounts and named by Transformation draws or the recurring cycle. To make it tradeable, the Premise gives it another transferability and an Exchange for it.

### 5.2 Exchanges

An Exchange trades a base Resource for a quote Resource. Its fields are:

- a cost curve: a linear cost function over a domain ([02 §5.1](02_kernel.md#51-cost-function)).
- **reserves**: the initial stock of each Resource. Reserves are the only liquidity; there is no separate liquidity parameter.
- **access**: the Actors allowed to trade. An Actor with access and no trade capability is an account, not an explicit Actor ([§6](#6-actor-catalog)).
- a position: the starting point on the cost curve, default 0.

A routed order names a base, a quote, a quantity, and a limit, without naming an Exchange. Limit prices and clearing are not Exchange fields ([02 §5.5](02_kernel.md#55-simultaneous-clearing)). Tariffs and access bans are Rules ([§5.5](#55-geography-access-and-rules)).

### 5.3 Transformations

A Transformation declares a recipe: inputs and outputs per unit ordered, draws held for the round and released at commit, an optional payment through a cost curve, order bounds, a delay, delayed flows, and whether the holder must sign each order. It holds no reserves and is never accounted as an Exchange. A Transformation may also:

- draw on Resources. A draw is held for the round and not consumed. It is taken from the ordering Actor's holding (a reservation, unavailable to its other actions in the round and not consumed) or from the holder of the Resource (`HolderOf`).
- charge a price per unit, read from a cost curve, paid in a Resource to an Actor.
- delay its effects by a number of rounds. With no delay, effects apply in the round the order settles.
- set a minimum order and a maximum total per Actor per round. A screened candidate Transaction that would exceed the maximum is rejected whole, and an order funded below the minimum is removed.
- require the holder of a drawn Resource to sign each order. Default: no.
- produce effects: delayed flows, each a per-unit creation or sink of a Resource settled after a delay, to a holder that defaults to the ordering Actor. An effect with `rounds` is reversed that many rounds later, by a spawned opposite settled Flow.

Transformations in one pool (those with a positive `HolderOf` draw of the same Resource) share what the Resource can supply. Those sharing a cost function share one uniform price. Every member escrows at the pool's ceiling (what the Resource can supply divided by the smallest per-unit draw, rounded down), so the escrowed price bounds the settled price. A Transformation in no pool escrows at the top of its cost domain. A Transformation's position on its cost curve is its total for the current round.

Production, consumption, procurement, issuance, destruction and investment are Transformations. They modify holdings only when inputs, draws and authority are available. A payment re-enters circulation at its payee. A Transformation may be shown as an exchange in the player interface, but its outputs are created only by its recipe, within what it can draw ([INV-07](02_kernel.md#inv-07)).

### 5.4 Recurring cycle

The recurring cycle holds the forms that the engine evaluates each round, from the opening World together with the round's batch result ([02 §8.2](02_kernel.md#82-funding-and-settlement)). It is empty by default. There are four kinds:

- **production**: output from a held capacity, delivered in the next round.
- **flows**: fixed payments per round.
- **shares**: a share of a per-round query over settled Flows, such as net sales or payments received, paid to a named recipient.
- **requirements**: operating needs consumed each round. A requirement's amount defaults to the Actor's purchases of the Resource that round. A penalty per unit short is charged against the Actor's incoming fixed flows, and defaults to 0.

All forms compile to one recurring rule, and a Resource's decay is one of them. The cycle is data in the World, one recurring flow per rule: a source, a destination, a Resource, an amount (fixed, an amount held, a fraction held, or measured by a stored query over the round's record), a shortfall effect, and whether the recipient's accrued offset reduces it. Each evaluation produces a settled Flow whose cause is the rule. A capacity's holding changes only through Transformation effects. A holding Rule limits only what can be drawn. Rates are per round ([§10.2](#102-temporal-structure)). Renewal and depreciation are written as laws ([§15.1](#151-laws-and-bilateral-agreements)).

### 5.5 Geography, access, and Rules

- Regions are separate Exchanges. Each commodity keeps one Resource, and a regional node is an Exchange with its access. There is no node hierarchy. Movement between regions uses flow gates and flows Rules.
- Flow gates mark a Resource whose movement is gated. A gate exempts parties that share an Exchange for that Resource. It can also require that a bundled order on the named Transformation carries the Transfer. Unless the gate exempts the parties, and unless such an order carries the Transfer, the Transfer is refused. The bundling is compared at screening, on the candidate Transaction's own quantities, so it is a static refusal rather than a law guard ([§12.4](#124-static-refusals)).
- Actor groups map an Actor to the Actor it counts as for flows Rules.
- Rule kinds are declared by the Premise. A kind is a levy or a block, governed by the destination, the origin, or either (which fixes the Rule's direction), or a reduction, governed by the Resource's holder. Each kind has display text. A Premise with no kinds has no Rules.
- Rules are the world laws a Premise authors as institutional conditions, each of a declared kind. A flows Rule scopes a governor, a counterparty, a direction, and at least one Resource it covers. Its effect is either a levy (a rate per unit or ad valorem on an Exchange, paid in a Resource to the destination, the governor, or a sink) or a block. A holding Rule reduces what can be drawn from a holder and changes no holding or flow. Rules in force at the start are listed with the game rules. A flows Rule applies to a Transfer between the groups it names, when it covers the Resource. A Block refuses the Transfer. A Levy charges it by its basis (per unit, or ad valorem on an Exchange's price), paid by the sender in the named Resource, to the recipient, to the governor, or to a sink. A flows Rule also applies to an Exchange trade, since a trade settles as pairs ([02 §5.5](02_kernel.md#55-simultaneous-clearing)) and each pair between two orders is a movement of the base from the seller to the buyer. A Block refuses the pair: neither the good nor the quote moves, each of its two orders is recorded as removed, `Blocked`, and the price is unchanged. A Levy is a third obligation of the pair, owed by the seller on the units moved and bound to the delivery, so one trade is charged once. A routed order pairs like any other, so a Rule cannot be passed by routing. The reserve has no jurisdiction, so no Rule applies to a pair with it: the engine does not infer the origin of what a reserve holds. The same check (`Laws.Engine.Screen.blockOf` and `leviesOn`) applies to a Transfer at screening, where its parties are known from the input, and to a pair once clearing has formed it. Levies do not enter clearing or escrow, so a levy never changes a price or a fill. A holding Rule removes its reduced units from what can be drawn from a holder. A Rule enacted during play needs the matching enactment capability ([§6](#6-actor-catalog)) and takes effect at the next round boundary ([02 §8.1](02_kernel.md#81-canonical-resolution-sequence)). Meridian's kinds are in [06 §13.2](06_meridian.md#132-trade-restrictions-and-the-tariff).

### 5.6 Limits

The schema has no separate constraints. Limits come from reserves, capacities, Exchange access, and Rules. A negative holding or reserve is a runtime failure ([04 §11](04_run.md#11-round-execution)), not a validation check.

## 6. Actor Catalog

The Actor catalog sets who holds what, who can act, and how each Actor is governed. Each Actor has accounts (its holdings, by Resource) and capabilities. An Actor has exactly one of a definition, which makes it an explicit Actor, or an aggregate block, which makes it an Aggregate ([§12.2](#122-checks-implemented)). Only a World that defines no Actors at all, such as a fixture's, may hold an Actor with neither, which is an account only.

The kinds of Capability are the authority to trade in an Exchange (`MayTrade`), transfer a Resource (`MayTransfer`), run a Transformation (`MayTransform`), or enact a Rule kind (`MayEnact`). Each is a pattern over Intents, and compiles to a `Holds` condition on the party ([02 §2.4](02_kernel.md#24-capability)). One matching function decides authorization for screening, the invariants and Strategies alike. It reads each Resource's transferability and the Rules in force. Enacting or repealing a Rule needs authority over its kind, and the acting Actor must govern it: the governor a flows Rule names, or the holder of the Resource a holding Rule reduces. A repeal names an existing Rule. Enacting a Rule equal to one already in force adds nothing at the boundary, so repeated enactments never accumulate, and one repeal removes it.

### 6.1 Projections

The catalog configures the three projections of an Actor ([02 §2.2](02_kernel.md#22-three-projections-of-an-actor)) separately:

- **Members.** Descriptive. Member lists may overlap, and membership confers nothing.
- **Accounts.** Each account is held by its Actor. A Resource drawn from its holder has one holder ([§12.2](#122-checks-implemented)).
- **Capabilities and procedures.** The authority to act, and what a collective procedure may grant.

### 6.2 Actor definitions

An Actor's definition sets:

- whether the Actor is required. A required Actor must be selected.
- whether a seat may control it. Default: yes.
- the non-playable Actor Strategies that may drive it. The list may be empty.
- its routine: an optional Strategy that runs beside whoever plays the seat, after them ([04 §6](04_run.md#6-strategy-initialization)). A routine names a Strategy of the catalog.
- its fallback, which covers it when unselected ([§6.5](#65-fallbacks)). Every Actor that is not required needs one ([§12.2](#122-checks-implemented)).
- an objective, a description with criteria ([§10.5](#105-evaluation)). Only its presence is checked.
- a description and a dependency, both prose. A dependency must be declared.

[INV-19](02_kernel.md#inv-19) requires a capability, a dependency, and a measurable objective for every Actor definition.

### 6.3 Aggregates

An Aggregate's block sets:

- a baseline Strategy ([§8](#8-strategy-catalog)).
- its members: the Actors it groups ([§6.1](#61-projections)).
- named procedures: collective rules, each with a weight for each member, a residual weight for the implicit members, a threshold, and the capabilities it authorizes. A procedure may not authorize a capability its Aggregate lacks ([§12.2](#122-checks-implemented)).

A collective procedure is an Aggregate's weighted-consent condition on the candidate Transactions made in its name. Its support is:

**Support = residualWeight + Σ weightᵢ over the members who signed**

A voted candidate Transaction executes if support exceeds the threshold, and lapses otherwise. The residual weight always counts, since publishing implies a signature. A member with no weight in a procedure still acts for the Aggregate there, without signing. How a non-explicit member counts is set at instantiation ([04 §4.1](04_run.md#41-resolving-the-three-projections)). A member influences a procedure only through its weight. A voted candidate Transaction never moves capabilities to a member or representative, and its authority belongs to the institutional Actor ([INV-03](02_kernel.md#inv-03)).

### 6.4 Overlap

Memberships may overlap. Holdings move between Actors only through instantiation's coverage merge ([04 §4.2](04_run.md#42-partitioning)).

### 6.5 Fallbacks

A fallback is the declared resolution of an Actor that is not selected. It is set in the Actor's definition, and the configuration may override it. The three fallbacks are:

| Fallback | Effect when the Actor is unselected |
|---|---|
| Coverage by an Aggregate | The Actor is merged into the named Aggregate, which must be an Aggregate ([§12.2](#122-checks-implemented)). |
| Strategy | The Actor stays and is driven by the named Strategy as a non-playable Actor. Its capabilities stay with it. |
| Omission | The Actor stays with no controller. |

The order in which fallbacks resolve is in [04 §3.2](04_run.md#32-fallback-resolution).

## 7. Structural Relationships

This section says how the schema links Actors and Resources. The schema has no relationship list. Its links are the fields above: dependency (Transformation inputs and outputs, requirements, flows, shares), competition (Exchange access), authority (Actor groups, Rules, enactment capabilities), and membership (Aggregate members and procedures). Descriptive relationships belong in the Premise report ([06](06_meridian.md)) and do not change outcomes.

## 8. Strategy Catalog

The Strategy catalog is a reference for what a Strategy can express. It maps each Strategy id to its `StrategyRules`: a list of rules and a sign clause, written in the shared decision language ([§8.1](#81-shared-interpreter)). These Strategies are declarative and fixed for a Run. The names their `StrategyRules` use are checked against the Premise ([§12.1](#121-load-pipeline)).

- Conditions read only the Strategy's View: prices, its own holdings, stored Representation factors, the round, the parameters, last round's events and records, and the Rules in force. Three expressions read what the Actor is committed to: `due` (what its fixed recurring flows take of a Resource each round), `scheduled` (what scheduled Flows still pending will deliver of a Resource to it), and `planned` (what the Transforms it has put forward this round consume of a Resource: those its seat's speech carried, and those its earlier clauses drafted; an order counts no more than its Transformation's maximum, since no more can settle). Clauses run in the order written, so a later clause can sell what an earlier one did not commit. A query of the Rules in force counts those a governor governs, of the kinds it names or of any when it names none. A condition `acting` holds when the acting Actor is the one it names, so one StrategyRules, such as a preset bound to every seat, can say what each seat it serves does.
- Rules can place exchange and Transformation orders, propose joint candidate Transactions that bundle transfers and transforms, and propose collective procedures. They can enact a declared Rule kind, repeal a Rule the Actor governs, mirror each Rule a condition finds back onto its governor, enter an agreement, or make a seeded choice among options.
- Sign clauses cover procedures, Transformations, named agreement templates, or everything addressed to the Actor. Each may be guarded by conditions.
- Exchange references name an Exchange by id, or select among the Exchanges the Actor can access that have a Resource as base: each one, the cheapest, or the costliest at the beginning-of-round price. Ties go to the first Exchange by id, and an Exchange with no price is not a candidate. A clause with selectors is instantiated once per Exchange picked, in id order. Several selectors range over the product of their picks. A selector that picks nothing instantiates nothing, which is not an error. A sign condition holds when any instantiation holds.

Strategies are referenced by an Actor's list of non-playable Actor Strategies, by an Aggregate's baseline, and by a Strategy fallback ([§6.5](#65-fallbacks)). Seat presets are bound outside the Premise ([07 §4](07_simulator.md#4-seat-strategies)).

### 8.1 Shared interpreter

All declarative Strategies run on one shared interpreter that drafts only Intents the View cannot already show to be invalid, so `StrategyRules` do not produce orders the engine is certain to reject. Signatures, funding and pool capacity stay engine-only. Joint and procedure Intents are exempt, since the engine checks them against their own authority. Human and AI Intents are not filtered: an invalid one is a recorded rejection. A seat preset and a non-playable Actor expressing the same rule therefore behave identically. A Strategy that enacts a Rule does so through `ChangeRule`.

## 9. Representation Schemes

A scheme is a template for a Representation of an Aggregate. Each scheme has:

- an identifier.
- **eligible Actors**: the Actors that may represent the Aggregate.
- a target: the Aggregate represented.
- a domain: the behaviour domain it adjusts.
- an observed Resource: the Resource whose purchases are observed. It defaults to the Resource named like the domain.
- an estimator: a reference and a shrinkage, with an optional bound on each deviation and an amplitude of sampling noise (default 0).

Which representatives are active, and their weights, are set at instantiation ([04 §5](04_run.md#5-representation-assignment)). The estimator formula and adjustment pipeline are in [02 §7.2](02_kernel.md#72-representation-and-estimators) to [§7.4](02_kernel.md#74-estimation-semantics). The rationale for the representation design is in [01](01_overview.md).

## 10. Rules of Play

These are the rules every Run plays by, not the institutional Rules of [§5.5](#55-geography-access-and-rules).

### 10.1 Candidate Transaction rules

A candidate Transaction binds Intents to the Actors asked to sign them: an author, Intents that settle together, the signers, and for a voted candidate Transaction the procedure used. Each Intent is executed by the signer whose resources or authority it uses. Publishing implies the author's signature. A signed candidate Transaction carries its collected signatures. It is visible to its signers. A candidate Transaction's identifier derives from its round, author and submission order, so recorded signatures replay deterministically.

The signature rules are:

- **Single-signer candidate Transaction:** a Trade, a gift Transfer, a Transform or a `ChangeRule`. Its author is its only signer.
- **Joint candidate Transaction:** needs every signer. It may bundle only Transfers, Transforms and `Enter` Intents. A joint candidate Transaction containing a Trade, a Route or a `ChangeRule` is rejected. Before its counterparties answer it is an offer, and a counterparty's signature is the acceptance.
- **Voted candidate Transaction:** executes when the signed weight exceeds the threshold ([§6.3](#63-aggregates)). It may carry any Intent its procedure authorizes, including a Trade. The restrictions on joint candidate Transactions do not apply to it.

A joint candidate Transaction is all-or-nothing: if its signed Transformation orders exceed what their pool can fill, every joint candidate Transaction in that pool is rejected together. A candidate Transaction whose signature rule is not met lapses, which is a recorded decline, and the round resolves normally. A counteroffer is a new candidate Transaction, signable next round. A fully signed candidate Transaction is escrowed at screening ([02 §8.2](02_kernel.md#82-funding-and-settlement)). Its Intents settle atomically, or the whole candidate Transaction is rejected and recorded, for example when escrow, transferability or delivery fails.

Escrow at screening takes each signed candidate Transaction's maximum cost: a buy its limit times quantity (or the Exchange's upper-bound price when unlimited), a sell its quantity, a Transformation order its pool ceiling, a reservation its per-unit draw times quantity. Joint candidate Transactions are the first rationing tier and are whole. Every other order is the next tier and scales, with one scale group per Actor ([02 §8.2](02_kernel.md#82-funding-and-settlement)). Validation records a valid candidate Transaction and its executed outcome separately: a valid Trade can still be partly filled or rejected at settlement.

Resource transferability ([§5.1](#51-resources)) and each Actor's capabilities ([§6](#6-actor-catalog)) govern what multi-signer candidate Transactions can move. These are joint and procedure candidate Transactions ([INV-10](02_kernel.md#inv-10)).

### 10.2 Temporal structure

The Premise fixes the horizon, the round range, and each Transformation's delay ([§3](#3-premise), [§5.3](#53-transformations)). Rates are per round, and nothing scales them to a length of time. The Premise declares no scheduled shocks. A shock is a named parameter that Strategies read ([§11](#11-initialization-constraints)).

### 10.3 Information visibility

The Premise declares no public, private, or capability-gated information. Every Strategy sees the same public state: the same prices, Rules, and events ([04 §6](04_run.md#6-strategy-initialization)). Beyond that, a View holds only what concerns its own Actor ([02 §4](02_kernel.md#4-strategies)).

### 10.4 Available actions

An Actor acts through its capabilities ([§6](#6-actor-catalog)), the Exchanges it can access ([§5.2](#52-exchanges)), and the Transformations and capacities it holds. Intents are the engine's only input from a Strategy, and every executed Intent came from a signed candidate Transaction ([INV-01](02_kernel.md#inv-01)). The six Intent kinds are:

- **Trade:** an order on an Exchange, with an optional limit price. Buys are positive and sells negative.
- **Route:** a Trade with its Exchange omitted, placed for a base and quote pair. It must carry a limit price ([02 §5.3](02_kernel.md#53-routing)).
- **Transfer:** a movement of a Resource from one Actor to another. Alone it is a gift or payment. Its sender must be a required signer.
- **Transform:** an execution of a declared Transformation. Its executing Actor must be a required signer.
- **`ChangeRule`:** enacts or repeals a Rule under the Actor's authority ([§6](#6-actor-catalog)). Enactment takes effect at the next round boundary.
- **`Enter`:** enters an agreement: it names a template, its arguments, and the Actor bound to each party ([§15.3](#153-agreements-in-a-premise-and-in-play)).

A Rule is either a flows Rule (its effect is a Levy or a Block, over a scope of groups and Resources) or a holding Rule (it reduces a Resource's drawable holding). The Premise declares each Rule's kind, which fixes its form and its governor ([§5.5](#55-geography-access-and-rules)).

### 10.5 Evaluation

Each explicit Actor's objective is data, evaluated from the recorded Run. It never affects the economy. An objective is a description with criteria. Each criterion compares a measure, summed over listed Actors, with a threshold. The threshold is a number or another measure. The criterion is evaluated on the final round by default, on every round, or cumulatively. It can be negated to express "at most".

The measures are a holding of a Resource, a shortfall in a Resource, the rounds with a shortfall, payee receipts, and the volume of a Transformation. Each is a measure of named Actors; the economy as a whole is read through the whole-economy views ([07 §11](07_simulator.md#11-experiments)).

## 11. Initialization Constraints

This section sets what a configuration may choose. The Premise fixes every economic value: holdings, capacities, Exchange cost curves and reserves, Transformation parameters, cycle entries, Rules in force, Strategies, named parameters, and scheme estimators. The Instantiation Configuration ([04 §2](04_run.md#2-instantiation-configuration)) may choose only:

- seats, non-playable Actor Strategies, and fallbacks, within each Actor's declared seat, non-playable Actor, and fallback options;
- each Representation's weight;
- the game seed;
- the round count, within the round range.

Named parameters are fixed for a Premise. Partial explicitness is resolved by instantiation ([04 §4](04_run.md#4-aggregate-initialization)), subject to [§12.3](#123-resolution-consistency).

## 12. Validation and Compilation

This section says what validation checks and how a PremiseComposition is built.

### 12.1 Load pipeline

Loading reads a composition's files and then checks them in this order:

1. decode the configuration;
2. compose the Premises in order ([§2](#2-premises-and-compositions)) and decode the composed document, including its parameters and Strategy catalog;
3. decode the seat presets;
4. validate the composition against the configuration ([§12.2](#122-checks-implemented) to [§12.3](#123-resolution-consistency));
5. check that every name a Strategy uses exists in the composition;
6. check that every name in a seat preset exists in the composition.

Load failures name the file or the decoding error: an unreadable file, a Premise that is not an object, a section no composition has, two Premises defining one thing differently (naming both and the thing), or a missing, ill-typed, or unknown key or tag. A checked PremiseComposition reports three kinds of error: validation errors, unresolved names (a Strategy or preset names something the Premise lacks), and an unknown preset.

Validation is always against one configuration. The composition has no separate validated type, so [INV-21](02_kernel.md#inv-21) is checked for the configuration supplied.

### 12.2 Checks implemented

These checks are the compile step of a PremiseComposition: loading compiles the composed Premises to a World, and a composition that fails them never reaches play. Per-round refusals are the round's own compile step ([§12.4](#124-static-refusals)).

Validation checks each of the following:

- Every name an Exchange, Transformation, effect, cycle entry, flow gate, Actor group, Rule, scheme, Aggregate member, procedure weight, Strategy reference, capability, procedure authorization, objective measure, or fallback refers to exists, and so do the focus and the numeraire. A held capacity that no draw, effect, or production entry uses is reported too.
- A Representation names a declared scheme.
- A Transformation's delay is not negative, and a flow's amount is not negative.
- A reservation names a reservable Resource.
- A Resource that a holder draw names, and that no production entry reads, has accounts at no more than one Actor. A holder draw names a Resource that some Actor holds.
- A Transformation has at most one holder draw.
- A production entry's Actor holds the Resource it reads.
- In an Actor definition, a non-required Actor has a fallback, and every Actor has an objective.
- A procedure authorizes only capabilities its Aggregate holds.
- A coverage fallback and a scheme target are Aggregates.
- The rounds of a launch are within the round range.
- A required Actor is selected.
- A seat is given only to an Actor that allows seats. A non-playable Actor Strategy is given only if it is in the Actor's list of non-playable Actor Strategies.
- A representing Actor is eligible for its scheme and is selected.
- Each Actor has exactly one of a definition or an aggregate block ([§6](#6-actor-catalog)).
- A defined Actor has at least one capability and declares a dependency ([INV-19](02_kernel.md#inv-19)).
- A scheme's estimator is in range: its reference and noise are not negative, its shrinkage is between 0 and 1, and its deviation bound is ordered. No two schemes adjust the same target and domain from the same observation and Actor ([INV-24](02_kernel.md#inv-24)).
- Every required responsibility has an owner: a required Actor is selected and every other has a fallback ([INV-25](02_kernel.md#inv-25)).
- Each Exchange opens within its cost domain with no negative reserve, and no Actor opens with a negative holding ([INV-27](02_kernel.md#inv-27)).
- A template names only what exists, spawns only templates that exist, and never spawns for its own round ([§15.3](#153-agreements-in-a-premise-and-in-play)).
- Resolution is consistent ([§12.3](#123-resolution-consistency)).

### 12.3 Resolution consistency

For each Actor whose definition declares a coverage fallback, validation compares two configurations: the given one, and the same one with that Actor unselected. The two must agree on stocks (Actor accounts and Exchange reserves), procedure weights, and enacting authority. A difference fails validation. The check runs only when the other checks pass.

### 12.4 Static refusals

The checks above run when a Premise loads. A further set is decided each round at screening, before any transaction exists: the compile step's static refusals. A static refusal is recorded as a `Rejected` event and is not a kernel value. They are:

- `DuplicateCandidateTransactionId`, and the references `UnknownActor`, `UnknownExchange`, `UnknownResource`, `UnknownTransformation`, `UnknownRule`, `UnknownProcedure` and `UnknownAgreement`.
- `MissingArgument` and `MissingParty`, for an `Enter` Intent.
- `RouteNeedsLimit`, and `OutOfScope` for a Rule outside its Actor's authority.
- `OnlyGoverningRepeal`, and `OutsideProcedureAuthority`.
- `JointBundlesOnlyTransfers`, `AboveMaximum`, and `MissingBundledOrder`.

Every other rejection is decided on a condition or by a solver in the settlement sequence ([§14](#14-compilation-to-the-kernel)). Each reason's kind is pinned by `reasonFate` in `packages/laws/test`.

## 12a. How Premises are made

A Premise is authored with typed builders in the Premise DSL, and the compiler writes it as a Premise file. Three operations reshape the authored source, and all three are read-only over it:

- Composition combines Premises in order into a PremiseComposition ([§2](#2-premises-and-compositions)). Premises not written for each other compose as long as they do not define one thing differently and every name they use is defined by some Premise in the composition. Two Premises defining one thing differently fail at load, with an error naming both Premises and the thing.
- Derivation changes the values of a base composition, through dials and named patches, to make a variant. It is not the derivation of a Run ([07 §8.1](07_simulator.md#81-read-api-and-run-operations)). A patch fails when its key is missing.
- Filtering keeps a subset of a composition. It fails if what is kept refers to what was dropped.

The compiler checks each composition ([§12.1](#121-load-pipeline)) before writing it. Each composition has its own directory, and each Premise its own file; a Premise id and version compiled by two compositions must be one Premise. Compiled JSON is generated, so edit the authored source and never the output.

## 13. Provenance and Versioning

- Changes to Exchanges, Transformations, the cycle, Strategies, or Actors are a new version. Version immutability is a convention: nothing hashes or compares Premise contents. A run's spec and manifest record the composition's identifier and version and each Premise's id and version, in order ([04 §9](04_run.md#9-runtime-state-and-lifecycle)); a derived Run replays only over the Premises and versions its source recorded.
- The schema has no provenance fields. Descriptions and dependencies are prose.

## 14. Compilation to the Kernel

The authored vocabulary lives with its compile step, in `packages/laws`. Each item compiles to kernel values ([02 §3](02_kernel.md#3-the-kernel)), and static refusals are decided first ([§12.4](#124-static-refusals)). The per-mechanism settlement sequence stays in `packages/laws`, and it calls the kernel in stated phases ([02 §8.1](02_kernel.md#81-canonical-resolution-sequence)). Every settled Flow posts through `Core.Kernel.Post`.

| Vocabulary | Kernel value | Written by |
|---|---|---|
| Trade, routed Trade | One transaction per cleared pair: the delivery of the base and the payment of the quote at the cleared rate, bound `Whole` to each other, with any levy bound to the delivery. Routing and clearing are solvers, and clearing forms the pairs. | `pairTransaction`, `Core.Kernel.Route`, `Core.Kernel.Clear` |
| Transform | A transaction of payment, input and output settled Flows. Outputs need the inputs held. Payment caps at what is held, with owed and moved recorded. | `transformTransaction` |
| Draw, reservation | A funding claim on the stock, held for the round and released at commit. It moves nothing. | `Laws.Engine.Fund` |
| Transfer | A transaction of one settled Flow that caps at what is held. | `payTransaction` |
| Joint candidate Transaction | Consent conditions on every signer; a transaction of its inputs, each bound whole to every other; rationing tier 0, whole. | `Laws.Engine.Screen`, `jointInputs`, `Laws.Engine.Fund` |
| Voted candidate Transaction | A `WeightedConsent` condition, evaluated at screening. | `Laws.Engine.Screen` |
| Signature, `ownerSigns` | `Consent` conditions, evaluated at screening. | `Laws.Engine.Screen` |
| Capabilities | `Holds` conditions. | `Laws.Authority` |
| Levy, Block (flows Rule) | A flow law: a Levy emits a settled Flow, a Block refuses. | `flowLaws` |
| Holding Rule | A stock law that withholds a portion. | `Laws.Query` (`drawable`) |
| Flow gate | A static refusal. | `Laws.Engine.Screen` |
| Decay (every-holder) | A time law on a stock, one settled Flow per holder to `Sink`. | `fractionLaw` |
| Other recurring flows | A time Law per rule, run in list order by the sequence; a created flow spawns a scheduled Flow. | `Laws.Engine.Circulate`, `timeLaw` |
| `ChangeRule` | A pending transaction due at the next boundary, whose obligation changes a law. | `ruleChange` |
| Scheduled Flow (`delivery`), delayed output or effect | A pending transaction with a `Created` or `Sink` settled Flow. A `rounds` reversal spawns the opposite settled Flow. | `delivery`, `Laws.Engine.Settle` |
| `Enter` | The named template's first step as a pending transaction, due next round, carrying the signers' consent, with the cause `OfAgreement`. | `entered`, `Core.Kernel.Consume` |
| Exchange, Transformation | An Exchange compiles to a standing transaction with a reserve stock and a cleared rate. A Transformation has no reserve. | `Laws.World` |

Limit prices and order minimums are decided by the sequence and the clearing solver rather than as `PriceWithin` and `QuantityAtLeast` conditions on a transaction ([02 §3.3](02_kernel.md#33-transactions-and-conditions)). A round record stores its inputs as authored vocabulary: each submitted signed candidate Transaction ([04 §11.1](04_run.md#111-round-record)).

## 15. Authoring Laws and Agreements

The kernel's expressive power is available without changing core. New designs are written at the DSL layer with `Schema.Build.Agreement`, whose builders construct transactions, clauses, guards and templates directly. They are exercised as test Premises in `packages/premises/test/Test/Premises/Agreements.purs`, separate from the shipped Premises.

### 15.1 Laws and bilateral agreements

- Depreciation is a recurring flow from every holder at a fraction held, to a sink. It compiles to a time law on a stock. At 10%, 20 becomes 18 and then 16.2.
- Wear from use is an action law on a stock, written with `onAction` and carried in the World's `laws`. It charges whoever runs the named Transformation, on the quantity run, in the same round.
- A tariff on a traded good is an action law on its Exchange, charged on each fill and paid to the governor. A Rule's levy, by contrast, applies to Transfers.
- A bilateral agreement binds one party's payment to the other's delivery and to a stock level, through `Performed … Whole` and `StockAtLeast` conditions. The delivery stands alone if the stock condition fails and the payment lapses. If the delivery cannot be made, nothing moves.
- A treaty binds the removal of a levy to a payment: a clause that changes a law, and a payment, each conditional on the other. Either both happen or neither does.

### 15.2 Multi-round agreements

Multi-round agreements are templates ([02 §3.4](02_kernel.md#34-successors)).

- A loan is a self-spawning template. Its `service` clause caps at what the borrower holds. On a shortfall it spawns the next `loan`, with interest on the frozen `Remainder`. If its stock condition fails, it spawns a `default` template that seizes collateral.
- A vesting schedule is a finite chain of templates.
- A renegotiable step adds a fresh `Consent` condition. A pending transaction has no signing input of its own, so the fresh consent holds only if the party signed the parent, and otherwise the step lapses.

### 15.3 Agreements in a Premise and in play

A Premise file may have a `templates` section, by name, and a `laws` section. Load validates both: every Actor, Resource and Exchange a template names must exist, every template a successor names must exist, and no template may spawn for its own round. A filter drops a template or a law naming something it dropped.

An agreement is entered by the Intent `enter`, which names a template, its arguments, and the Actor bound to each of its parties. It is carried by a candidate Transaction that every named party signs, and it may be bundled like a Transfer. On settlement the template's first step becomes pending, due next round, carrying the signers' consent. An unknown template, a missing argument or party, or a party who is not asked to sign is refused by screening; a party who is asked and does not sign lapses it. A View lists the templates that can be entered and what each needs. A StrategyRules drafts the Intent as any other and signs with the `agreements` sign rule, which names templates. Load refuses a Strategy that names a template the Premise lacks, or enters one without every argument and party it needs. The Intent's compilation is in [§14](#14-compilation-to-the-kernel).

### 15.4 Limits

- A due transaction in phase 0 settles at the round boundary. One in a later phase is rationed against that round's orders by its tier ([02 §3.4](02_kernel.md#34-successors)).
- Kernel values the shipped sequence does not read are described in [02 §3.3](02_kernel.md#33-transactions-and-conditions).
