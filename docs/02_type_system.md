# Type System Specification

## 1. Purpose

This layer defines the economic types, invariants, numerics and round sequence through which explicit Actors, Aggregates and implicit populations act in one deterministic economy. It is independent of any setting, resource taxonomy or conflict. It stays small by giving existing primitives precise meaning rather than adding a primitive for each phenomenon. The engine is pure: every impure act is a named effect ([§8.4](#84-effects-and-handlers)).

The game is in [01](01_overview.md), settings in [03](03_gameboard.md), execution in [04](04_instantiated_game.md), the reference setting in [05](05_test_instance.md), and the simulator in [06](06_simulator.md). This document owns the type-system terms in [§2.5](#25-glossary).

## 2. Core Ontology

This section names the types the layer is built from. Each is defined in the glossary ([§2.5](#25-glossary)) and specified in the section named: Entity and its three projections ([§2.1](#21-entity), [§2.2](#22-three-projections-of-an-entity)); Resource and Account ([§2.3](#23-resource)); Capability ([§2.4](#24-capability)); Market, CostFunction and Transformation, which is a Facility when costed ([§5](#5-cost-functions-markets-and-transformations)); Aggregate and Representation ([§7](#7-aggregates-and-representation)); Policy ([§4](#4-policies)); Intent, Proposal, Signature and Lapse ([§3](#3-intents-and-economic-actions)); and World State, Round record and Manifest ([§8](#8-world-state-and-round-resolution)).

### 2.1 Entity

An Entity is the one economic record that every participant, explicit or implicit, carries. It has an identifier, accounts, capabilities and a kind.

Its kind is an **Actor**, driven by a Controller (a Seat supplied from outside the setting, or an NPC the setting defines), or an **Aggregate**, with a baseline Policy, descriptive members and collective procedures ([§2.2](#22-three-projections-of-an-entity)). Actors and Aggregates own accounts and act in the same Markets and Transformations. They differ only in how their decisions are produced. The reference instance is in [05 §9](05_test_instance.md#9-actor-catalog) and [§10](05_test_instance.md#10-aggregate-catalog).

### 2.2 Three projections of an Entity

Membership, economic ownership and collective decision-making are distinct relations. The layer keeps them separate rather than letting "Aggregate" carry all three.

| Projection | Rule |
|---|---|
| **Membership** | Descriptive. It may overlap with other Aggregates and confers no ownership. A member with no weight in a procedure still acts for the Aggregate there, without signing. |
| **Account ownership** | Every account and behavioral responsibility has exactly one owner ([INV-08](#inv-08)). |
| **Collective procedures and capabilities** | Authority belongs to the institutional Entity or a Rule, never to members by virtue of membership ([INV-03](#inv-03)). |

A collective procedure is a weighted-signature rule: member weights, a residual weight for the implicit members, a threshold, and the capabilities a passed procedure Proposal may use ([§3.3](#33-proposals-and-signatures)). Support is:

**Support = residualWeight + Σ weightᵢ over the members who signed**

The Proposal executes if support exceeds the threshold, and lapses otherwise. The residual weight always counts, since publishing implies a signature. How a non-explicit member counts is set at instantiation ([04 §4.1](04_instantiated_game.md#41-resolving-the-three-projections)).

A member influences a procedure only through its weight. Membership never lets several Aggregates own the same assets.

### 2.3 Resource

A Resource is an identified economic quantity with a unit and declared behavior. Its transferability is **Free** (it moves on its holder's signature alone), **Restricted** (each Transfer needs `MayTransfer`, [§2.4](#24-capability)), or **NonTransferable**. A category is a setting's own grouping, which the engine never reads. A Resource that no Transformation produces or consumes is conserved, a consequence of the declared Transformations.

Not everything economically meaningful is a Resource. Market access and institutional authority are state or Rules attached to Entities, Markets and Transformations. A [capacity](#25-glossary) is a Resource that is used rather than consumed. Holdings are distinct from capabilities: influence over a Rule does not let an Entity spend, transfer or create what the Rule affects.

### 2.4 Capability

A Capability is a pattern over Intents stating what an Entity may do: trade a Market (`MayTrade`), transfer a Resource (`MayTransfer`), execute a Transformation (`MayTransform`), or enact a Rule kind the setting declares (`MayEnact`).

One matching function decides authorization for screening, the invariants and Policies alike. It reads each Resource's transferability and the Rules in force. Enacting or repealing a Rule needs authority over its kind, and the acting Entity must govern it: the governor a flows Rule names, or the holder of the Resource a holding Rule reduces. A repeal names an existing Rule ([§3](#3-intents-and-economic-actions)).

Institutional capabilities stay with the institutional Entity or with a declared Rule. If an institution is not played explicitly, its capabilities stay with its Aggregate's procedures and never move to a population Aggregate. Membership, Representation and signing a procedure Proposal never transfer capabilities to a member or representative.

### 2.5 Glossary

This table is the one place the type-system terms are defined. Scenario terms are in [01 §1](01_overview.md#1-concept), and the other layers' terms in their `Terms` tables ([03](03_gameboard.md#terms), [04](04_instantiated_game.md#terms), [06](06_simulator.md#terms), [07](07_architecture.md#terms)).

| Term | Definition |
|---|---|
| **Account** | A quantity of a Resource owned by exactly one Entity or Market. |
| **Actor** | An Entity represented explicitly, driven by a Controller ([§2.1](#21-entity)). |
| **Adjustment factor** | The stored multiplier that scales an Aggregate's baseline in the following round ([§7.4](#74-estimation-semantics)). |
| **Aggregate** | An Entity holding the residual accounts and baseline behavior of an implicit population or institution ([§7](#7-aggregates-and-representation)). |
| **Behavior domain** | The part of an Aggregate's baseline that a Representation may adjust ([§7.2](#72-representation-and-estimators)). |
| **Capacity** | A Resource that Transformations draw on from its holder, or that a recurring flow reads as an amount held, rather than consume. |
| **Capability** | A pattern over Intents stating what an Entity may do ([§2.4](#24-capability)). |
| **Cause** | What produced a settlement: a Proposal's Intent, a Rule kind, or a recurring-flow rule. |
| **Collective procedure** | An Aggregate's weighted-signature rule for Proposals made in its name ([§2.2](#22-three-projections-of-an-entity)). |
| **Controller** | What drives an Actor: a Seat or an NPC Policy. |
| **CostFunction** | A linear cumulative cost curve that prices quantity without creating it ([§5.1](#51-costfunction)). |
| **Delayed flow** | A per-unit creation or sink of a Resource for its holder, settled after a delay ([§5.3](#53-facilities-costed-transformations)). |
| **Delivery** | A scheduled change to a holding, applied at the start of the round it is due. |
| **Display kind** | Quantity, Price or Ratio. Each has one display rule ([§9.1](#91-numerics-and-randomness)). |
| **Draw** | A per-unit quantity a Transformation holds for one round and releases at commit ([§5.3](#53-facilities-costed-transformations)). |
| **Draw key** | The semantic name of one random draw ([§9.1](#91-numerics-and-randomness)). |
| **Effect** | A declared impure operation, interpreted by a handler ([§8.4](#84-effects-and-handlers)). Randomness is not an effect. |
| **Entity** | The economic record of a participant, Actor or Aggregate ([§2.1](#21-entity)). |
| **Escrow** | The hold placed at screening on a signed Proposal's maximum cost, released at commit if unused ([§8.2](#82-funding-and-settlement)). |
| **Estimator** | The configured function that turns a representative's observed behavior into an adjustment factor ([§7.4](#74-estimation-semantics)). |
| **Facility** | The player-facing name for a costed Transformation, accounted as a Transformation and never as a Market ([§5.3](#53-facilities-costed-transformations)). |
| **Flow** | One leg of a settlement: a quantity of a Resource owed from one endpoint to another, and how much moved. Legs settled together form a Coupled flow ([§5.5](#55-simultaneous-clearing)). |
| **Flow gate** | A condition on Transfers of a Resource: which parties are exempt and which bundled order must carry them ([§5.4](#54-flow-gates-and-flows-rules)). |
| **Game seed** | The one seed per game instance, fixed at initialization, from which every in-world draw derives ([§9.1](#91-numerics-and-randomness)). |
| **Group** | The Entity that another Entity counts as when a flows Rule applies to it. |
| **Holder** | The Entity a holding Rule or `HolderOf` draw answers to: the one with a positive holding, else the first in id order with an account. |
| **Holding** | A quantity of a Resource held by an Entity or in a Market's reserves. |
| **Intent** | A typed action: `Trade`, `Route`, `Transfer`, `Transform` or `ChangeRule` ([§3](#3-intents-and-economic-actions)). |
| **Joint Proposal** | A Proposal that needs a signature from someone besides its author. Before the others answer, it is an offer ([§3.3](#33-proposals-and-signatures)). |
| **Lapse** | The outcome of a Proposal whose signature rule is unmet by the end of Sign. It is a decline, not an error ([§3.3](#33-proposals-and-signatures)). |
| **Market** | A mechanism that exchanges the Resources in its reserves and clears at a uniform batch price ([§5.2](#52-exchange-markets)). |
| **Money** | The numeraire Resource, the unit of account for views. The engine does not read it: a payment or levy names its own Resource. |
| **NPC** | An Actor whose Policy is part of the setting. |
| **Observation** | The admissible proposed buy quantity a Representation reads from its Actor's validated Proposals ([§7.3](#73-representation-pipeline)). |
| **Policy** | Anything that produces Intents from the View of the Entity it drives ([§4](#4-policies)). |
| **Pool** | The Transformations with a positive `HolderOf` draw of the same Resource, which share what that Resource can supply ([§5.3](#53-facilities-costed-transformations)). |
| **Player seed** | The seed that keys the draws of seat Policies. It is never part of World State ([§9.1](#91-numerics-and-randomness)). |
| **Procedure Proposal** | A Proposal an Aggregate authors under one of its collective procedures ([§2.2](#22-three-projections-of-an-entity)). |
| **Proposal** | A message carrying Intents and signers. It executes only when its signature rule is met ([§3.3](#33-proposals-and-signatures)). |
| **Recurring flow** | One rule of the recurring cycle: a source, a destination, a Resource, an amount and a shortfall effect ([§6](#6-economic-circulation)). |
| **Representation** | A link through which an explicit Actor's sampled behavior adjusts an Aggregate's baseline ([§7.2](#72-representation-and-estimators)). |
| **Reservation** | A draw from the ordering Entity's own holding: unavailable to its other actions in the round, and not consumed ([§5.3](#53-facilities-costed-transformations)). |
| **Residual** | The accounts and behavior of a population left in an Aggregate once explicit Actors' are assigned ([§7.1](#71-aggregate-accounts)). |
| **Resource** | An economic quantity with a unit and declared behavior ([§2.3](#23-resource)). |
| **Rule** | An institutional condition the engine interprets: a flows Rule (a levy or a block) or a holding Rule (reduces what can be drawn) ([§5.4](#54-flow-gates-and-flows-rules)). |
| **Seat** | An Actor whose Policy is supplied from outside the setting. Swapping the implementation never changes the Actor ([INV-30](#inv-30)). |
| **Shortfall** | `moved < owed` on a leg: an amount owed but not delivered, because the source's holdings fell short. |
| **Signature** | A signer's endorsement of a Proposal. The author's signature is implicit in publishing ([§3.3](#33-proposals-and-signatures)). |
| **Snapshot** | The World State a round ends with. The next round's decisions are all made on it ([§8.1](#81-canonical-resolution-sequence)). |
| **Strategy** | The declarative rules a Policy runs on the shared interpreter ([§4.4](#44-shared-decision-language-interpreter)). Only declarative Policies have one. |
| **Transformation** | A declared recipe that changes quantities, optionally priced by a CostFunction and limited by what it can draw ([§5.3](#53-facilities-costed-transformations)). |
| **Uniform batch price** | The single price at which a Market clears its round batch ([§5.5](#55-simultaneous-clearing)). |
| **View** | What a Policy sees of the beginning-of-round Snapshot ([§4](#4-policies)). |
| **World State** | The complete economic state at a round boundary, including the game seed and the round number ([§8](#8-world-state-and-round-resolution)). |

## 3. Intents and Economic Actions

Intents are the engine's only input, and every executed Intent came from a signed Proposal ([INV-01](#inv-01)). The engine never authors or rewrites an Intent, and never knows how one was produced ([§4](#4-policies)). It checks authority and signatures and keeps the books balanced.

The five Intent kinds are:

- **Trade:** an order on a Market, with an optional limit price. Buys are positive and sells negative.
- **Route:** a Trade with its Market omitted, placed for a base and quote pair. It must carry a limit price ([§5.2](#52-exchange-markets)).
- **Transfer:** a movement of a Resource from one Entity to another. Alone it is a gift or payment. Its sender must be a required signer.
- **Transform:** an execution of a declared Transformation. Its executing Entity must be a required signer.
- **ChangeRule:** enacts or repeals a Rule under the Entity's authority ([§2.4](#24-capability)). Enactment takes effect at the next round boundary.

A Rule is either a flows Rule (its effect is a Levy or a Block, over a scope of groups and Resources) or a holding Rule (it reduces a Resource's drawable holding). The setting declares each Rule's kind, which fixes its form and its governor ([03 §5.5](03_gameboard.md#55-geography-access-and-rules)).

### 3.1 Intent validation

Validation has two stages. **Authorization** checks the signer, the references, and the capabilities, Market access, transferability, Rule scope and order bounds that apply ([§2.4](#24-capability)). **Economic feasibility** checks unreserved resources, Market reserves, what a pool can draw, and joint demand across the round. The engine records a **valid Proposal** and its **executed outcome** separately: a valid Trade can still be partly filled or rejected at settlement.

### 3.2 Infeasible actions

Infeasible actions are rejected or partially executed by deterministic rules. The engine never invents resources or authority to make an action succeed.

- A pro-rata share is executed when an Entity's Proposals jointly exceed its opening resources, and when demand exceeds supply or what can be drawn.
- Indivisible remainders go to the largest remainders, ties in canonical id order, never by submission order ([§9.1](#91-numerics-and-randomness)).
- Orders whose limit price is violated at the batch price are removed at clearing ([§5.5](#55-simultaneous-clearing)).
- A multi-signer Proposal settles atomically, or is rejected and recorded as a unit.

### 3.3 Proposals and signatures

A Proposal binds Intents to the Entities asked to sign them: an author, Intents that settle together, the signers, and for a procedure Proposal the procedure used. Each Intent is executed by the signer whose resources or authority it uses. Publishing implies the author's signature. A **signed Proposal** carries its collected signatures. It is visible to its signers; wider visibility is **Planned** ([03 §10.3](03_gameboard.md#103-information-visibility)).

The signature rules are:

- **Single-signer Proposal:** a Trade, a gift Transfer, a Transform or a ChangeRule. Its author is its only signer.
- **Joint Proposal:** needs every signer. It may bundle only Transfers and Transforms. A joint Proposal containing a Trade, a Route or a ChangeRule is rejected. Before its counterparties answer it is an offer, and a counterparty's signature is the acceptance.
- **Procedure Proposal:** executes when the signed weight exceeds the threshold ([§2.2](#22-three-projections-of-an-entity)). It may carry any Intent its procedure authorizes, including a Trade. The joint-Proposal restrictions do not apply to it.

A joint Proposal is all-or-nothing: if its signed facility orders exceed what their pool can fill, every joint Proposal in that pool is rejected together. A Proposal whose signature rule is not met lapses, which is a recorded decline, and the round resolves normally. A counteroffer is a new Proposal, signable next round; the engine enforces no promises across rounds.

A fully signed Proposal is escrowed at screening ([§8.2](#82-funding-and-settlement)). Its Intents settle atomically, or the whole Proposal is rejected and recorded, for example when escrow, transferability or delivery fails. A Proposal's identifier derives from its round, author and submission order, so recorded signatures replay deterministically.

## 4. Policies

This section defines what a Policy is and what it sees and returns. The language of declarative Policies is specified in [03 §8](03_gameboard.md#8-policy-catalog). A Policy is anything that produces Intents from the authorized View of the Entity it drives. It is a function of the View, its memory and the turn.

**Policy + View + Memory + Turn → Intents**

A View is what a Policy sees at the start of a round: its own Entity, prices, the Rules in force, transferability, the events so far, stored adjustment factors, the Proposals awaiting its signature, the Markets it can access, the known Entities and Transformations with their bounds, and the declared Rule kinds. A Policy never sees World State.

### 4.1 Policy kinds

A Policy is a **declarative strategy** (NPC rules, Aggregate baselines and seat presets, run on the shared interpreter), an **AI agent** (a language model given the role's briefing and View), or a **human** (whose text is interpreted into Intents). Their replay is specified in [06 §4](06_simulator.md#4-seat-policies) and [04 §7.3](04_instantiated_game.md#73-seeds-and-randomness).

### 4.2 Policy rules

An initialized Policy operates only on its authorized View, produces typed Intents, and stays deterministic. Its definition is immutable and versioned for the game ([INV-12](#inv-12)), though its outputs vary with the World State it reacts to.

A seat's Policy is bound at run time. Swapping it never changes the Actor's definition ([INV-30](#inv-30)). A Policy that enacts a Rule does so through `ChangeRule`. It never rewrites any Entity's Policy.

**Planned:** Policy memory. No World State field holds it yet. Policies read the previous round's events and stored factors.

### 4.3 Aggregate baselines

Every Aggregate has a baseline Policy for its implicit population. It declares which behavioral domains it controls, so that Representations adjust the right outputs, and it decides when to publish a procedure Proposal ([§2.2](#22-three-projections-of-an-entity)). It reads the factors stored last round and scales the matching baseline by them ([§7.4](#74-estimation-semantics)). With none stored, it follows its baseline. Aggregates do not sign other Entities' Proposals. A Gameboard permission to let them sign multi-signer Proposals is **Planned**.

### 4.4 Shared decision-language interpreter

All declarative strategies run on one shared interpreter that drafts only Intents the View cannot already show to be invalid, so a Strategy does not produce orders the engine is certain to reject. Signatures, funding and pool capacity stay engine-only. Joint and procedure Intents are exempt, since the engine checks them against their own authority. Human and AI Intents are not filtered: an invalid one is a recorded rejection. A seat preset and an NPC expressing the same rule therefore behave identically.

## 5. Cost Functions, Markets, and Transformations

This section defines how quantity is priced and exchanged. Pricing never creates an asset: only a Transformation does ([INV-07](#inv-07)).

### 5.1 CostFunction

A CostFunction is a linear cumulative cost curve. Its marginal price at cumulative position q is p(q) = intercept + slope·q, and moving the position by Δq costs C(q + Δq) − C(q). It has a supported domain. A Market's position persists across rounds. A Transformation's position is its total for the current round.

### 5.2 Exchange Markets

A Market exchanges the Resources in its reserves. It has a base Resource (the good), a quote Resource (the denomination), a CostFunction, a position, reserves and an access list of Entities. It buys and sells on the same cost function, within domain and reserve limits. It cannot supply a Resource it does not hold.

A **Route** is planned across the Markets for its pair. Its **candidates** are the accessible Markets that match the pair and for which the Entity holds the trade capability; with none, it is rejected at screening. It must carry a limit price, which is its escrow ceiling for a buy. **Merit order** takes candidates by opening marginal price (lowest first to buy, highest first to sell, ties by Market id). The **split** is planned in canonical order: each candidate in merit order takes the largest quantity whose batch price stays within its cap, the limit or the next candidate's opening price if tighter.

- **Guarantees.** The split is deterministic and order-independent ([INV-11](#inv-11)). An Entity's fills sum to at most its quantity, and no Market is filled beyond its domain or reserves ([INV-06](#inv-06)).
- **Non-guarantees.** The split does not equalise marginal prices across Markets. An order removed at clearing can leave a slice unfilled rather than re-routed.

### 5.3 Facilities (costed Transformations)

A Transformation declares a recipe: inputs and outputs per unit, **draws** held for the round and released at commit, an optional payment through a CostFunction, order bounds, a delay, delayed flows, and whether the holder must sign each order. A draw is taken from the ordering Entity (a **reservation**, not consumed) or from the Resource's holder (`HolderOf`). A costed Transformation is a **facility**. It is accounted as a Transformation and holds no reserves ([03 §5.3](03_gameboard.md#53-facilities)).

Facilities in one **pool** share what the Resource can supply. Those sharing a cost function share one uniform price. Every member escrows at the pool's ceiling (what the Resource can supply divided by the smallest per-unit draw, rounded down), so the escrowed price bounds the settled price. A facility in no pool escrows at the top of its cost domain.

Production, consumption, procurement, issuance, destruction and investment are Transformations. They modify holdings only when inputs, draws and authority are available. A payment re-enters circulation at its payee. A facility may be shown as a "market" in the player interface, but its outputs are created only by its recipe, within what it can draw ([INV-07](#inv-07)).

### 5.4 Flow gates and flows Rules

A **flow gate** conditions a Transfer of a Resource. Unless the gate exempts parties who share a Market, and unless a bundled order on the named Transformation carries the Transfer, the Transfer is refused.

A **flows Rule** applies to a Transfer between the groups it names, when it covers the Resource. A Block refuses the Transfer. A Levy charges it by its basis (per unit, or ad valorem on a Market's price), paid by the sender in the named Resource, to the recipient, to the governor, or to a sink. Levies do not enter clearing ([§5.5](#55-simultaneous-clearing)). A holding Rule removes its reduced units from what can be drawn from a holder. It changes no holding and no flow. Rule changes take effect at the next round boundary ([§8.1](#81-canonical-resolution-sequence)). How a setting uses these is in [03 §5.5](03_gameboard.md#55-geography-access-and-rules).

### 5.5 Simultaneous clearing

All orders in a round are resolved as one batch, and each Market clears at one uniform price. The steps are:

1. Orders whose access rules fail are rejected, and the remaining purchases and sales are netted.
2. Residual net demand is executed against the cost function, within domain and reserve limits.
3. The uniform batch price is the average marginal cost over the net quantity, or the current marginal price when net demand is zero. It is rounded before it multiplies quantities ([§9.1](#91-numerics-and-randomness)).
4. Orders whose limit is violated at that price are removed, and the price is recomputed. Removed orders stay removed, so the procedure terminates and depends only on the set of orders. Partial fills follow [§3.2](#32-infeasible-actions).
5. Holdings and reserves change as one net update.

The result does not depend on the order of submissions ([INV-11](#inv-11)). A settlement event is a **Coupled flow**: its legs settle together at one exact stored rate, when it has one. A trade is two legs at a rate. A Transform is its input, output and payment legs.

## 6. Economic Circulation

This section defines the recurring cycle that moves quantities between holders each round. It is evaluated from the Snapshot together with the round's batch ([§8.2](#82-funding-and-settlement)). The setting's authored forms are in [03 §5.4](03_gameboard.md#54-recurring-cycle).

The cycle is data in World State, one recurring flow per rule: a source, a destination, a Resource, an amount (fixed, an amount held, a fraction held, or measured by a stored query over the round's record), a shortfall effect, and whether the recipient's accrued offset reduces it. Each evaluation produces a Coupled flow whose cause is the rule.

Every Resource stock reconciles each round:

**Closing stock = Opening stock + Production + Issuance − Consumption − Destruction − Decay**

Transfers and trades sum to zero across all accounts, including Market reserves ([INV-05](#inv-05)). A draw moves nothing, so it has no term here. A delayed flow counts as production or destruction of its Resource.

## 7. Aggregates and Representation

This section defines how explicit Actors influence an implicit Aggregate. A **Representation** treats an Actor's behavior as a sample of the population and adjusts the baseline. A **collective procedure** lets members' signatures decide whether the Aggregate's own Proposal executes ([§2.2](#22-three-projections-of-an-entity)). Neither gives the Actor the Aggregate's resources or authority.

### 7.1 Aggregate accounts

An Aggregate's economic accounts are its residual: the accounts and behavior of its population that explicit Actors do not hold. Instantiation partitions them ([04 §4.2](04_instantiated_game.md#42-partitioning)).

### 7.2 Representation and estimators

A Representation links an explicit Actor to an Aggregate. It names the behavior domain it adjusts, the Resource whose purchases it observes, the Actor's weight in the combination, and an estimator. The estimator has a reference behavior, a shrinkage (1 means the deviation is fully applied), an optional bound on the deviation, and a noise spread (zero means none). The domain limits which part of the Actor's behavior may influence the Aggregate. A Representation never transfers holdings or capabilities.

### 7.3 Representation pipeline

The engine records four stages: **observation** (round r, the Actor's total proposed buy quantity of the observed Resource, taken from a validated Proposal before funding and not what it executed), **estimate** (round r, the stored adjustment factor), the **proposed Aggregate Intent** (round r + 1, the Aggregate Policy's adjusted Proposal), and the **executed result** (round r + 1). Any difference between intended and executed actions is recorded.

### 7.4 Estimation semantics

Representation is an inference, never a resource multiplier. Each representative's deviation from its reference is perturbed and then clamped:

**Deviationᵢ = clamp(deviationBound, (observedᵢ − referenceᵢ) / referenceᵢ + εᵢ)**, where **εᵢ = noiseSpreadᵢ × (2uᵢ − 1)** and uᵢ is the seeded draw for that input ([§9.1](#91-numerics-and-randomness)).

Noise models the uncertainty of inferring a population from a few representatives. It is added before clamping, so the bound still caps the effect. A zero reference contributes no estimate.

**Adjustment factor = 1 + Σ(shrinkageᵢ × weightᵢ × deviationᵢ)**

The sum runs over the Representations targeting that Aggregate and domain. Weights account for overlapping and possibly biased samples, so representatives are not treated as independent. The factor is stored in World State. In round r + 1 the Aggregate's Policy proposes **Adjusted target = Baseline × stored factor**, with the baseline of round r + 1, applied exactly once. With no observation in the previous round, no factor is stored and the Aggregate follows its baseline.

An Actor's direct transactions execute once, from its own accounts. The adjusted behavior replaces the matching baseline and is never added to a complete baseline ([INV-09](#inv-09)). A representative's decision has two validated effects, direct and estimated, and neither is counted twice.

## 8. World State and Round Resolution

This section defines the state the engine advances and the one sequence that advances it. World State is the complete economic state at a round boundary. Policies read it and never mutate it. They only submit Intents. The engine advances the world through one deterministic resolution, from World State and signed Proposals to the next World State.

### 8.1 Canonical resolution sequence

This is the single authoritative sequence. The other layers reference it and do not redefine it. The phases run in this order, and each guarantees what is stated.

1. **Prepare** (engine): applies the deliveries and Rule changes due at the boundary.
2. **Observe** (engine): produces the beginning-of-round Snapshot and each Entity's View. Every decision is made on it.
3. **Propose and sign** (round runner): every Policy publishes Proposals against the Snapshot at the same time; then each signer signs the Proposals it endorses. The set is frozen at the end of Propose ([§3.3](#33-proposals-and-signatures)).
4. **Screen and interpret** (engine): checks authority, Entity identity and each signature rule. Proposals that fail their rule lapse and are recorded. Admissible observations are extracted from validated Proposals. Nothing is filled yet.
5. **Estimate** (engine): computes each Representation's adjustment factor from this round's validated Proposals and stores it. It creates and modifies no Proposal ([§7.4](#74-estimation-semantics)).
6. **Resolve economy** (engine): funds every accepted action from the Snapshot, then settles and clears all of them as one simultaneous batch. Markets clear at uniform prices. Transformations, signed joint Proposals, passed procedure Proposals and the recurring cycle ([§6](#6-economic-circulation)) are evaluated together, into one net change.
7. **Commit and record** (engine): commits the new World State atomically and records the round's inputs and events.

### 8.2 Funding and settlement

- **Funding from the Snapshot.** Each Entity's actions are funded from its holdings at the start of the round. At screening, each signed Proposal escrows its maximum cost: a buy its limit times quantity (or the Market's upper-bound price when unlimited), a sell its quantity, a facility order its pool ceiling, a reservation its per-unit draw times quantity. Joint feasibility and pro-rata scaling are checked against these escrows ([§3.2](#32-infeasible-actions)). Unused escrow is released at commit.
- **Credit at commit.** Proceeds and receipts are credited when the round commits and are usable only next round. Selling and buying both draw on opening holdings.

Three shortage laws hold for every setting:

1. **One short Resource scales everything an Entity executes.** Its funding scale is the smallest ratio of available to claimed across all Resources it claims, and that one factor scales every single-signer Intent it executes.
2. **Joint Proposals fill first, and overflow drops them all.** They are funded before single-signer orders and are never scaled. A party whose joint demand on a Resource exceeds its holding loses every joint Proposal that claims it. Single-signer orders then share a pool pro rata.
3. **A quote-reserve shortage drops the whole sell side.** If a batch would overdraw a Market's quote reserves, its sell orders are removed and the batch clears on its buys alone.

A Transformation's maximum holds per Entity per round: a screened Proposal that would exceed it is rejected whole. An order funded below its minimum is removed. Every payment is capped at what the payer holds after the batch so far, and any remainder is a shortfall. A negative holding or reserve fails the round as an invariant failure ([INV-04](#inv-04)).

### 8.3 Outcomes and round records

Lapses and infeasible actions are recorded as events and never fail a round. A round fails only on an invariant failure, such as a negative holding that settlement should have prevented ([INV-04](#inv-04)).

The round record and the manifest are defined in [04 Terms](04_instantiated_game.md#terms). The manifest is immutable ([INV-31](#inv-31)). Resolution returns the new World State together with the round record.

### 8.4 Effects and handlers

The engine has no effects. In-world randomness uses a pure keyed draw ([§9.1](#91-numerics-and-randomness)) with the game seed held in World State. The player seed never reaches the engine.

Everything else impure is a named effect: a capability a program declares it needs, interpreted by a handler over device primitives. The request and reply shapes are in [07 §4](07_architecture.md#4-key-interfaces).

The effects are: **Seat input** (a seat's Proposals, signatures and transcript); **Interpret** (the round's transcripts read together into Intents, with no confirmation in live play); **Narrate** (round text from resolved events, adding no facts); **Record** (appends a round's result to the run); **Observe** (Run Store reads, which cannot write); **Store** (Run Store lifecycle writes); **API** (every route of the dashboard's HTTP API, [06 §8.1](06_simulator.md#81-read-api-and-run-operations)); **Cache** (what a process keeps between requests); **Play** (one whole stored play); **Live** (one live session, settled at its end); and **Platform** (the clock, process identity, files, the terminal, and one operating-system number, drawn only for a player seed nobody gave).

Every program is typed by the capabilities it asks for, so its type states all it can touch. Handlers choose the context (live session, simulation, replay or test). The round runner is the same code under every context. The per-context behavior is in [04](04_instantiated_game.md) and [06](06_simulator.md).

## 9. Architectural Invariants and Numerics

The invariant table is the one numbered table for the whole system; other layers cite its IDs. IDs 18, 20 and 32–35 are retired. **Checked** means a predicate, a validation or a named test enforces the invariant today. The per-round assertion status is in [06 §7](06_simulator.md#7-run-assertions).

| ID | Invariant | Status |
|---|---|---|
| <a id="inv-01"></a>INV-01 | Unified action semantics: all participants act through the same Intent and validation. Every executed Intent came from a signed Proposal. | Checked |
| <a id="inv-02"></a>INV-02 | Authority separation: an Entity exercises only the capabilities granted to it, within scope. | Checked |
| <a id="inv-03"></a>INV-03 | No authority amplification: membership, Representation, signing a procedure Proposal and fallbacks never confer or remove capabilities or holdings. | Checked |
| <a id="inv-04"></a>INV-04 | Resource consistency: holdings change only by authorized exchange, transformation, issuance, destruction or decay. No holding or Market reserve is negative. | Checked |
| <a id="inv-05"></a>INV-05 | Stock-flow reconciliation: every Resource stock reconciles each round ([§6](#6-economic-circulation)), transfers net to zero, and no account accumulates without a declared outflow. | Checked |
| <a id="inv-06"></a>INV-06 | Market solvency: Market positions stay within their cost domain, and facility fills never exceed what their pool can draw or their escrowed ceiling. | Checked |
| <a id="inv-07"></a>INV-07 | Price is not production: a CostFunction never creates an asset without an authorizing Transformation. | Planned |
| <a id="inv-08"></a>INV-08 | Account partition: every account and behavioral responsibility has exactly one owner, however memberships overlap. A Resource named by a `HolderOf` draw has at most one positive holder. | Checked |
| <a id="inv-09"></a>INV-09 | Representation consistency: a stored estimate replaces the matching baseline exactly once, in the following round, within estimator bounds, and never duplicates direct activity. | Checked |
| <a id="inv-10"></a>INV-10 | Signature completeness: a Proposal executes only if its signature rule is met. A joint Proposal settles atomically or is rejected together. | Checked |
| <a id="inv-11"></a>INV-11 | Order independence: all actions resolve as one bundled update with uniform clearing. Permuting submissions never changes settlement. | Checked |
| <a id="inv-12"></a>INV-12 | Policy reproducibility: a Policy's output is determined by its versioned definition, permitted view, memory, time and declared randomness. | Planned |
| <a id="inv-13"></a>INV-13 | Deterministic replay: the same manifest, seeds and recorded seat input reproduce identical World States. Every random value is a keyed draw. | Checked |
| <a id="inv-14"></a>INV-14 | Resolution consistency: instantiating a world at different levels of explicitness reconciles accounts, liabilities and authority before any decision differs. | Checked |
| <a id="inv-15"></a>INV-15 | Single authoritative state: all actions modify one shared World State and timeline. | Planned |
| <a id="inv-16"></a>INV-16 | Information isolation: every Policy, in-world or seat, observes only what the Gameboard permits. | Planned |
| <a id="inv-17"></a>INV-17 | Type-system conformity: all setting definitions use supported types and operations. | Checked |
| <a id="inv-19"></a>INV-19 | Role consequence: every Actor definition declares a capability, a dependency and a measurable objective. | Checked |
| <a id="inv-21"></a>INV-21 | Instantiation feasibility: the Gameboard admits at least one valid initial world. | Checked |
| <a id="inv-22"></a>INV-22 | Version stability: a Gameboard version's definitions are immutable. | Planned |
| <a id="inv-23"></a>INV-23 | Unique control: every explicit Actor has exactly one valid controller. | Checked |
| <a id="inv-24"></a>INV-24 | Valid participation: every selected Actor and Representation is permitted by the Gameboard and validly parameterized. | Checked |
| <a id="inv-25"></a>INV-25 | Fallback completeness: every unselected Actor has a permitted fallback, and no required responsibility is left unowned. | Checked |
| <a id="inv-26"></a>INV-26 | Policy completeness: every NPC and Aggregate has an executable, initialized Policy. | Checked |
| <a id="inv-27"></a>INV-27 | Market feasibility: initial Market positions, reserves and holdings satisfy economic constraints. | Planned |
| <a id="inv-28"></a>INV-28 | Temporal consistency: round intervals and Policy timing fit the simulated horizon. | Checked |
| <a id="inv-29"></a>INV-29 | Engine and interpreter reuse: simulated games use the same engine, round sequence and decision-language interpreter as real games. | Checked |
| <a id="inv-30"></a>INV-30 | Controller integrity: swapping a seat's Policy never changes the Actor's definition or turns a seat into an NPC. | Checked |
| <a id="inv-31"></a>INV-31 | Authoritative records: manifests, append-only round records and snapshots are authoritative and immutable. Metrics are versioned projections. | Checked |
| <a id="inv-36"></a>INV-36 | Purity: the engine is a pure function. All impurity occurs only through declared effects interpreted by handlers ([§8.4](#84-effects-and-handlers)). | Checked |

### 9.1 Numerics and randomness

This section sets the numeric and random rules for every economic value. Mechanics are deterministic by default. Randomness appears only where a Gameboard rule or estimator calls for it, such as estimator sampling noise.

**Two seed streams.** The **game seed** is one per game instance, fixed at initialization and held in World State. It feeds every in-world draw. The **player seed** keys the draws of seat Policies. It is recorded with the run and is never part of the configuration, World State or the engine's input. If no player seed is given, one is drawn once and recorded.

**Draws.** A draw is a pure function of a seed and a draw key (round, source, input Entity, category and index), so randomness needs no effect. It returns a value in [0, 1) at four decimal places. Draws are keyed, not sequenced, so adding an Entity or changing evaluation order never changes unrelated draws. The algorithm is versioned in the manifest.

**Fixed point.** All quantities, prices and settlement amounts are fixed-point numbers with **four decimal places**. Every product or quotient off that grid is rounded **half-to-even** (ties go to the even last digit, symmetric in sign). No floating-point arithmetic enters economic state. A uniform batch price is rounded before it multiplies quantities.

- **Representation.** A quantity is an exact integer count of 10⁻⁴ units, with exact addition, subtraction and negation and no overflow. A negative holding is an invariant violation ([INV-04](#inv-04)), not a numeric event.
- **Remainders.** A total split among shares uses largest remainder, ties to the earlier canonical index, so the shares sum to the total. A quotient used as a scale truncates toward zero, so a scaled amount never exceeds its numerator. A half-even quotient can round up and overdraw.
- **Parse and format.** A decimal has an optional sign, digits, and up to four decimal places. A fifth decimal is an error; no input is rounded silently. Division by a divisor that can be zero is explicit, never a silent default.
- **Display.** Each display kind has one rule, and every view and chart that shows a number names its kind:
  - **Quantity** (Money and Resource counts): a whole number has no decimals. Otherwise at most two, trailing zeros trimmed (`12`, `12.5`, `12.25`). A non-zero value that rounds to zero shows as `<0.01`.
  - **Price** (quote per base unit): displayed as a Quantity.
  - **Ratio** (Gini, influence, shrinkage, factors, support, levy rates): always two decimals. Chart ticks take the chart's kind and are never parsed back.
- **Floating point.** No float enters economic state. The only conversion of a quantity to a float is for chart plotting. Integers that are not economic values, such as rounds, seeds and counts, stay integers.

Which kind each page uses is set by [06 §9](06_simulator.md#9-pages).

## 10. Design Boundaries

The type system does not model resource classifications, a macroeconomic model, estimator parameters, procedure thresholds or market realism. These are choices each setting makes, and the design principles behind them are in [01](01_overview.md).
