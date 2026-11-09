# Type System Specification

## 1. Purpose

This layer defines the kernel of the economy and the economic types every setting shares. The kernel is a small vocabulary of stocks, flows, laws over them, and couplings of conditional obligations ([§3](#3-the-kernel)). It decides no trade, signature, procedure or schedule: those are vocabulary a setting authors, and they compile to kernel values ([03 §14](03_gameboard.md#14-compilation-to-the-kernel)). Explicit Actors, Aggregates and implicit populations act through the same kernel in one deterministic economy. The engine is pure: every impure act is a named effect ([§8.4](#84-effects-and-handlers)).

The game is in [01](01_overview.md), settings and the authored vocabulary in [03](03_gameboard.md), execution in [04](04_instantiated_game.md), the reference setting in [05](05_test_instance.md), and the simulator in [06](06_simulator.md). This document owns the type-system terms in [§2.5](#25-glossary).

## 2. Core Ontology

This section names the types the layer is built from. Each is defined in the glossary ([§2.5](#25-glossary)) and specified in the section named: Entity and its three projections ([§2.1](#21-entity), [§2.2](#22-three-projections-of-an-entity)); Resource and Account ([§2.3](#23-resource)); Capability ([§2.4](#24-capability)); Stock, Flow, Law, Coupling, Obligation, Condition and Successor ([§3](#3-the-kernel)); CostFunction, Market and the solvers ([§5](#5-cost-functions-markets-and-solvers)); Aggregate and Representation ([§7](#7-aggregates-and-representation)); Policy ([§4](#4-policies)); and World State, Round record and Manifest ([§8](#8-world-state-and-round-resolution)). Which agreements, institutions and schedules a setting has are authored, not core ([§10](#10-design-boundaries)).

### 2.1 Entity

An Entity is the one economic record that every participant, explicit or implicit, carries. It has an identifier, accounts, capabilities and a kind.

Its kind is an **Actor**, driven by a Controller (a Seat supplied from outside the setting, or an NPC the setting defines), or an **Aggregate**, with a baseline Policy and descriptive members ([§2.2](#22-three-projections-of-an-entity)). Actors and Aggregates own accounts and act through the same kernel. They differ only in how their decisions are produced. The reference instance is in [05 §9](05_test_instance.md#9-actor-catalog) and [§10](05_test_instance.md#10-aggregate-catalog).

### 2.2 Three projections of an Entity

Membership, economic ownership and collective decision-making are distinct relations. The layer keeps them separate rather than letting "Aggregate" carry all three.

| Projection | Rule |
|---|---|
| **Membership** | Descriptive. It may overlap with other Aggregates and confers no ownership or authority. |
| **Account ownership** | Every account and behavioral responsibility has exactly one owner ([INV-08](#inv-08)). |
| **Collective decision-making** | Authority belongs to the institutional Entity, never to members by virtue of membership ([INV-03](#inv-03)). How an Entity decides is authored ([03 §6.3](03_gameboard.md#63-aggregates)). |

Membership never lets several Aggregates own the same assets.

### 2.3 Resource

A Resource is an identified economic quantity with a unit and declared behavior. Its transferability is **Free** (it moves on its holder's consent alone), **Restricted** (each movement needs the transfer capability, [§2.4](#24-capability)), or **NonTransferable**. A category is a setting's own grouping, which the engine never reads. A Resource that nothing authored produces or consumes is conserved.

Not everything economically meaningful is a Resource. Market access and institutional authority are state or laws attached to Entities and Markets. Holdings are distinct from capabilities: influence over a law does not let an Entity spend, transfer or create what the law affects.

### 2.4 Capability

A Capability is a condition on a party: what an Entity may do. The kernel asks only whether a party holds one, through a `Holds` condition ([§3.3](#33-couplings-and-conditions)). It does not read what a capability means. The kinds of capability are authored ([03 §6](03_gameboard.md#6-entity-catalog)).

Institutional capabilities stay with the institutional Entity or with a declared law. If an institution is not played explicitly, its capabilities stay with its Aggregate and never move to a population Aggregate. Membership, Representation and taking part in a collective decision never transfer capabilities to a member or representative.

### 2.5 Glossary

This table is the one place the type-system terms are defined. Scenario terms are in [01 §1](01_overview.md#1-concept), and the other layers' terms in their `Terms` tables ([03](03_gameboard.md#terms), [04](04_instantiated_game.md#terms), [06](06_simulator.md#terms), [07](07_architecture.md#terms)).

| Term | Definition |
|---|---|
| **Account** | A quantity of a Resource owned by exactly one Entity or Market. |
| **Actor** | An Entity represented explicitly, driven by a Controller ([§2.1](#21-entity)). |
| **Adjustment factor** | The stored multiplier that scales an Aggregate's baseline in the following round ([§7.4](#74-estimation-semantics)). |
| **Aggregate** | An Entity holding the residual accounts and baseline behavior of an implicit population or institution ([§7](#7-aggregates-and-representation)). |
| **Behavior domain** | The part of an Aggregate's baseline that a Representation may adjust ([§7.2](#72-representation-and-estimators)). |
| **Bond** | How a `Performed` condition binds one obligation to another: **Whole** (all or nothing) or **Proportional** at a rate ([§3.3](#33-couplings-and-conditions)). An obligation with no such condition stands alone. |
| **Capability** | A condition on a party, stating what an Entity may do ([§2.4](#24-capability)). |
| **Cause** | What produced a settlement. The kernel carries it as data and does not read it. The authored vocabulary says what it names. |
| **Condition** | What an obligation depends on: another obligation's performance, a stock level, consent, a price, a quantity, or a capability ([§3.3](#33-couplings-and-conditions)). |
| **Consumption** | How an obligation ends: exactly once, in its due round, as performed, short or lapsed ([§3.4](#34-successors)). |
| **Controller** | What drives an Actor: a Seat or an NPC Policy. |
| **CostFunction** | A linear cumulative cost curve that prices quantity without creating it ([§5.1](#51-costfunction)). |
| **Coupling** | A multilateral agreement: obligations settled together, with their Phase, Rationing, Cause and who has consented ([§3.3](#33-couplings-and-conditions)). |
| **Display kind** | Quantity, Price or Ratio. Each has one display rule ([§9.1](#91-numerics-and-randomness)). |
| **Draw** | A condition on a stock plus a Hold for one round. It moves nothing, so it is not a flow. |
| **Draw key** | The semantic name of one random draw ([§9.1](#91-numerics-and-randomness)). |
| **Effect** | A declared impure operation, interpreted by a handler ([§8.4](#84-effects-and-handlers)). Randomness is not an effect. |
| **Entity** | The economic record of a participant, Actor or Aggregate ([§2.1](#21-entity)). |
| **Escrow** | What is set aside at screening against a claim's maximum cost, released at commit if unused ([§8.2](#82-funding-and-settlement)). |
| **Estimator** | The configured function that turns a representative's observed behavior into an adjustment factor ([§7.4](#74-estimation-semantics)). |
| **Flow** | A quantity of a Resource moving between endpoints: a holder, a Market's reserve, or the economy's boundary (`Source`, `Sink`) ([§3.1](#31-stocks-flows-and-legs)). Flows settled together form a Coupled flow ([§5.5](#55-simultaneous-clearing)). |
| **Game seed** | The one seed per game instance, fixed at initialization, from which every in-world draw derives ([§9.1](#91-numerics-and-randomness)). |
| **Hold** | A quantity set aside for one round and released at commit. It moves nothing ([§3.3](#33-couplings-and-conditions)). |
| **Holder** | The Entity that answers for a Resource: the one with a positive holding, else the first in id order with an account. |
| **Holding** | A quantity of a Resource held by an Entity or in a Market's reserves. |
| **Law** | A rule over stocks or flows: a Subject, a Trigger, an Operation, and a guard of Conditions ([§3.2](#32-laws)). |
| **Leg** | One movement in a settlement: a Resource, two endpoints, how much was owed, and how much moved. |
| **Market** | A Standing coupling with reserves and a cleared rate, which exchanges the Resources it holds at one uniform batch price ([§5.2](#52-exchange-markets)). |
| **Money** | The numeraire Resource, the unit of account for views. The engine does not read it: a leg names its own Resource. |
| **NPC** | An Actor whose Policy is part of the setting. |
| **Obligation** | One named party's duty for one round: a Leg, or a change to a law ([§3.3](#33-couplings-and-conditions)). |
| **Observation** | The admissible proposed buy quantity a Representation reads from its Actor's input to the round ([§7.3](#73-representation-pipeline)). |
| **Operation** | What a law does: emit a Leg, refuse, or withhold a portion ([§3.2](#32-laws)). |
| **Pending obligation** | A coupling in World State waiting for its due round ([§3.4](#34-successors)). |
| **Phase** | Where in a round a coupling or law settles. Lower runs first ([§3.5](#35-resolution)). |
| **Player seed** | The seed that keys the draws of seat Policies. It is never part of World State ([§9.1](#91-numerics-and-randomness)). |
| **Policy** | Anything that produces the engine's input from the View of the Entity it drives ([§4](#4-policies)). |
| **Pool** | The claims on one Resource's supply, which share what it can supply ([§5.4](#54-rationing)). |
| **Rationing** | Dividing one limit among many claims, configured by Tier, whole or scaled, and scale group ([§5.4](#54-rationing)). |
| **Representation** | A link through which an explicit Actor's sampled behavior adjusts an Aggregate's baseline ([§7.2](#72-representation-and-estimators)). |
| **Residual** | The accounts and behavior of a population left in an Aggregate once explicit Actors' are assigned ([§7.1](#71-aggregate-accounts)). |
| **Resource** | An economic quantity with a unit and declared behavior ([§2.3](#23-resource)). |
| **Seat** | An Actor whose Policy is supplied from outside the setting. Swapping the implementation never changes the Actor ([INV-30](#inv-30)). |
| **Shortfall** | `moved < owed` on a leg: an amount owed but not delivered, because the source's holdings fell short. |
| **Snapshot** | The World State a round ends with. The next round's decisions are all made on it ([§8.1](#81-canonical-resolution-sequence)). |
| **Solver** | One of the closed set of mechanisms that divide one limit among many: clearing on a curve, routing across Markets, and pool allocation ([§5](#5-cost-functions-markets-and-solvers)). |
| **Standing coupling** | A continuing coupling whose terms are data read each round rather than a value that is consumed. A Market is one ([§5.2](#52-exchange-markets)). |
| **Stock** | A quantity of a Resource at a holder or a Market's reserve ([§3.1](#31-stocks-flows-and-legs)). |
| **Subject** | What a law applies to: a stock or a flow ([§3.2](#32-laws)). |
| **Successor** | An obligation spawned by consuming another, for a later round ([§3.4](#34-successors)). |
| **Template** | A named coupling with parameters and bound parties, which a Successor instantiates ([§3.4](#34-successors)). |
| **Tier** | A Rationing priority. Lower fills first ([§5.4](#54-rationing)). |
| **Trigger** | When a law applies: each round, or on an action ([§3.2](#32-laws)). |
| **Uniform batch price** | The single price at which a Market clears its round batch ([§5.5](#55-simultaneous-clearing)). |
| **View** | What a Policy sees of the beginning-of-round Snapshot ([§4](#4-policies)). |
| **World State** | The complete economic state at a round boundary, including the game seed and the round number ([§8](#8-world-state-and-round-resolution)). |

## 3. The Kernel

The kernel is what core offers: stocks, flows, laws on them, and couplings of conditional obligations. A setting's vocabulary compiles to it ([03 §14](03_gameboard.md#14-compilation-to-the-kernel)). Every leg of every settlement is posted through one place ([§3.1](#31-stocks-flows-and-legs)). The kernel never authors or rewrites a decision and never knows how one was produced ([§4](#4-policies)).

### 3.1 Stocks, flows, and legs

A **stock** is a quantity of a Resource at a holder or in a Market's reserve. A **flow** is a quantity of a Resource moving between endpoints: `Held` (an Entity's account), `InMarket` (a Market's reserve), `Source` (the economy's boundary, where a Resource is created) and `Sink` (the boundary, where it ends). A settled flow is a **leg**, recording what was owed and what moved.

**A stock changes only by a leg.** `Core.Kernel.Post` is the only code that writes the ledger, and `scripts/check/layout.js` enforces that. A leg honours its obligation's shortfall rule:

- **Cap** moves what the source holds, recording owed and moved.
- **Reject** moves nothing when the whole cannot move.

The boundary can always pay, and so can a reserve: clearing bounds a batch's net, and a negative closing reserve fails the round at commit ([INV-04](#inv-04)). A leg that moves nothing still writes its accounts. `crossesBoundary` classifies a leg as crossing the boundary (from `Source` or to `Sink`) or conserving ([INV-07](#inv-07)).

### 3.2 Laws

A law is a structure over stocks and flows. Stocks and flows take laws in parallel: a law's **subject** is one or the other, and its **trigger** is time or an action.

| Subject | Trigger: each round | Trigger: on an action |
|---|---|---|
| Stock | decay, depreciation, interest | wear from use |
| Flow | delay, expiry | levy, block |

The kernel has no concept of decay, depreciation, tariff or tax. Those are values the authoring layer writes ([03 §15](03_gameboard.md#15-authoring-laws-and-agreements)). The table names patterns, not kernel kinds. Not every cell is used yet: in the shipped engine a stock law runs only each round, and a flow law only on the action that moves it.

A law's **operation** is one of three:

- **Emit** a leg, priced from the subject's quantity. A law that emits keeps the stock-flow identity ([INV-05](#inv-05)): depreciation is a leg from the stock to `Sink`.
- **Refuse** the action.
- **Withhold** a portion: it cannot be drawn, and no holding changes.

A law also has a guard of conditions, a phase and a cause. `Core.Kernel.Law` applies a law to one holder and matches a flow against its scope.

### 3.3 Couplings and conditions

A **coupling** is a multilateral agreement. It holds its obligations, its holds, a rationing configuration ([§5.4](#54-rationing)), a phase, a cause, and the parties who have consented. An **obligation** is one named party's duty for one round. Its effect is a leg or a **change to a law** (put a law in force, or remove one). A change to a law can be bound like any other effect, so a treaty can bind "I remove this levy" to "you pay this amount".

Each obligation carries **conditions**, each with what its failure reports:

| Condition | Holds when |
|---|---|
| `Performed` | Another obligation of the coupling is still standing, bound by a **Whole** or **Proportional** bond. |
| `StockAtLeast` | A stock holds at least an amount. |
| `Consent` | The named parties have signed the coupling. |
| `WeightedConsent` | Residual weight plus the signed weights exceed a threshold. |
| `PriceWithin` | The cleared price is within a limit. |
| `QuantityAtLeast` | The obligation's quantity is at least an amount. |
| `Holds` | A party holds a capability ([§2.4](#24-capability)). |

A `Performed` condition says whether one obligation applies with another. An obligation with no such condition stands alone. Mutual `Performed … Whole` conditions make a coupling atomic with no special case: if one party cannot perform, the others' conditions fail on the next pass ([§3.5](#35-resolution)). `onShort` is a separate question: **Reject** or **Cap** says what a shortfall does ([§3.1](#31-stocks-flows-and-legs)).

What the shipped sequence uses differs from what the kernel can express. `resolve` reads a `Performed` condition only as the other obligation still standing, so `Proportional` carries its rate as data and a proportional fill is decided by the clearing solver. The sequence decides a limit price and an order minimum itself and passes no price to the kernel, so `PriceWithin` and `QuantityAtLeast` hold unconditionally in its calls, and the kernel's own tests exercise them. A coupling's `holds` and `solver` fields are carried, but `resolve` does not act on them: the sequence escrows draws as claims and calls the solvers directly ([§5](#5-cost-functions-markets-and-solvers), [§8.2](#82-funding-and-settlement)).

### 3.4 Successors

An obligation lives for one round. Consuming it can spawn new obligations, so debt, vesting, schedules and default clauses are patterns the authoring layer writes ([03 §15.2](03_gameboard.md#152-multi-round-agreements)). Core has no concept of a loan.

- **Three outcomes.** Every due obligation is consumed exactly once as **performed** (`Fulfilled`), **short** (`Shorted`) or **lapsed** (`Expired`, removed because a condition failed) ([INV-37](#inv-37)). `next` lists what each outcome spawns.
- **Successors are data.** A spawn names a template, a due offset of at least one round, frozen arguments and bound parties. A template may spawn itself, which expresses "until repaid".
- **Spawned at commit, for a later round.** `Core.Kernel.Consume` instantiates each spawn for a later round, so a round stays finite and resolution is undisturbed. A spawn that fails this check (a due offset below one, an unknown template, a missing party, or a party outside its parent) produces no successor.
- **Amounts freeze at consumption.** Arguments are computed from the consumed obligation (`Moved`, `Remainder`) and stored as plain values.
- **Consent is given once.** A successor carries its parent's consent. A template may name only parties bound from its parent ([INV-03](#inv-03)). A renegotiable step adds a fresh `Consent` condition, which holds only when the party's consent was carried from the parent ([03 §15.2](03_gameboard.md#152-multi-round-agreements)).

World State holds one list of **pending** couplings and a table of **templates**. A pending coupling due at or before the round being prepared is consumed at that boundary, in phase then tier order ([§8.1](#81-canonical-resolution-sequence)). Due couplings therefore settle ahead of that round's orders: a successor is senior to new orders by construction, rather than rationed against them by tier.

### 3.5 Resolution

`Core.Kernel.Resolve` resolves couplings by the procedure [§5.5](#55-simultaneous-clearing) uses for limit orders. Phase by phase, every obligation starts admissible, and whatever fails its conditions is removed. Then conditions are recomputed against the set still standing. Removed obligations stay removed, so resolution terminates.

Passes are order-independent ([INV-11](#inv-11)): every condition of a pass reads only the set the pass began with, and survivors post in canonical order. A coupling's phase is the order its legs take within `resolve`. When `resolve` is given several couplings, it runs them phase by phase.

The round's whole sequence is authored vocabulary, and it calls the kernel in stated phases ([§8.1](#81-canonical-resolution-sequence)). There is deliberately no single kernel call for a whole round: what a settlement records is decided per mechanism, some couplings can only be built once earlier phases have run, and a measured amount reads this round's earlier legs. The sequence runs one coupling at a time, at the position §8.1 states, and every leg goes through `Core.Kernel.Post`.

### 3.6 Infeasible actions

Infeasible actions are rejected or partially executed by deterministic rules. The engine never invents resources or authority to make an action succeed.

- A pro-rata share is executed when a party's claims jointly exceed its opening resources, and when demand exceeds supply or what can be drawn ([§5.4](#54-rationing)).
- Indivisible remainders go to the largest remainders, ties in canonical id order, never by submission order ([§9.1](#91-numerics-and-randomness)).
- Orders whose limit price is violated at the batch price are removed at clearing ([§5.5](#55-simultaneous-clearing)).
- A coupling whose obligations are bound whole settles atomically, or is rejected and recorded as a unit.

## 4. Policies

This section defines what a Policy is and what it sees and returns. A Policy is what produces the engine's input from the authorized View of the Entity it drives. It is a function of the View, its memory and the turn. The authored form of that input, the decision language, and the rules of a declarative Strategy are in [03 §8](03_gameboard.md#8-policy-catalog) and [03 §10.4](03_gameboard.md#104-available-actions).

**Policy + View + Memory + Turn → input**

A View is what a Policy sees at the start of a round: its own Entity, prices, the laws in force, transferability, the events so far, stored adjustment factors, the input awaiting its consent, the pending couplings it owes or is owed ([§3.4](#34-successors)), the Markets it can access, and the known Entities and the authored actions and bounds that apply to them. A Policy never sees World State.

### 4.1 Policy kinds

A Policy is a **declarative strategy** (NPC rules, Aggregate baselines and seat presets, run on the shared interpreter), an **AI agent** (a language model given the role's briefing and View), or a **human** (whose text is interpreted into input). Their replay is specified in [06 §4](06_simulator.md#4-seat-policies) and [04 §7.3](04_instantiated_game.md#73-seeds-and-randomness).

### 4.2 Policy rules

An initialized Policy operates only on its authorized View, produces typed input, and stays deterministic. Its definition is immutable and versioned for the game ([INV-12](#inv-12)), though its outputs vary with the World State it reacts to.

A seat's Policy is bound at run time. Swapping it never changes the Actor's definition ([INV-30](#inv-30)). A Policy never rewrites any Entity's Policy.

**Planned:** Policy memory. No World State field holds it yet. Policies read the previous round's events and stored factors.

### 4.3 Aggregate baselines

Every Aggregate has a baseline Policy for its implicit population. It declares which behavioral domains it controls, so that Representations adjust the right outputs, and it decides when to act in the Aggregate's name. It reads the factors stored last round and scales the matching baseline by them ([§7.4](#74-estimation-semantics)). With none stored, it follows its baseline. A Gameboard permission to let Aggregates consent to other Entities' input is **Planned**.

## 5. Cost Functions, Markets, and Solvers

This section defines how quantity is priced and exchanged, and what stays closed in core. Pricing never creates an asset ([INV-07](#inv-07)). Two solvers cannot be written as conditions, because they divide one limit among many: clearing, and routing and rationing. They are a closed set, selected by data.

### 5.1 CostFunction

A CostFunction is a linear cumulative cost curve. Its marginal price at cumulative position q is p(q) = intercept + slope·q, and moving the position by Δq costs C(q + Δq) − C(q). It has a supported domain. A Market's position persists across rounds, and an authored price curve's position is its total for the current round.

### 5.2 Exchange Markets

A Market is a standing coupling with a reserve stock and a cleared rate. It exchanges the Resources in its reserves. It has a base Resource (the good), a quote Resource (the denomination), a CostFunction, a position, reserves and an access list of Entities. It buys and sells on the same cost function, within domain and reserve limits. It cannot supply a Resource it does not hold. Each round's fills are ordinary couplings whose legs post at the cleared rate.

### 5.3 Routing

Routing plans how an order placed for a base and quote pair is split across the Markets for that pair (`Core.Kernel.Route`). Its **candidates** are the accessible Markets that match the pair and for which the party holds the required capability; with none, it is rejected at screening. It must carry a limit price, which is its escrow ceiling for a buy. **Merit order** takes candidates by opening marginal price (lowest first to buy, highest first to sell, ties by Market id). The **split** is planned in canonical order: each candidate in merit order takes the largest quantity whose batch price stays within its cap, the limit or the next candidate's opening price if tighter.

- **Guarantees.** The split is deterministic and order-independent ([INV-11](#inv-11)). A party's fills sum to at most its quantity, and no Market is filled beyond its domain or reserves ([INV-06](#inv-06)).
- **Non-guarantees.** The split does not equalise marginal prices across Markets. An order removed at clearing can leave a slice unfilled rather than re-routed.

### 5.4 Rationing

Rationing divides one limit among many claims (`Core.Kernel.Ration`). A claim carries a **tier**, whether it is **whole** or scaled, and a **scale group** whose claims scale together. Claims fill tier by tier. A whole claimant that overflows a Resource loses every whole claim on that Resource. The rest of a tier scales, and everything in one scale group scales by the group's tightest ratio.

A **pool** shares what one Resource can supply. Whole orders fill in full, or, if together they overflow it, all go. The rest share what remains pro rata, in whole units of what is drawn, each floored to the grid so that no order draws more than its share.

### 5.5 Simultaneous clearing

All orders in a round are resolved as one batch, and each Market clears at one uniform price (`Core.Kernel.Clear`). The steps are:

1. Orders whose access rules fail are rejected, and the remaining purchases and sales are netted.
2. Residual net demand is executed against the cost function, within domain and reserve limits.
3. The uniform batch price is the average marginal cost over the net quantity, or the current marginal price when net demand is zero. It is rounded before it multiplies quantities ([§9.1](#91-numerics-and-randomness)).
4. Orders whose limit is violated at that price are removed, and the price is recomputed. Removed orders stay removed, so the procedure terminates and depends only on the set of orders. Partial fills follow [§3.6](#36-infeasible-actions).
5. Holdings and reserves change as one net update.

The result does not depend on the order of submissions ([INV-11](#inv-11)). A settlement event is a **Coupled flow**: its legs settle together at one exact stored rate, when it has one.

## 6. Economic Circulation

Every Resource stock reconciles each round:

**Closing stock = Opening stock + Production + Issuance − Consumption − Destruction − Decay**

Here Consumption is the use of a Resource, not the consumption of an obligation. Moves between accounts sum to zero, including Market reserves ([INV-05](#inv-05)). A Hold moves nothing, so it has no term here. Delayed movements count as production or destruction of their Resource. The authored recurring cycle that moves quantities between holders each round is in [03 §5.4](03_gameboard.md#54-recurring-cycle).

## 7. Aggregates and Representation

This section defines how explicit Actors influence an implicit Aggregate. A **Representation** treats an Actor's behavior as a sample of the population and adjusts the baseline. A collective decision is authored ([03 §6.3](03_gameboard.md#63-aggregates)). Neither gives the Actor the Aggregate's resources or authority.

### 7.1 Aggregate accounts

An Aggregate's economic accounts are its residual: the accounts and behavior of its population that explicit Actors do not hold. Instantiation partitions them ([04 §4.2](04_instantiated_game.md#42-partitioning)).

### 7.2 Representation and estimators

A Representation links an explicit Actor to an Aggregate. It names the behavior domain it adjusts, the Resource whose purchases it observes, the Actor's weight in the combination, and an estimator. The estimator has a reference behavior, a shrinkage (1 means the deviation is fully applied), an optional bound on the deviation, and a noise spread (zero means none). The domain limits which part of the Actor's behavior may influence the Aggregate. A Representation never transfers holdings or capabilities.

### 7.3 Representation pipeline

The engine records four stages: **observation** (round r, what the Actor set out to buy of the observed Resource, read from its validated input before funding and not what it executed), **estimate** (round r, the stored adjustment factor), the **proposed Aggregate input** (round r + 1, the Aggregate Policy's adjusted decision), and the **executed result** (round r + 1). Any difference between intended and executed actions is recorded.

### 7.4 Estimation semantics

Representation is an inference, never a resource multiplier. Each representative's deviation from its reference is perturbed and then clamped:

**Deviationᵢ = clamp(deviationBound, (observedᵢ − referenceᵢ) / referenceᵢ + εᵢ)**, where **εᵢ = noiseSpreadᵢ × (2uᵢ − 1)** and uᵢ is the seeded draw for that input ([§9.1](#91-numerics-and-randomness)).

Noise models the uncertainty of inferring a population from a few representatives. It is added before clamping, so the bound still caps the effect. A zero reference contributes no estimate.

**Adjustment factor = 1 + Σ(shrinkageᵢ × weightᵢ × deviationᵢ)**

The sum runs over the Representations targeting that Aggregate and domain. Weights account for overlapping and possibly biased samples, so representatives are not treated as independent. The factor is stored in World State. In round r + 1 the Aggregate's Policy proposes **Adjusted target = Baseline × stored factor**, with the baseline of round r + 1, applied exactly once. With no observation in the previous round, no factor is stored and the Aggregate follows its baseline.

An Actor's direct transactions execute once, from its own accounts. The adjusted behavior replaces the matching baseline and is never added to a complete baseline ([INV-09](#inv-09)). A representative's decision has two validated effects, direct and estimated, and neither is counted twice.

## 8. World State and Round Resolution

This section defines the state the engine advances and the one sequence that advances it. World State is the complete economic state at a round boundary, and it holds the pending couplings and templates ([§3.4](#34-successors)). Policies read it and never mutate it. They only submit input. The engine advances the world through one deterministic resolution, from World State and the round's input to the next World State.

### 8.1 Canonical resolution sequence

This is the single authoritative sequence. The other layers reference it and do not redefine it. It is authored vocabulary: `resolveRound` (`packages/rules`) runs it, and it calls the kernel in these phases, with each leg posted through `Core.Kernel.Post` ([§3.5](#35-resolution)).

1. **Prepare** (engine): consumes every pending coupling due at the boundary, in phase then tier order, posting its legs, applying its changes to the laws, and spawning its successors ([§3.4](#34-successors)).
2. **Observe** (engine): produces the beginning-of-round Snapshot and each Entity's View. Every decision is made on it.
3. **Propose and sign** (round runner): every Policy submits its input against the Snapshot at the same time; then each party consents to what it endorses. The set is frozen at the end of Propose ([03 §10.1](03_gameboard.md#101-proposal-rules)).
4. **Screen and interpret** (engine): checks authority and each consent condition, and decides the static refusals ([03 §12.5](03_gameboard.md#125-static-refusals)). What fails is recorded. Admissible observations are extracted from the validated input. Nothing is filled yet.
5. **Estimate** (engine): computes each Representation's adjustment factor from this round's validated input and stores it. It creates and modifies no input ([§7.4](#74-estimation-semantics)).
6. **Resolve economy** (engine): funds every accepted claim from the Snapshot, then settles and clears them as one simultaneous batch. Markets clear at uniform prices. Couplings and the recurring cycle ([03 §5.4](03_gameboard.md#54-recurring-cycle)) are evaluated together, into one net change.
7. **Commit and record** (engine): commits the new World State atomically, appends the round's spawned couplings to `pending`, and records the round's inputs and events.

### 8.2 Funding and settlement

- **Funding from the Snapshot.** Each party's claims are funded from its holdings at the start of the round. At screening, each accepted input escrows its maximum cost. Joint feasibility and pro-rata scaling are checked against these escrows ([§3.6](#36-infeasible-actions)). Unused escrow is released at commit.
- **Credit at commit.** Proceeds and receipts are credited when the round commits and are usable only next round. Selling and buying both draw on opening holdings.

Rationing is configured by tier, whole or scaled, and scale group ([§5.4](#54-rationing)). Three shortage laws hold for every setting. The authored tiers that produce them are in [03 §10.1](03_gameboard.md#101-proposal-rules).

1. **One short Resource scales everything a scale group executes.** Its funding scale is the smallest ratio of available to claimed across all Resources it claims, and that one factor scales every scaled claim in the group.
2. **Whole claims fill first, and overflow drops them all.** They are funded before scaled claims and are never scaled. A party whose whole demand on a Resource exceeds its holding loses every whole claim on it. Scaled claims then share a pool pro rata.
3. **A quote-reserve shortage drops the whole sell side.** If a batch would overdraw a Market's quote reserves, its sell orders are removed and the batch clears on its buys alone.

Every capped leg moves at most what the payer holds after the batch so far, and any remainder is a shortfall. A negative holding or reserve fails the round as an invariant failure ([INV-04](#inv-04)).

### 8.3 Outcomes and round records

Lapses and infeasible actions are recorded as events and never fail a round. A round fails only on an invariant failure, such as a negative holding that settlement should have prevented ([INV-04](#inv-04)).

The round record and the manifest are defined in [04 Terms](04_instantiated_game.md#terms). The manifest is immutable ([INV-31](#inv-31)). Resolution returns the new World State together with the round record.

### 8.4 Effects and handlers

The engine has no effects. In-world randomness uses a pure keyed draw ([§9.1](#91-numerics-and-randomness)) with the game seed held in World State. The player seed never reaches the engine.

Everything else impure is a named effect: a capability a program declares it needs, interpreted by a handler over device primitives. The request and reply shapes are in [07 §4](07_architecture.md#4-key-interfaces).

The effects are: **Seat input** (a seat's input, consents and transcript); **Interpret** (the round's transcripts read together into input, with no confirmation in live play); **Narrate** (round text from resolved events, adding no facts); **Record** (appends a round's result to the run); **Observe** (Run Store reads, which cannot write); **Store** (Run Store lifecycle writes); **API** (every route of the dashboard's HTTP API, [06 §8.1](06_simulator.md#81-read-api-and-run-operations)); **Cache** (what a process keeps between requests); **Play** (one whole stored play); **Live** (one live session, settled at its end); and **Platform** (the clock, process identity, files, the terminal, and one operating-system number, drawn only for a player seed nobody gave).

Every program is typed by the capabilities it asks for, so its type states all it can touch. Handlers choose the context (live session, simulation, replay or test). The round runner is the same code under every context. The per-context behavior is in [04](04_instantiated_game.md) and [06](06_simulator.md).

## 9. Architectural Invariants and Numerics

The invariant table is the one numbered table for the whole system; other layers cite its IDs. IDs 18, 20 and 32–35 are retired. **Checked** means a predicate, a validation or a named test enforces the invariant today. The per-round assertion status is in [06 §7](06_simulator.md#7-run-assertions).

| ID | Invariant | Status |
|---|---|---|
| <a id="inv-01"></a>INV-01 | Unified action semantics: all participants act through the same input and validation, and every settlement names by its Cause what produced it. A settlement of a Proposal's input ([03 §10.1](03_gameboard.md#101-proposal-rules)) matches that input. | Checked |
| <a id="inv-02"></a>INV-02 | Authority separation: a party exercises only the capabilities its `Holds` conditions name, within scope. | Checked |
| <a id="inv-03"></a>INV-03 | No authority amplification: membership, Representation, collective decisions and fallbacks never confer or remove capabilities or holdings. A round never changes an Entity's capabilities or kind. A Successor names only parties bound from its parent. | Checked |
| <a id="inv-04"></a>INV-04 | Resource consistency: holdings change only by a recorded leg. Each Entity's holdings change by exactly what the round's legs say. No holding or Market reserve is negative. | Checked |
| <a id="inv-05"></a>INV-05 | Stock-flow reconciliation: every Resource stock reconciles each round ([§6](#6-economic-circulation)): its change in total equals what legs created from `Source` less what they sent to `Sink`. Moves net to zero, and no account accumulates without a declared outflow. | Checked |
| <a id="inv-06"></a>INV-06 | Market solvency: Market positions stay within their cost domain, and pool fills never exceed what the pool can draw or its escrowed ceiling. | Checked |
| <a id="inv-07"></a>INV-07 | Price is not production: no leg a Market curve or a plain Transfer settles crosses the economy's boundary. Only an authored recipe, a Rule or a recurring flow may create or remove a Resource. | Checked |
| <a id="inv-08"></a>INV-08 | Account partition: every account and behavioral responsibility has exactly one owner, however memberships overlap. A Resource drawn from its holder has at most one positive holder. | Checked |
| <a id="inv-09"></a>INV-09 | Representation consistency: a stored estimate replaces the matching baseline exactly once, in the following round, within estimator bounds, and never duplicates direct activity. | Checked |
| <a id="inv-10"></a>INV-10 | Consent completeness: an obligation stands only if its `Consent` or `WeightedConsent` condition holds. A coupling bound whole settles atomically or is rejected together. | Checked |
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
| <a id="inv-37"></a>INV-37 | Obligation consumption: every obligation due in a round is consumed exactly once, and every spawn is due in a later round ([§3.4](#34-successors)). | Checked |

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

`packages/core` holds the kernel and the numerics, curve, draw and description algebra it needs. It names no agreement, institution or schedule, and `scripts/check/layout.js` keeps it so. Everything a setting names is authored: Intents, Proposals and their consent rules, Rules, recurring flows, Facilities and the capability kinds compile to the kernel at the authoring layer ([03 §14](03_gameboard.md#14-compilation-to-the-kernel)). Two kinds of thing stay closed in core: the solvers ([§5](#5-cost-functions-markets-and-solvers)), because they divide one limit among many, and the invariant that a stock changes only by a leg ([§3.1](#31-stocks-flows-and-legs)).

Entity, World State, Representation and the settlement sequence are shared by every setting, and this document specifies them, but their code is in `packages/rules`, beside the vocabulary they read ([07 §3](07_architecture.md#3-codebase-layout)).

The type system does not model resource classifications, a macroeconomic model, estimator parameters, procedure thresholds or market realism. These are choices each setting makes, and the design principles behind them are in [01](01_overview.md).
