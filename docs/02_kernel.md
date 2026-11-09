# Kernel Specification

## 1. Purpose

The kernel says what any World can be made of: Stocks, Flows, Laws over them, and Transactions of conditional Obligations ([§3](#3-the-kernel)). A PremiseComposition describes a set of possible Worlds in those terms ([03](03_premise.md)), and a Run realizes one of them ([04](04_run.md)). This document specifies the kernel and the economic types every PremiseComposition shares. The kernel decides no trade, signature, procedure or schedule: those are vocabulary a PremiseComposition authors, and they compile to kernel values ([03 §14](03_premise.md#14-compilation-to-the-kernel)). Explicit Actors, Aggregates and implicit populations act through the same kernel in one deterministic economy. The engine is pure: every impure act is a named effect ([§8.4](#84-effects-and-handlers)).

The anchors are defined in [01 Terms](01_overview.md#terms). The reference PremiseComposition is in [05](05_meridian.md), and the simulator in [06](06_simulator.md).

## 2. Core Ontology

The sections below specify the anchors and the types built on them: Actor and its three projections ([§2.1](#21-actor), [§2.2](#22-three-projections-of-an-actor)); Resource and accounts ([§2.3](#23-resource)); Capability ([§2.4](#24-capability)); Stock, Flow, Law, Transaction, Obligation and successors ([§3](#3-the-kernel)); cost functions, Exchange and the solvers ([§5](#5-cost-functions-exchanges-and-solvers)); Aggregate and Representation ([§7](#7-aggregates-and-representation)); Strategy ([§4](#4-strategies)); and World ([§8](#8-world-and-round-resolution)). Which agreements, institutions and schedules a PremiseComposition has are authored, not core ([§10](#10-design-boundaries)).

### 2.1 Actor

Every participant, explicit or implicit, is one Actor with an identifier, accounts, capabilities and a kind.

Its kind is Playable Actor, non-playable Actor or Aggregate. Playable and non-playable Actors are represented explicitly and driven by a controller: for a Playable Actor a seat, whose Strategy is supplied from outside the PremiseComposition, and for a non-playable Actor a Strategy that is part of the PremiseComposition. An Aggregate has a baseline Strategy and descriptive members ([§2.2](#22-three-projections-of-an-actor)). All three own accounts and act through the same kernel. They differ only in how their decisions are produced. Meridian's are in [05 §9](05_meridian.md#9-actor-catalog) and [§10](05_meridian.md#10-aggregate-catalog).

### 2.2 Three projections of an Actor

Membership, economic ownership and collective decision-making are distinct relations. The kernel keeps them separate rather than letting "Aggregate" carry all three.

| Projection | Rule |
|---|---|
| **Membership** | Descriptive. It may overlap with other Aggregates and confers no ownership or authority. |
| **Account ownership** | Every account and behavioral responsibility has exactly one owner ([INV-08](#inv-08)). |
| **Collective decision-making** | Authority belongs to the institutional Actor, never to members by virtue of membership ([INV-03](#inv-03)). How an Actor decides is authored ([03 §6.3](03_premise.md#63-aggregates)). |

Membership never lets several Aggregates own the same assets.

### 2.3 Resource

A Resource's transferability is Free (it moves on its holder's consent alone), Restricted (each movement needs the transfer capability, [§2.4](#24-capability)), or Non-transferable. A category is a PremiseComposition's own grouping, which the engine never reads. A Resource that nothing authored produces or consumes is conserved. Money, the numeraire, is the unit of account for views; the engine does not read it, since a settled Flow names its own Resource.

An account is a quantity of a Resource owned by exactly one Actor or Exchange, and a holding is the quantity an Actor or an Exchange's reserves hold.

Not everything economically meaningful is a Resource. Exchange access and institutional authority are state or laws attached to Actors and Exchanges. Holdings are distinct from capabilities: influence over a law does not let an Actor spend, transfer or create what the law affects.

### 2.4 Capability

The kernel asks only whether a party holds a Capability, through a `Holds` condition ([§3.3](#33-transactions-and-conditions)). It does not read what a capability means. The kinds of capability are authored ([03 §6](03_premise.md#6-actor-catalog)).

Institutional capabilities stay with the institutional Actor or with a declared law. If an institution is not played explicitly, its capabilities stay with its Aggregate and never move to a population Aggregate. Membership, Representation and taking part in a collective decision never transfer capabilities to a member or representative.

## 3. The Kernel

The kernel is what core offers: stocks, flows, laws on them, and transactions of conditional obligations. A Premise's vocabulary compiles to it ([03 §14](03_premise.md#14-compilation-to-the-kernel)). Every settled Flow of every settlement is posted through one place ([§3.1](#31-stocks-flows-and-settled-flows)). The kernel never authors or rewrites a decision and never knows how one was produced ([§4](#4-strategies)).

### 3.1 Stocks, flows, and settled Flows

A Stock sits at a holder or in an Exchange's reserve. The holder of a Resource is the Actor that answers for it: the one with a positive holding, else the first in id order with an account. A Flow's endpoints are `Held` (an Actor's account), `InExchange` (an Exchange's reserve), `Source` (the economy's boundary, where a Resource is created) and `Sink` (the boundary, where it ends).

**A stock changes only by a settled Flow.** `Core.Kernel.Post` is the only code that writes the ledger, and `scripts/check/layout.js` enforces that. A settled Flow honours its obligation's shortfall rule:

- Cap moves what the source holds, recording owed and moved.
- Reject moves nothing when the whole cannot move.

A shortfall is `moved < owed` on a settled Flow: an amount owed but not delivered, because the source's holdings fell short.

The boundary can always pay, and so can a reserve: clearing bounds a batch's net, and a negative closing reserve fails the round at commit ([INV-04](#inv-04)). A settled Flow that moves nothing still writes its accounts. `crossesBoundary` classifies a settled Flow as crossing the boundary (from `Source` or to `Sink`) or conserving ([INV-07](#inv-07)).

### 3.2 Laws

Stocks and Flows take Laws in parallel: a Law's subject is one or the other, and its trigger is time or an action.

| Subject | Trigger: each round | Trigger: on an action |
|---|---|---|
| Stock | decay, depreciation, interest | wear from use |
| Flow | delay, expiry | levy, block |

The kernel has no concept of decay, depreciation, tariff or tax. Those are values the authoring layer writes ([03 §15](03_premise.md#15-authoring-laws-and-agreements)). The table names patterns, not kernel kinds. Not every cell is used yet: in the shipped engine a stock law runs only each round, and a flow law only on the action that moves it.

A law's operation is one of three:

- Emit a settled Flow, priced from the subject's quantity. A law that emits keeps the stock-flow identity ([INV-05](#inv-05)): depreciation is a settled Flow from the stock to `Sink`.
- Refuse the action.
- Withhold a portion: it cannot be drawn, and no holding changes.

A Law also has a guard of conditions, a phase and a cause. A cause says what produced a settlement: the kernel carries it as data and does not read it, and the authored vocabulary says what it names. `Core.Kernel.Law` applies a law to one holder and matches a flow against its scope.

### 3.3 Transactions and conditions

A Transaction holds its Obligations, a rationing configuration ([§5.4](#54-rationing)), a phase, a cause, and the parties who have consented. An Obligation's effect is a settled Flow or a change to a Law (put a Law in force, or remove one). A change to a Law can be bound like any other effect, so a treaty can bind "I remove this levy" to "you pay this amount".

Each obligation carries conditions, each with what its failure reports:

| Condition | Holds when |
|---|---|
| `Performed` | Another obligation of the transaction is still standing, bound by a `Whole` or `Proportional` bond. |
| `StockAtLeast` | A stock holds at least an amount. |
| `Consent` | The named parties have signed the transaction. |
| `WeightedConsent` | Residual weight plus the signed weights exceed a threshold. |
| `PriceWithin` | The cleared price is within a limit. |
| `QuantityAtLeast` | The obligation's quantity is at least an amount. |
| `Holds` | A party holds a capability ([§2.4](#24-capability)). |

A `Performed` condition says whether one obligation applies with another. An obligation with no such condition stands alone, and one with it is bound Whole (all or nothing) or Proportional at a rate. Mutual `Performed … Whole` conditions make a transaction atomic with no special case: if one party cannot perform, the others' conditions fail on the next pass ([§3.5](#35-resolution)). `onShort` is a separate question: Reject or Cap says what a shortfall does ([§3.1](#31-stocks-flows-and-settled-flows)).

What the shipped sequence uses differs from what the kernel can express. `resolve` reads a `Performed` condition only as the other obligation still standing, so `Proportional` carries its rate as data and a proportional fill is decided by the clearing solver. The sequence decides a limit price and an order minimum itself and passes no price to the kernel, so `PriceWithin` and `QuantityAtLeast` hold unconditionally in its calls, and the kernel's own tests exercise them.

### 3.4 Successors

An obligation lives for one round. Consuming it can spawn new obligations, so debt, vesting, schedules and default clauses are patterns the authoring layer writes ([03 §15.2](03_premise.md#152-multi-round-agreements)). Core has no concept of a loan.

- **Three outcomes.** Every due obligation is consumed exactly once as performed (`Fulfilled`), short (`Shorted`) or lapsed (`Expired`, removed because a condition failed) ([INV-37](#inv-37)). `next` lists what each outcome spawns.
- **Successors are data.** A spawn names a template, a due offset of at least one round, frozen arguments and bound parties. A template may spawn itself, which expresses "until repaid".
- **Spawned at commit, for a later round.** `Core.Kernel.Consume` instantiates each spawn for a later round, so a round stays finite and resolution is undisturbed. A spawn that fails this check (a due offset below one, an unknown template, a missing party, or a party outside its parent) produces no successor.
- **Amounts freeze at consumption.** Arguments are computed from the consumed obligation (`Moved`, `Remainder`) and stored as plain values.
- **Consent is given once.** A successor carries its parent's consent. A template may name only parties bound from its parent ([INV-03](#inv-03)). A renegotiable step adds a fresh `Consent` condition, which holds only when the party's consent was carried from the parent ([03 §15.2](03_premise.md#152-multi-round-agreements)).
- **Entering during play.** The `Enter` Intent compiles to a pending transaction: the first step of the template it names, due next round, carrying its signers' consent, with the cause `OfAgreement` ([03 §14](03_premise.md#14-compilation-to-the-kernel), [§15.3](03_premise.md#153-agreements-in-a-premise-and-in-play)).

The World holds one list of pending transactions and a table of templates. A pending transaction due in phase 0 is consumed at the round boundary, in tier order. One in a later phase claims holdings beside that round's orders and is rationed against them by its tier ([§5.4](#54-rationing)): tier 0 fills with joint orders, ahead of single orders in tier 1, and a higher tier takes what they leave. A junior tier's shortage never scales a senior one.

### 3.5 Resolution

`Core.Kernel.Resolve` resolves transactions by the procedure [§5.5](#55-simultaneous-clearing) uses for limit orders. Phase by phase, every obligation starts admissible, and whatever fails its conditions is removed. Then conditions are recomputed against the set still standing. Removed obligations stay removed, so resolution terminates.

Passes are order-independent ([INV-11](#inv-11)): every condition of a pass reads only the set the pass began with, and survivors post in canonical order. A transaction's phase is the order its settled Flows take within `resolve`, lowest first. When `resolve` is given several transactions, it runs them phase by phase.

The round's whole sequence belongs to the authored vocabulary, and it calls the kernel in stated phases ([§8.1](#81-canonical-resolution-sequence)). There is deliberately no single kernel call for a whole round: what a settlement records is decided per mechanism, some transactions can only be built once earlier phases have run, and a measured amount reads this round's earlier settled Flows. The sequence runs one transaction at a time, at the position §8.1 states, and every settled Flow goes through `Core.Kernel.Post`.

### 3.6 Infeasible actions

Infeasible actions are rejected or partially executed by deterministic rules. The engine never invents resources or authority to make an action succeed.

- A pro-rata share is executed when a party's claims jointly exceed its opening resources, and when demand exceeds supply or what can be drawn ([§5.4](#54-rationing)).
- Indivisible remainders go to the largest remainders, ties in canonical id order, never by submission order ([§9.1](#91-numerics-and-randomness)).
- Orders whose limit price is violated at the batch price are removed at clearing ([§5.5](#55-simultaneous-clearing)).
- A transaction whose obligations are bound whole settles atomically, or is rejected and recorded as a unit.

## 4. Strategies

This section says what a Strategy sees and returns. A Strategy produces the engine's input from the authorized View of the Actor it drives, as a function of the View and the turn. The authored form of that input, the decision language, and the declarative `StrategyRules` are in [03 §8](03_premise.md#8-strategy-catalog) and [03 §10.4](03_premise.md#104-available-actions).

**Strategy + View + Turn → input**

A View is what a Strategy sees at the start of a round: its own Actor, prices, the laws in force, transferability, the events so far, stored adjustment factors, the input awaiting its consent, the pending transactions it owes or is owed ([§3.4](#34-successors)), the Exchanges it can access, the templates that can be entered, and the known Actors and the authored actions and bounds that apply to them. A Strategy never sees the World.

### 4.1 Strategy kinds

A Strategy is declarative (`StrategyRules` run on the shared interpreter: non-playable Actor rules, Aggregate baselines and seat presets), an AI agent (a language model given the role's briefing and View), or a human (whose text is interpreted into input). Their replay is specified in [06 §4](06_simulator.md#4-seat-strategies) and [04 §7.3](04_run.md#73-seeds-and-randomness).

### 4.2 Strategy rules

An initialized Strategy operates only on its authorized View, produces typed input, and stays deterministic. Its definition is fixed for the Run, though its outputs vary with the World it reacts to.

A seat's Strategy is bound at run time. Swapping it never changes the Actor's definition ([INV-30](#inv-30)). A Strategy never rewrites any Actor's Strategy.

A Strategy keeps no memory of its own: it reads the previous round's events and stored factors.

### 4.3 Aggregate baselines

Every Aggregate has a baseline Strategy for its implicit population. It decides when to act in the Aggregate's name. It reads the factors stored last round and scales the matching baseline by them ([§7.4](#74-estimation-semantics)). With none stored, it follows its baseline.

## 5. Cost Functions, Exchanges, and Solvers

This section defines how quantity is priced and exchanged, and what stays closed in core. Pricing never creates an asset ([INV-07](#inv-07)). Two solvers cannot be written as conditions, because they divide one limit among many: clearing, and routing and rationing. They are a closed set, selected by data.

### 5.1 Cost function

A cost function is a linear cumulative cost curve. Its marginal price at cumulative position q is p(q) = intercept + slope·q, and moving the position by Δq costs C(q + Δq) − C(q). It has a supported domain. An Exchange's position persists across rounds, and an authored price curve's position is its total for the current round.

### 5.2 Exchanges

An Exchange's terms are data read each Round rather than a value that is consumed, which makes it a standing Transaction. It has a base Resource (the good), a quote Resource (the denomination), a cost function, a position, reserves and an access list of Actors. It buys and sells on the same cost function, within domain and reserve limits. It cannot supply a Resource it does not hold. Each round's fills are ordinary transactions whose settled Flows post at the cleared rate.

### 5.3 Routing

Routing plans how an order placed for a base and quote pair is split across the Exchanges for that pair (`Core.Kernel.Route`). Its candidates are the accessible Exchanges that match the pair and for which the party holds the required capability; with none, it is rejected at screening. It must carry a limit price, which is its escrow ceiling for a buy. Merit order takes candidates by opening marginal price (lowest first to buy, highest first to sell, ties by Exchange id). The split is planned in canonical order: each candidate in merit order takes the largest quantity whose batch price stays within its cap, the limit or the next candidate's opening price if tighter.

- **Guarantees.** The split is deterministic and order-independent ([INV-11](#inv-11)). A party's fills sum to at most its quantity, and no Exchange is filled beyond its domain or reserves ([INV-06](#inv-06)).
- **Non-guarantees.** The split does not equalise marginal prices across Exchanges. An order removed at clearing can leave a slice unfilled rather than re-routed.

### 5.4 Rationing

Rationing divides one limit among many claims (`Core.Kernel.Ration`). A claim carries a tier, whether it is whole or scaled, and a scale group whose claims scale together. Claims fill tier by tier, lowest first. A whole claimant that overflows a Resource loses every whole claim on that Resource. The rest of a tier scales, and everything in one scale group scales by the group's tightest ratio.

A pool shares what one Resource can supply. Whole orders fill in full, or, if together they overflow it, all go. The rest share what remains pro rata, in whole units of what is drawn, each floored to the grid so that no order draws more than its share.

### 5.5 Simultaneous clearing

All orders in a round are resolved as one batch, and each Exchange clears at one uniform price (`Core.Kernel.Clear`). The steps are:

1. Orders whose access rules fail are rejected, and the remaining purchases and sales are netted.
2. Residual net demand is executed against the cost function, within domain and reserve limits.
3. The uniform batch price is the average marginal cost over the net quantity, or the current marginal price when net demand is zero. It is rounded before it multiplies quantities ([§9.1](#91-numerics-and-randomness)).
4. Orders whose limit is violated at that price are removed, and the price is recomputed. Removed orders stay removed, so the procedure terminates and depends only on the set of orders. Partial fills follow [§3.6](#36-infeasible-actions).
5. Holdings and reserves change as one net update.

The result does not depend on the order of submissions ([INV-11](#inv-11)). A `Settled` event records one settlement: its settled Flows settle together at one exact stored rate, when it has one.

## 6. Economic Circulation

Every Resource stock reconciles each round:

**Closing stock = Opening stock + Production + Issuance − Consumption − Destruction − Decay**

Here Consumption is the use of a Resource, not the consumption of an obligation. Moves between accounts sum to zero, including Exchange reserves ([INV-05](#inv-05)). A hold moves nothing, so it has no term here. Delayed movements count as production or destruction of their Resource. The authored recurring cycle that moves quantities between holders each round is in [03 §5.4](03_premise.md#54-recurring-cycle).

## 7. Aggregates and Representation

This section defines how explicit Actors influence an implicit Aggregate. A Representation treats an Actor's behavior as a sample of the population and adjusts the baseline. A collective decision is authored ([03 §6.3](03_premise.md#63-aggregates)). Neither gives the Actor the Aggregate's resources or authority.

### 7.1 Aggregate accounts

An Aggregate's economic accounts are its residual: the accounts and behavior of its population that explicit Actors do not hold. Instantiation partitions them ([04 §4.2](04_run.md#42-partitioning)).

### 7.2 Representation and estimators

A Representation links an explicit Actor to an Aggregate. It names the behavior domain it adjusts, the Resource whose purchases it observes, the Actor's weight in the combination, and an estimator. The estimator has a reference behavior, a shrinkage (1 means the deviation is fully applied), an optional bound on the deviation, and a noise spread (zero means none). The domain limits which part of the Actor's behavior may influence the Aggregate. A Representation never transfers holdings or capabilities.

### 7.3 Representation pipeline

The engine records four stages: observation (round r, what the Actor set out to buy of the observed Resource, read from its validated input before funding and not what it executed), estimate (round r, the stored adjustment factor), the proposed Aggregate input (round r + 1, the Aggregate Strategy's adjusted decision), and the executed result (round r + 1). Any difference between intended and executed actions is recorded.

### 7.4 Estimation semantics

Representation is an inference, never a resource multiplier. Each representative's deviation from its reference is perturbed and then clamped:

**Deviationᵢ = clamp(deviationBound, (observedᵢ − referenceᵢ) / referenceᵢ + εᵢ)**, where **εᵢ = noiseSpreadᵢ × (2uᵢ − 1)** and uᵢ is the seeded draw for that input ([§9.1](#91-numerics-and-randomness)).

Noise models the uncertainty of inferring a population from a few representatives. It is added before clamping, so the bound still caps the effect. A zero reference contributes no estimate.

**Adjustment factor = 1 + Σ(shrinkageᵢ × weightᵢ × deviationᵢ)**

The sum runs over the Representations targeting that Aggregate and domain. Weights account for overlapping and possibly biased samples, so representatives are not treated as independent. The factor is stored in the World. In round r + 1 the Aggregate's Strategy proposes **Adjusted target = Baseline × stored factor**, with the baseline of round r + 1, applied exactly once. With no observation in the previous round, no factor is stored and the Aggregate follows its baseline.

An Actor's direct transactions execute once, from its own accounts. The adjusted behavior replaces the matching baseline and is never added to a complete baseline ([INV-09](#inv-09)). A representative's decision has two validated effects, direct and estimated, and neither is counted twice.

## 8. World and Round Resolution

This section defines the state the engine advances and the one sequence that advances it. The World holds the pending transactions and templates ([§3.4](#34-successors)). Strategies read it and never mutate it. They only submit input. The engine advances the world through one deterministic resolution, from the World and the round's input to the next World.

### 8.1 Canonical resolution sequence

This is the single authoritative sequence. The other docs reference it and do not redefine it. It belongs to the authored vocabulary: `resolveRound` (`packages/laws`) runs it, and it calls the kernel in these phases, with each settled Flow posted through `Core.Kernel.Post` ([§3.5](#35-resolution)).

1. **Prepare** (engine): consumes every pending transaction due at the boundary, in phase then tier order, posting its settled Flows, applying its changes to the laws, and spawning its successors ([§3.4](#34-successors)).
2. **Observe** (engine): produces the opening World (the World after Prepare) and each Actor's View. Every decision is made on it.
3. **Propose and sign** (round runner): every Strategy submits its input against the opening World at the same time; then each party consents to what it endorses. The set is frozen at the end of Propose ([03 §10.1](03_premise.md#101-candidate-transaction-rules)).
4. **Screen and interpret** (engine): checks authority and each consent condition, and decides the static refusals ([03 §12.4](03_premise.md#124-static-refusals)). What fails is recorded. Admissible observations are extracted from the validated input. Nothing is filled yet.
5. **Estimate** (engine): computes each Representation's adjustment factor from this round's validated input and stores it. It creates and modifies no input ([§7.4](#74-estimation-semantics)).
6. **Resolve economy** (engine): funds every accepted claim from the opening World, then settles and clears them as one simultaneous batch. Exchanges clear at uniform prices. Transactions and the recurring cycle ([03 §5.4](03_premise.md#54-recurring-cycle)) are evaluated together, into one net change.
7. **Commit and record** (engine): commits the new World atomically, appends the round's spawned transactions to `pending`, and records the round's inputs and events.

### 8.2 Funding and settlement

- **Funding from the opening World.** Each party's claims are funded from its holdings at the start of the round. At screening, each accepted input escrows its maximum cost. Joint feasibility and pro-rata scaling are checked against these escrows ([§3.6](#36-infeasible-actions)). Unused escrow is released at commit.
- **Credit at commit.** Proceeds and receipts are credited when the round commits and are usable only next round. Selling and buying both draw on opening holdings.

Rationing is configured by tier, whole or scaled, and scale group ([§5.4](#54-rationing)). Three shortage behaviours hold for every PremiseComposition. The authored tiers that produce them are in [03 §10.1](03_premise.md#101-candidate-transaction-rules).

1. **One short Resource scales everything a scale group executes.** Its funding scale is the smallest ratio of available to claimed across all Resources it claims, and that one factor scales every scaled claim in the group.
2. **Whole claims fill first, and overflow drops them all.** They are funded before scaled claims and are never scaled. A party whose whole demand on a Resource exceeds its holding loses every whole claim on it. Scaled claims then share a pool pro rata.
3. **A quote-reserve shortage drops the whole sell side.** If a batch would overdraw an Exchange's quote reserves, its sell orders are removed and the batch clears on its buys alone.

Every capped settled Flow moves at most what the payer holds after the batch so far, and any remainder is a shortfall. A negative holding or reserve fails the round as an invariant failure ([INV-04](#inv-04)).

### 8.3 Outcomes and round records

Lapses and infeasible actions are recorded as events and never fail a round. A round fails only on an invariant failure, such as a negative holding that settlement should have prevented ([INV-04](#inv-04)).

The round record is in [04 §11.1](04_run.md#111-round-record) and the Run manifest in [04 §9](04_run.md#9-runtime-state-and-lifecycle). The manifest is immutable ([INV-31](#inv-31)). Resolution returns the new World together with the round record.

### 8.4 Effects and handlers

The engine has no effects. In-world randomness uses a pure keyed draw ([§9.1](#91-numerics-and-randomness)) with the game seed held in the World. The player seed never reaches the engine.

Everything else impure is a named effect: an operation a program declares it needs, interpreted by a handler over device primitives. The request and reply shapes are in [07 §4](07_architecture.md#4-key-interfaces).

The effects are: Seat input (a seat's input, consents and transcript); Interpret (the round's transcripts read together into input, with no confirmation in live play); Record (appends a round's result to the run); Observe (Run Store reads, which cannot write); Store (Run Store lifecycle writes); API (every route of the HTTP API, [06 §8.1](06_simulator.md#81-read-api-and-run-operations)); Cache (what a process keeps between requests); Play (one whole stored play); Live (one live session, settled at its end); Sessions (the live sessions a server hosts); and Platform (the clock, process identity, files, the terminal, and one operating-system number, drawn only for a player seed nobody gave).

Every program is typed by the effects it asks for, so its type states all it can touch. Handlers choose the context (live session, simulation, replay or test). The round runner is the same code under every context. The per-context behavior is in [04](04_run.md) and [06](06_simulator.md).

## 9. Architectural Invariants and Numerics

The invariant table is the one numbered table for the whole system; other docs cite its IDs. IDs 12, 15, 16, 18, 20, 22 and 32–35 are retired. Checked means a predicate, a validation or a named test enforces the invariant today. The per-round assertion status is in [06 §7](06_simulator.md#7-run-assertions).

| ID | Invariant | Status |
|---|---|---|
| <a id="inv-01"></a>INV-01 | Unified action semantics: all participants act through the same input and validation, and every settlement names by its Cause what produced it. A settlement of a candidate Transaction's input ([03 §10.1](03_premise.md#101-candidate-transaction-rules)) matches that input. | Checked |
| <a id="inv-02"></a>INV-02 | Authority separation: a party exercises only the capabilities its `Holds` conditions name, within scope. | Checked |
| <a id="inv-03"></a>INV-03 | No authority amplification: membership, Representation, collective decisions and fallbacks never confer or remove capabilities or holdings. A round never changes an Actor's capabilities or kind. A successor names only parties bound from its parent, or parties who signed an `Enter` that round. | Checked |
| <a id="inv-04"></a>INV-04 | Resource consistency: holdings change only by a recorded settled Flow. Each Actor's holdings change by exactly what the round's settled Flows say. No holding or Exchange reserve is negative. | Checked |
| <a id="inv-05"></a>INV-05 | Stock-flow reconciliation: every Resource stock reconciles each round ([§6](#6-economic-circulation)): its change in total equals what settled Flows created from `Source` less what they sent to `Sink`. Moves net to zero, and no account accumulates without a declared outflow. | Checked |
| <a id="inv-06"></a>INV-06 | Exchange solvency: Exchange positions stay within their cost domain, and pool fills never exceed what the pool can draw or its escrowed ceiling. | Checked |
| <a id="inv-07"></a>INV-07 | Price is not production: no settled Flow an Exchange curve or a plain Transfer settles crosses the economy's boundary. Only an authored recipe, a Rule or a recurring flow may create or remove a Resource. | Checked |
| <a id="inv-08"></a>INV-08 | Account partition: every account and behavioral responsibility has exactly one owner, however memberships overlap. A Resource drawn from its holder has at most one positive holder. | Checked |
| <a id="inv-09"></a>INV-09 | Representation consistency: a stored estimate replaces the matching baseline exactly once, in the following round, within estimator bounds, and never duplicates direct activity. | Checked |
| <a id="inv-10"></a>INV-10 | Consent completeness: an obligation stands only if its `Consent` or `WeightedConsent` condition holds. A transaction bound whole settles atomically or is rejected together. | Checked |
| <a id="inv-11"></a>INV-11 | Order independence: all actions resolve as one bundled update with uniform clearing. Permuting submissions never changes settlement. | Checked |
| <a id="inv-13"></a>INV-13 | Deterministic replay: the same manifest, seeds and recorded seat input reproduce identical Worlds. Every random value is a keyed draw. | Checked |
| <a id="inv-14"></a>INV-14 | Resolution consistency: instantiating a world at different levels of explicitness reconciles accounts and authority before any decision differs. | Checked |
| <a id="inv-17"></a>INV-17 | Type-system conformity: all Premise definitions use supported types and operations. | Checked |
| <a id="inv-19"></a>INV-19 | Role consequence: every Actor definition declares a capability, a dependency and a measurable objective. | Checked |
| <a id="inv-21"></a>INV-21 | Instantiation feasibility: the PremiseComposition admits at least one valid initial World. | Checked |
| <a id="inv-23"></a>INV-23 | Unique control: every explicit Actor has exactly one valid controller. | Checked |
| <a id="inv-24"></a>INV-24 | Valid participation: every selected Actor and Representation is permitted by the PremiseComposition and validly parameterized. | Checked |
| <a id="inv-25"></a>INV-25 | Fallback completeness: every unselected Actor has a permitted fallback, and no required responsibility is left unowned. | Checked |
| <a id="inv-26"></a>INV-26 | Strategy completeness: every non-playable Actor and Aggregate has an executable, initialized Strategy. | Checked |
| <a id="inv-27"></a>INV-27 | Exchange feasibility: initial Exchange positions, reserves and holdings satisfy economic constraints. | Checked |
| <a id="inv-28"></a>INV-28 | Temporal consistency: round intervals and Strategy timing fit the simulated horizon. | Checked |
| <a id="inv-29"></a>INV-29 | Engine and interpreter reuse: simulated Runs use the same engine, round sequence and decision-language interpreter as live Runs. | Checked |
| <a id="inv-30"></a>INV-30 | Controller integrity: swapping a seat's Strategy never changes the Actor's definition or turns a seat into a non-playable Actor. | Checked |
| <a id="inv-31"></a>INV-31 | Authoritative records: manifests, append-only round records and round-end Worlds are authoritative and immutable. Metrics are versioned projections. | Checked |
| <a id="inv-36"></a>INV-36 | Purity: the engine is a pure function. All impurity occurs only through declared effects interpreted by handlers ([§8.4](#84-effects-and-handlers)). | Checked |
| <a id="inv-37"></a>INV-37 | Obligation consumption: every obligation due in a round is consumed exactly once, and every spawn is due in a later round ([§3.4](#34-successors)). | Checked |

### 9.1 Numerics and randomness

This section sets the numeric and random rules for every economic value. Mechanics are deterministic by default. Randomness appears only where a PremiseComposition rule or estimator calls for it, such as estimator sampling noise.

**Two seed streams.** The game seed is one per Run, fixed at initialization and held in the World. It feeds every in-world draw. The player seed keys the draws of seat Strategies. It is recorded with the run and is never part of the configuration, World or the engine's input. If no player seed is given, one is drawn once and recorded.

**Draws.** A draw is a pure function of a seed and a draw key (round, source, input Actor, category and index), so randomness needs no effect. It returns a value in [0, 1) at four decimal places. Draws are keyed, not sequenced, so adding an Actor or changing evaluation order never changes unrelated draws. The algorithm is versioned in the manifest.

**Fixed point.** All quantities, prices and settlement amounts are fixed-point numbers with four decimal places. Every product or quotient off that grid is rounded half-to-even (ties go to the even last digit, symmetric in sign). No floating-point arithmetic enters economic state. A uniform batch price is rounded before it multiplies quantities.

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

`packages/core` holds the kernel and the numerics, curve, draw and description algebra it needs. It names no agreement, institution or schedule, and `scripts/check/layout.js` keeps it so. Everything a PremiseComposition names is authored: Intents, candidate Transactions and their consent rules, Rules, recurring flows, Transformations and the capability kinds compile to the kernel at the authoring layer ([03 §14](03_premise.md#14-compilation-to-the-kernel)). Two kinds of thing stay closed in core: the solvers ([§5](#5-cost-functions-exchanges-and-solvers)), because they divide one limit among many, and the invariant that a stock changes only by a settled Flow ([§3.1](#31-stocks-flows-and-settled-flows)).

Actor, World, Representation and the settlement sequence are shared by every PremiseComposition, and this document specifies them, but their code is in `packages/laws`, beside the vocabulary they read ([07 §3](07_architecture.md#3-codebase-layout)).

This document does not model resource classifications, a macroeconomic model, estimator parameters, procedure thresholds or exchange realism. These are choices each PremiseComposition makes, and the design principles behind them are in [01](01_overview.md).
