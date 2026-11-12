# Overview

## 1. Concept

This section says what the game is, who plays it, and how the world, the AI, and the engine fit together.

The Scenario Planning Game is a multiplayer economic strategy game. Players are influential actors in a shared world economy. They negotiate, form alliances, allocate resources, and make decisions that shape how the economy and politics develop. The game combines diplomacy, economic simulation, and scenario planning. At its centre is an engine that turns player decisions into consequences across interconnected exchanges.

Players may represent countries, institutions, corporations, or small economic participants. Their power is deliberately unequal. A small-business owner can still move the same economy as a head of state, through representation ([§5](#5-small-players-matter)).

The world has two parts: explicitly represented participants, and an implicit economy that models everything else. Both act through the same economic system.

Two roles sit around the engine:

- **AI interprets.** It turns what players say into structured proposed actions.
- **The engine determines outcomes.** Every result comes from the resolved economy, never from the interpreter.

Speech transcription and the newspaper are separate components, built outside this repository, that use its API ([07 §8.2](07_simulator.md#82-external-services)). One submits what a player said as text. The other reads what a round produced and writes the front page.

**The frame.** The kernel says what any World can be made of ([02](02_kernel.md)). A Premise describes part of an economy, and a PremiseComposition, an ordered list of Premises, describes a set of possible Worlds ([03](03_premise.md)). A Run realizes one of them, Round by Round ([04](04_run.md)). *Pacific* is the reference PremiseComposition ([05](05_pacific.md)). Comparing several Runs of one PremiseComposition, which differ in a shock, an override or a seed, is the scenario planning the game is for.

## Terms

These 17 anchors are the project's vocabulary, and this is the one place they are defined. Every other term in the docs is an anchor with a qualifier (a candidate Transaction, a settled Flow, a round-end World) or is explained in the section that uses it. Terms for people working on the code are in [08](08_architecture.md#implementation-terms). An anchor means the same thing whether it is capitalized or, as with a round or a stock in running prose, in lower case.

Launching and playing:

- **Premise.** An authored, composable description of part of an economy, such as an energy shock or a set of Actors. A PremiseComposition is an ordered list of Premises that together describe a set of possible Worlds and is what a Run is launched from, such as *Pacific*.
- **Run.** One realization of a PremiseComposition, played Round by Round and recorded. A derived Run starts from a stored one, and a Run set is several Runs compared together.
- **Round.** One step of a Run, in which every decision is made on the same opening World and all of them resolve as one update.
- **World.** The complete economic state at a Round boundary, including the pending Obligations, the Laws in force and the seed. A possible World is one a PremiseComposition allows, a realized World is one a Run produced, a round-end World is what a Round ends with, and world laws are the Laws in force.

Participants:

- **Actor.** The economic record of one participant: its accounts, its Capabilities and its kind. A Playable Actor is played by a human or an LLM, a non-playable Actor is run by a Strategy its PremiseComposition defines, and an Aggregate stands for an implicit population or institution.
- **Aggregate.** An Actor holding the residual accounts and baseline behaviour of an implicit population or institution.
- **Capability.** What an Actor may do, stated as a condition on a party.
- **Strategy.** Anything that decides for an Actor from what it can see: declarative rules, an AI agent or a human.

The economy:

- **Resource.** An identified economic quantity with a unit and declared behaviour, such as money, energy or a production capacity.
- **Stock.** A quantity of a Resource at a holder or in an Exchange's reserve. It changes only by a settled Flow.
- **Flow.** A quantity of a Resource moving between endpoints. A settled Flow records what was owed and what moved, and a scheduled Flow is due in a later Round.
- **Exchange.** A standing Transaction with a reserve Stock and a cleared rate, which trades the Resources it holds at one uniform price each Round.
- **Transformation.** A declared recipe that turns inputs into outputs, optionally priced by a cost curve and limited by what it can draw.

What governs change:

- **Law.** A rule over Stocks or Flows, applied each Round or on an action, that emits a settled Flow, refuses, or withholds a portion. The Rules, recurring flows and levies a Premise writes all compile to Laws.

Agreements:

- **Intent.** A typed action a party wants done: `Trade`, `Route`, `Transfer`, `Transform`, `ChangeRule` or `Enter`.
- **Transaction.** A multilateral agreement: Obligations settled together, all-or-nothing only if their conditions say so. A candidate Transaction is what a party puts forward; once signed it may still lapse, be scaled or be rejected, and it may settle as several Transactions or none.
- **Obligation.** One named party's duty for one Round, a settled Flow or a change to a Law, which stands only while its conditions hold.

## 2. Session Format

This section gives the shape of a typical session.

| Dimension | Design |
|---|---|
| Players | 10–20 |
| Session length | About 6 hours, including meals |
| Rounds | 4–8 |
| Simulated period | 5 years in the reference PremiseComposition |
| World | One shared timeline |
| Player roles | Countries, institutions, corporations, and smaller economic participants |
| Input | Out-of-band negotiation, oral announcements transcribed by a separate component, and AI-interpreted actions |
| Output | Economic outcomes, balance sheets, and a fictional financial-news front page written by a separate component |

## 3. The Round

This section describes what a player experiences in one round. Each round runs these five stages in order.

1. **Briefing.** Players receive the current state of the world.
2. **Negotiation.** Players talk freely, persuade, bluff, and strike joint candidate Transactions. This happens entirely outside the game. The game has no negotiation protocol.
3. **Submission.** Each human submits one transcript of what they intend to do this round. A trade may be described by one party, by both, or by neither. Once submitted, a player has no further input that round.
4. **Resolution.** The actions of every player, non-playable Actor, and Exchange resolve together as one update. Nothing is ordered within a round, and what a round computes is visible in the next round ([02 §8.1](02_kernel.md#81-canonical-resolution-sequence)).
5. **Reporting.** Players receive updated balance sheets. A separate component reads the round's report from the API and writes a front page describing the round.

**Propose, then sign.** Negotiation works in two steps. Each party proposes actions, then signs the candidate Transactions that name it and that it accepts. A joint candidate Transaction needs every named party's signature, so the two sides cannot disagree about its terms. A joint candidate Transaction that is missing a signature lapses. A counteroffer is a new candidate Transaction for the next round ([03 §10.1](03_premise.md#101-candidate-transaction-rules)). A transcript that describes a joint candidate Transaction counts as its speaker's agreement to it. When the interpreter reads the transcripts together, it reconciles matching descriptions into one candidate Transaction, adjusting details slightly where needed. A joint candidate Transaction that only one side describes lapses.

## 4. Explicit and Implicit Economy

This section explains which parts of the economy players decide over directly and which they experience only through its effects.

The explicit economy contains the actors whose decisions are played one by one. Governments, companies, and institutions in a given Run are chosen from the PremiseComposition. Which actors are played and which are left to the economy is a choice made per Run ([04](04_run.md)).

The implicit economy works like a backdrop: a compact representation of a much larger world. Millions of small businesses might be one aggregate that holds their combined resources and responds to conditions. It is not scenery. Its demand, supply, and investment move prices and shape the results of player decisions.

Three relationships are kept separate, so overlapping groups never double-count:

- **Membership** lists which actors belong to a group. Membership confers no authority and no holdings.
- **Account ownership** gives every account and responsibility exactly one owner.
- **Collective decisions** govern how an institution decides and what authority it holds.

For example, Germany can be an EU member, have its government's accounts played as an actor, and leave German households and firms in the implicit European economy. EU-wide authority stays with the EU's own decision procedures.

Exchanges trade existing resources at prices that rise and fall with demand. Transformations produce new goods within their capacity, and capacity can be renewed through investment ([03 §5.3](03_premise.md#53-transformations)). Money paid into production always goes to a named recipient, so shortages reflect choices and modelled scarcity, not missing accounting ([02 §6](02_kernel.md#6-economic-circulation)).

## 5. Small Players Matter

This section explains how small participants can shape a larger economy. Both mechanisms are bounded, and neither gives a player the aggregate's holdings or authority.

- **Representation.** A small actor's decisions count as evidence about similar actors. If a small business buys less energy than usual, the aggregate that stands for small businesses buys somewhat less in the following round. The adjustment is capped and includes some random variation. The business's own purchase still executes once, from its own budget. Players know their choices move the aggregate, so they are not unbiased samples. That influence is kept explicit and bounded ([02 §7](02_kernel.md#7-aggregates-and-representation)).
- **Collective procedure.** A bloc acts when members whose combined weight passes the threshold sign its candidate Transaction. The bloc then acts in that same round with its own resources and authority. Otherwise it lapses ([03 §6.3](03_premise.md#63-aggregates)).

## 6. Evaluation

This section says how a PremiseComposition judges whether play went well. There is no single winner.

Each actor has a role-specific objective with measurable criteria, such as holding at least a rival's military capacity or never running short of energy. A PremiseComposition defines no system-level measure: the whole-economy views show how the economy as a whole went ([07 §11](07_simulator.md#11-experiments)). Objectives are scored from the recorded Run and never change the economy. Outcomes are not reduced to money. Military capacity, influence, and resource security count through these objectives.

A PremiseComposition is judged against these questions:

- Do small players matter?
- Do governments have reasons to negotiate?
- Are shortages consequential without being permanently disabling?
- Does cooperation pay?
- Can several strategies succeed?
- Do early decisions matter later?
- Does the economy respond without excessive volatility?

The simulator's indicators support this judgment but do not replace playtesting ([07 §12](07_simulator.md#12-correctness-versus-gameplay-quality)).

## 7. Design Principles

This section lists the rules that every PremiseComposition and every rule should respect.

- **One world, many scales.** A PremiseComposition can be played at any level of detail, from a few great powers to a crowd of small participants, without changing what the world contains ([INV-14](02_kernel.md#inv-14)).
- **Same rules for everyone.** Every participant, human or not, acts through the same kinds of decision under the same rules, so no seat gets special mechanics ([INV-01](02_kernel.md#inv-01)).
- **Diplomacy is free; joint candidate Transactions are binding.** Players may negotiate however they like, but only what every party has signed takes effect, and every joint candidate Transaction must be paid for.
- **Unequal but interdependent.** Power is deliberately unequal, yet every role has a capability, a dependency that others control, and an objective that others can help or obstruct. Agency matters more than equal starting resources.
- **Small players matter.** Representation and collective procedures let a small participant shift a larger economy, with influence that is real but bounded ([§5](#5-small-players-matter)).
- **The core is a small kernel; agreements are authored.** Core offers stocks, flows, laws and conditional obligations. Trades, signatures, procedures, schedules and depreciation are vocabulary a Premise writes, so a new kind of agreement needs no change to the engine's core ([02 §10](02_kernel.md#10-design-boundaries)).
- **Economies as reproducing objects.** A PremiseComposition may be organized as a few economic objects, each a Premise: for example a national reproduction regime per country, reproducing its productive capability, effective demand and financial capacity, and one clearing system that moves goods, currencies and claims between them. An object is a way of authoring and reading the World, never a new kind of engine object: it is built from the same Resources, laws, agreements, Exchanges and Rules, its measures are derived from the record rather than stored, and its feedback loops arise from those rules rather than being scripted ([05 §1](05_pacific.md#1-the-game)).
- **Consequences, not scripts.** Outcomes emerge from decisions meeting exchanges. Actors that no player controls keep fixed motives, so players change the world others respond to, not the way they respond.

## 8. Document Map

- [02 Kernel](02_kernel.md): what any World can be made of, and the invariants of the economy.
- [03 Premise](03_premise.md): how Premises are authored and composed, and the vocabulary that compiles to the kernel.
- [04 Run](04_run.md): how a PremiseComposition is assembled and realized, Round by Round.
- [05 Pacific](05_pacific.md): *Pacific*, a two-country PremiseComposition of trade in two currencies, export controls, industrial competence, financial repression and the dollar, about economic arrangements that grow costlier to change.
- [07 Simulator](07_simulator.md): simulation, the run store, the dashboard, analysis, and tests.
- [08 Architecture](08_architecture.md): guidelines and the structure of the implementation.
