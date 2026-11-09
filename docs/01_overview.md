# Game Overview

## 1. Concept

This section says what the game is, who plays it, and how the world, the AI, and the engine fit together.

The Scenario Planning Game is a multiplayer economic strategy game. Players are influential actors in a shared world economy. They negotiate, form alliances, allocate resources, and make decisions that shape how the economy and politics develop. The game combines diplomacy, economic simulation, and scenario planning. At its centre is an engine that turns player decisions into consequences across interconnected markets.

Players may represent countries, institutions, corporations, or small economic participants. Their power is deliberately unequal. A small-business owner can still move the same economy as a head of state, through representation ([§5](#5-small-players-matter)).

The world has two parts: explicitly represented participants, and an implicit economy that models everything else. Both act through the same economic system.

Two roles sit around the engine:

- **AI interprets and narrates.** It turns what players say into structured proposed actions, and it writes the round's report.
- **The engine determines outcomes.** Every result comes from the resolved economy, never from the narrator.

**Scenario planning terms.** The game's planning vocabulary is defined here, once.

- **Setting.** The whole thing a game is launched from, such as *Meridian*: the authored world, its uncertainties, and the strategies it configures. Every scenario of a set shares it. The world itself is specified in [03](03_gameboard.md).
- **Scenario.** A story of one possible future: one run of a setting with its uncertainties resolved, its rounds recorded, and its narration produced.
- **Scenario set.** The scenarios compared together. They share the setting's predetermined elements and vary its uncertain ones.
- **Uncertainty.** An element a scenario set varies, such as a shock, a parameter override, or the random seed.

## 2. Game Format

This section gives the shape of a typical session.

| Dimension | Design |
|---|---|
| Players | 10–20 |
| Session length | About 6 hours, including meals |
| Rounds | 4–8 |
| Simulated period | 5 years in the reference setting |
| World | One shared timeline |
| Player roles | Countries, institutions, corporations, and smaller economic participants |
| Input | Out-of-band negotiation, oral announcements, and AI-interpreted actions |
| Output | Economic outcomes, balance sheets, and a fictional financial-news front page |

## 3. The Round

This section describes what a player experiences in one round. Each round runs these five stages in order.

1. **Briefing.** Players receive the current state of the world.
2. **Negotiation.** Players talk freely, persuade, bluff, and strike joint Proposals. This happens entirely outside the game. The game has no negotiation protocol.
3. **Submission.** Each human submits one transcript of what they intend to do this round. A trade may be described by one party, by both, or by neither. Once submitted, a player has no further input that round.
4. **Resolution.** Every player, NPC, and market action resolves together as one update. Nothing is ordered within a round, and what a round computes is visible in the next round ([02 §8.1](02_type_system.md#81-canonical-resolution-sequence)).
5. **Reporting.** Players receive updated balance sheets and a front page describing the round.

**Propose, then sign.** Negotiation works in two steps. Each party proposes actions, then signs the proposals that name it and that it accepts. A joint Proposal needs every named party's signature, so the two sides cannot disagree about its terms. A joint Proposal that is missing a signature lapses. A counteroffer is a new Proposal for the next round ([02 §3.3](02_type_system.md#33-proposals-and-signatures)). A transcript that describes a joint Proposal counts as its speaker's agreement to it. When the interpreter reads the transcripts together, it reconciles matching descriptions into one Proposal, adjusting details slightly where needed. A joint Proposal that only one side describes lapses.

**Planned:** spoken submission, transcribed before interpretation.

## 4. Explicit and Implicit Economy

This section explains which parts of the economy players decide over directly and which they experience only through its effects.

The **explicit economy** contains the actors whose decisions are played one by one. Governments, companies, and institutions in a given playthrough are chosen from the setting. Which actors are played and which are left to the game is a choice made per game ([04](04_instantiated_game.md)).

The **implicit economy** works like a backdrop: a compact representation of a much larger world. Millions of small businesses might be one aggregate that holds their combined resources and responds to conditions. It is not scenery. Its demand, supply, and investment move prices and shape the results of player decisions.

Three relationships are kept separate, so overlapping groups never double-count:

- **Membership** lists which actors belong to a group. Membership confers no authority and no holdings.
- **Account ownership** gives every account and responsibility exactly one owner.
- **Collective decisions** govern how an institution decides and what authority it holds.

For example, Germany can be an EU member, have its government's accounts played as an actor, and leave German households and firms in the implicit European economy. EU-wide authority stays with the EU's own decision procedures.

Markets trade existing resources at prices that rise and fall with demand. Facilities produce new goods within their capacity, and capacity can be renewed through investment ([02 §5](02_type_system.md#5-cost-functions-markets-and-transformations)). Money paid into production always goes to a named recipient, so shortages reflect choices and modelled scarcity, not missing accounting ([02 §6](02_type_system.md#6-economic-circulation)).

## 5. Small Players Matter

This section explains how small participants can shape a larger economy. Both mechanisms are bounded, and neither gives a player the aggregate's holdings or authority.

**Representation.** A small actor's decisions count as evidence about similar actors. If a small business buys less energy than usual, the aggregate that stands for small businesses buys somewhat less in the following round. The adjustment is capped and includes some random variation. The business's own purchase still executes once, from its own budget. Players know their choices move the aggregate, so they are not unbiased samples. That influence is kept explicit and bounded ([02 §7](02_type_system.md#7-aggregates-and-representation)).

**Collective procedure.** A bloc acts when members whose combined weight passes the threshold sign its proposal. The bloc then acts in that same round with its own resources and authority. Otherwise the proposal lapses ([02 §3.3](02_type_system.md#33-proposals-and-signatures)).

## 6. Evaluation

This section says how a setting judges whether play went well. There is no single winner.

Each actor has a role-specific objective with measurable criteria, such as holding at least a rival's military capacity or never running short of energy. The setting also defines system-level measures ([03 §10.5](03_gameboard.md#105-evaluation)). Objectives are scored from the recorded game and never change the economy. Outcomes are not reduced to money. Military capacity, influence, and resource security count through these objectives.

A setting is judged against these questions:

- Do small players matter?
- Do governments have reasons to negotiate?
- Are shortages consequential without being permanently disabling?
- Does cooperation pay?
- Can several strategies succeed?
- Do early decisions matter later?
- Does the economy respond without excessive volatility?

The simulator's indicators support this judgment but do not replace playtesting ([06 §12](06_simulator.md#12-correctness-versus-gameplay-quality)).

## 7. Design Principles

This section lists the rules that every setting and every rule should respect.

- **One world, many scales.** A setting can be played at any level of detail, from a few great powers to a crowd of small participants, without changing what the world contains ([INV-14](02_type_system.md#inv-14)).
- **Same rules for everyone.** Every participant, human or not, acts through the same kinds of decision under the same rules, so no seat gets special mechanics ([INV-01](02_type_system.md#inv-01)).
- **Diplomacy is free; joint Proposals are binding.** Players may negotiate however they like, but only what every party has signed takes effect, and every joint Proposal must be paid for.
- **Unequal but interdependent.** Power is deliberately unequal, yet every role has a capability, a dependency that others control, and an objective that others can help or obstruct. Agency matters more than equal starting resources.
- **Small players matter.** Representation and collective procedures let a small participant shift a larger economy, with influence that is real but bounded ([§5](#5-small-players-matter)).
- **Consequences, not scripts.** Outcomes emerge from decisions meeting markets. Non-player actors keep fixed motives, so players change the world others respond to, not the way they respond.

## 8. Document Map

- [02 Type System](02_type_system.md): the types, rules, and invariants of the economy.
- [03 Gameboard](03_gameboard.md): how a setting is authored.
- [04 Instantiated Game](04_instantiated_game.md): how a playthrough is assembled and run.
- [05 Test Instance](05_test_instance.md): *Meridian*, the reference setting.
- [06 Simulator](06_simulator.md): simulation, the run store, the dashboard, analysis, and tests.
- [07 Architecture](07_architecture.md): guidelines and the structure of the implementation.
