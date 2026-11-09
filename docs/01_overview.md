# Game Overview

## 1. Concept

The Scenario Planning Game is a multiplayer economic strategy game. Each participant plays an influential actor in a shared world economy, negotiating, forming alliances, allocating resources, and making decisions that shape how the world develops economically and politically.

The game combines diplomacy, economic simulation, and scenario planning. At its centre is an economic engine that turns player decisions into consequences across interconnected markets.

Players may represent countries, corporations, institutions, or small economic participants, and their power is deliberately unequal. A small-business owner can still influence the same economy as a head of state, through statistical representation (§8).

The world has two parts: explicitly represented participants, and an implicit economy that models everything else. Both act through one economic system. AI sits between the players and the engine. It interprets what players say into structured actions and reports the results as narrative.

## 2. Game Format

| Dimension | Design |
|---|---|
| Players | 10–20 |
| Duration | About 6 hours, including meals |
| Rounds | 4–8 |
| Simulated period | 5 years |
| World | One shared timeline |
| Player roles | Countries, institutions, corporations, and smaller economic participants |
| Primary mechanic | Strategic decisions and signed joint Proposals affecting interconnected markets |
| Input | Out-of-band negotiation, oral announcements, and AI-interpreted actions |
| Output | Economic outcomes, balance sheets, and a fictional financial-news front page |

Each round runs the engine's canonical resolution sequence, defined once in [02 §8.1](02_type_system.md#81-canonical-resolution-sequence). The session procedure is in [04 §11](04_instantiated_game.md#11-round-execution). The list below is the player's view of a round, not the engine's steps.

1. **Briefing.** Players receive the current state of the world.
2. **Negotiation.** Players talk freely and strike joint Proposals. Negotiation happens entirely out of band. The system has no negotiation protocol.
3. **Submission.** Players talk with each other in private, then each human submits one transcript of what they intend to do this round (**Planned:** as an audio recording, transcribed first; the current build takes the transcript typed at the prompt). A trade may be described by one party, by both, or by neither. Once submitted, a human has no further input in that round. Interpretation then reads every transcript together. A transcript that describes a joint Proposal is that party's agreement to it, and the interpreter reconciles matching descriptions into one Proposal, adjusting details slightly where needed. Informal transcripts are loose by nature, and a player who did not want a joint Proposal would not have described it. A joint Proposal that only one side describes lapses, as any Proposal whose signatures are missing does.
4. **Resolution.** The engine resolves every player, NPC, and implicit-economy action together as one round update.
5. **Reporting.** Players receive updated balance sheets and a front page describing the round.

## 3. Three Architectural Layers

**Type System.** The reusable economic machinery: what entities exist, how resources are owned and priced, how decisions are expressed, and how explicit participants interact with the implicit economy. It prescribes no particular countries, resources, or conflicts.

**Gameboard.** The world a setting describes ([02 §2.5](02_type_system.md#25-glossary)), from which any game is drawn. It establishes sectors, populations, institutions, relationships, and persistent tensions, such as US–China competition or class conflict. Vertical reports, research documents on individual sectors, ground its configuration ([03](03_gameboard.md)).

**Instantiated Game.** One playthrough. It chooses the world's **resolution**: which parts are explicit Actors, which Actors are player **seats** and which are NPCs, and which parts remain aggregate. For example, the EU might be human-controlled in one game, an NPC in another, and implicit in a third. Every Actor that isn't selected falls back to a declared alternative ([04](04_instantiated_game.md)).

The three layers apply to the world and to its policies alike. In-world Strategies (NPC and Aggregate Policies) are declared in the Gameboard and belong to the Instantiated Game, which binds them; seat Policies come from outside. Simulation and live play run the same `Game`, assembled from a setting.

## 4. Core Types

The type system is built from a small set of primitives: Resources, Entities with accounts and capabilities, Policies, Markets, costed Transformations (Facilities), Aggregates, Representations, Intents, and Proposals. Their definitions are in [02 §2–§5](02_type_system.md). The glossary is [02 §2.5](02_type_system.md#25-glossary).

Authority and resources are distinct. A government can regulate businesses without owning their resources. Not everything meaningful is a Resource: production capacity, market access, and authority are economic state or rules. This lets a few Resources support a rich economy.

## 5. Explicit and Implicit Economy

The **explicit economy** contains the Actors whose decisions are individually played.

The **implicit economy** works like a game's skybox: a compact representation of a much larger world. Millions of small businesses might be a single Aggregate that holds their combined resources and follows a Policy that responds to conditions. It is not background scenery. Its demand, supply, and investment move prices and shape the consequences of player decisions.

Three relationships are kept separate:

- **Membership:** which Entities an Aggregate lists as members. Memberships may overlap and confer nothing.
- **Account ownership:** every account and responsibility has exactly one owner, so overlapping memberships never double-count.
- **Collective decisions:** how an institution decides, and what authority it holds.

Germany can therefore be an EU member, hold its government's accounts as an explicit Actor, and leave German households and firms in the implicit European economy. EU-wide authority stays with the EU's own collective procedures.

## 6. Markets, Production, and Circulation

Markets trade existing resources along a cost curve (a bonding curve). Facilities, such as military procurement, produce new quantities within capacity, priced by the same kind of curve. A price curve never creates an asset by itself ([INV-07](02_type_system.md#inv-07)). Markets can be regional, with access rules. Transport between regions is itself a capacity-limited facility, and Transfers between them can carry tariffs, so trade restrictions have real effects.

To sustain five years of play, every round includes a recurring economic cycle, computed together rather than in sequence:

- **Production** from capacity and inputs.
- **Exchange**, with Money flowing to suppliers.
- **Income and expenditure**: wages, taxes, and public spending.
- **Capacity renewal** from investment.

Money paid into production always goes to a declared recipient, and stocks reconcile every round ([INV-05](02_type_system.md#inv-05)). Shortages therefore reflect choices and modeled scarcity, not missing accounting.

## 7. Engine and Policies

The engine takes the World State and a round's Intents and produces the next World State. It never knows how the Intents were produced. A **Policy** produces Intents from what its Actor is allowed to see. There are three kinds:

- **Strategies:** declarative rules for NPCs, Aggregate baselines, and preset seat behaviour, all on one shared interpreter.
- **AI agents:** a language model playing a role from its briefing and view.
- **Humans:** a person who submits one transcript per round, which is interpreted into Intents.

Strategies are reproducible from their seed. AI agents and humans are reproducible from their recorded Intents ([02 §4](02_type_system.md#4-policies), [INV-13](02_type_system.md#inv-13)).

An NPC's Policy is part of the setting, such as a government that retaliates against tariffs. A seat's Policy comes from outside, so the simulator can replace a human with an agent or strategy without changing the game. In-world Policies are fixed at initialization, but their actions change as the world changes. A player who changes a rule changes the conditions the Policies respond to, never the Policies themselves.

**Negotiation is propose, then sign.** Each round, every Policy publishes Proposals and then signs the ones that name it and that it endorses. Publishing implies the author's signature. A joint Proposal is a Proposal that also needs the counterparty's signature, so the two sides cannot disagree about terms. An unsigned joint Proposal lapses, and a counteroffer is a new Proposal for the next round ([02 §3.3](02_type_system.md#33-proposals-and-signatures)).

## 8. Representation and Collective Procedures

Two mechanisms let smaller players matter:

- **Representation.** A small-business owner's decisions are treated as evidence about similar businesses. If they buy less Energy than usual, the Global SMEs Aggregate buys somewhat less in the following round. An estimator shrinks and caps the adjustment and adds seeded variance. The owner's own purchase still executes once, from their own small budget.
- **Collective procedure.** A bloc acts by weighted signing on its own Proposal. Members sign to support it, and the bloc's residual weight always counts. If support exceeds the threshold, the bloc acts in that same round with its own resources and authority. Otherwise the Proposal lapses ([02 §2.2](02_type_system.md#22-three-projections-of-an-entity)).

For Representation the engine computes and stores the estimate, and the Aggregate's own Policy applies it next round. Neither mechanism gives the Actor the Aggregate's holdings or authority. Because players know their choices move the Aggregate, they are not unbiased samples. That influence is kept explicit and bounded, and the simulator measures it as a balancing parameter.

## 9. Resolution

Seat decisions, NPC decisions, Aggregate behaviour, and signed joint Proposals all become Intents in signed Proposals. These resolve together as one bundled round update with uniform market prices. The engine never authors or rewrites an Intent, nothing is ordered within a round, and everything a round computes becomes visible in the next round. The canonical sequence is [02 §8.1](02_type_system.md#81-canonical-resolution-sequence).

## 10. AI Interface and Reporting

Players never touch the type system, and they give no input after submitting. Interpretation reads all of a round's transcripts together into Intents on a best-effort basis, in the spirit of each transcript. Transcribing audio is **Planned**. Ambiguity is resolved in that translation, so there is no clarification or confirmation step. The transcripts and the Intents built from them are recorded, so a game can be replayed without reinterpreting ([04 §11](04_instantiated_game.md#11-round-execution)).

The engine, not the language model, decides what happens. AI then writes a fictional financial-news front page in the style of the *Financial Times* that all players can read, alongside an update to their balance sheet dashboard. **AI interprets inputs and communicates results. The engine determines outcomes.** Reporting must stay grounded in the resolved world state.

## 11. Evaluation

There is no single winner. Each Actor has a role-specific objective with measurable criteria, such as holding at least a rival's military capacity or never running short of Energy, and the setting defines system-level measures ([03 §10.5](03_gameboard.md#105-evaluation)). Objectives are evaluated from the recorded game and never affect the economy. Outcomes are not reduced to Money. Military capacity, influence, and resource security matter through these objectives.

Every role should have a meaningful capability, a dependency that others can provide or withhold, and an objective others can help or obstruct. A small-business owner doesn't need the resources of the United States, but their choices should materially alter some part of the economy.

## 12. Design Principles

- **One world, many scales.** Players, NPCs, and the implicit economy share one economy. A setting can be played at any resolution, from a few great powers to a crowd of small participants, without changing what the world contains ([INV-14](02_type_system.md#inv-14)).
- **Same rules for everyone.** Every participant, human or not, acts through the same kinds of decision under the same rules. No seat gets special mechanics ([INV-01](02_type_system.md#inv-01)).
- **Diplomacy is free. Joint Proposals are binding.** Players negotiate, persuade, and bluff however they like. Only what is proposed and signed by every party takes effect, and every joint Proposal must be paid for.
- **Unequal but interdependent.** Power, resources, and authority are deliberately unequal, yet every role has a capability, a dependency others control, and an objective others can help or obstruct (§11).
- **Small players matter.** Representation and collective procedures let a small business or a member state shift a larger economy, with influence that is real but bounded (§8).
- **A living economy.** Production, trade, income, and investment recur every round, so the world sustains itself across five years and scarcity comes from choices, not from accounting gaps (§6).
- **Consequences, not scripts.** Outcomes emerge from decisions meeting markets. NPCs keep fixed motives, so players change the world others respond to, not the way they respond.
- **Many ways to succeed.** There is no single winner and no single measure of success. Security, influence, and capacity count alongside Money (§11).
- **Plain language in, grounded story out.** Players speak naturally and read a news front page, but the economy, not the narrator, decides what happened (§10).

## 13. Document Map

| Document | Contents |
|---|---|
| [`02_type_system.md`](02_type_system.md) | Types, Intents and Proposals, Policies, markets, Representation, round sequence, glossary, invariants, numerics |
| [`03_gameboard.md`](03_gameboard.md) | How a setting is configured |
| [`04_instantiated_game.md`](04_instantiated_game.md) | How a game is initialized and run |
| [`05_test_instance.md`](05_test_instance.md) | Meridian reference setting and fixtures A–G |
| [`06_simulator.md`](06_simulator.md) | Simulator (seat Policies, replay, experiments), run store, dashboard, and mandatory tests |
| [`07_architecture.md`](07_architecture.md) | Codebase layout, module boundaries, and key interfaces |

Economic participation does not require individual representation. Actors and Aggregates act through the same economic system, so a small group of human decisions can move a much larger simulated economy.
