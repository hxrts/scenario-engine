# Scenario Planning Game — Overview

## 1. Concept

The Scenario Planning Game is a multiplayer economic strategy game. Each participant plays an influential actor in a shared world economy. Players negotiate, form alliances, allocate resources, and make decisions that shape how the world develops economically and politically.

The game combines elements of *Diplomacy*, economic simulation, and scenario planning. At its centre is an economic engine that turns player decisions into consequences across interconnected markets.

Players may represent countries, corporations, institutions, or small economic participants, and their power is deliberately unequal. Even so, a small-business owner can influence the same economy as a head of state, through statistical representation.

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
| Primary mechanic | Strategic decisions and signed deals affecting interconnected markets |
| Input | Out-of-band negotiation, oral announcements, and AI-interpreted actions |
| Output | Economic outcomes, balance sheets, and a fictional financial-news front page |

Each round runs in five steps:

1. **Briefing.** Players receive the current state of the world.
2. **Negotiation.** Players talk freely and strike deals. Negotiation happens entirely out of band; the system has no negotiation protocol.
3. **Submission.** Players announce their intended actions. One side of an agreed deal proposes it (for example, "20 Energy to Japan for 48 Money"), and the other side signs it. AI converts these into structured actions and confirms anything materially ambiguous before the deadline. A deal that isn't signed by everyone involved simply lapses.
4. **Resolution.** The engine resolves every player, NPC, and implicit-economy action together as one round update.
5. **Reporting.** Players receive updated balance sheets and a front page describing the round.

## 3. Three Architectural Layers

**Type System.** The reusable economic machinery: what entities exist, how resources are owned and priced, how decisions are expressed, and how explicit participants interact with the implicit economy. It prescribes no particular countries, resources, or conflicts.

**Gameboard.** A particular scenario: the larger economic world from which any game is drawn. It establishes sectors, populations, institutions, relationships, and persistent tensions, such as US–China competition or class conflict. **Vertical reports**, research documents on individual sectors, ground its configuration.

**Instantiated Game.** One playthrough. It chooses the world's **resolution**: which parts are explicit Actors, which Actors are player **seats** and which are NPCs, and which parts remain aggregate. For example, the EU might be human-controlled in one game, an NPC in another, and implicit in a third. Every Actor that isn't selected falls back to a declared alternative.

## 4. Core Types

| Type | Meaning |
|---|---|
| **Resource** | An economic quantity that can be owned, exchanged, produced, or consumed. Its behavior (transferable, reservable, decaying) is configured. |
| **Entity** | An economic participant with accounts and capabilities. |
| **Policy** | Anything that produces Intents from what an Actor is allowed to see: a program, an AI agent, or a human. |
| **Market** | A mechanism for exchanging existing resources at a price. |
| **Aggregate** | An Entity representing the residual economy of an implicit population. |
| **Representation** | A relationship through which an explicit Actor's sampled behavior adjusts an Aggregate. |
| **Intent** | A structured economic or institutional action; the engine's only input. |

An **Actor** is an explicit Entity: either a **seat**, driven by a Policy supplied from outside the scenario (a human, an AI agent, or a preset program), or an **NPC**, driven by a Policy that is part of the scenario. A **cost function** prices activity whose marginal cost rises with volume; it is shared by Markets and by capacity-limited production **facilities**. A **Proposal** is a message carrying Intents; it executes only once it has the signatures it requires.

Authority and resources are distinct. A government can regulate businesses without owning their resources. Not everything meaningful is a Resource: production capacity, market access, and authority are economic state or rules. This lets a few Resources support a rich economy.

Formal definitions, the glossary, and the numbered invariants are in [`02_type_system.md`](02_type_system.md).

## 5. Explicit and Implicit Economy

The **explicit economy** contains the Actors whose decisions are individually played.

The **implicit economy** works like a game's skybox: a compact representation of a much larger world. Millions of small businesses might be a single Aggregate that holds their combined resources and follows a Policy that responds to conditions. It is not background scenery. Its demand, supply, and investment move prices and shape the consequences of player decisions.

Three relationships are kept separate:

- **Membership:** which Entities an Aggregate lists as members. Memberships may overlap and confer nothing.
- **Account ownership:** every account and responsibility has exactly one owner, so overlapping memberships never double-count.
- **Collective decisions:** how an institution decides, and what authority it holds.

Germany can therefore be an EU member while the German government's accounts belong to an explicit Actor, the EU Aggregate owns the rest of the European economy, and EU-wide authority belongs to the EU's own collective procedures. German households and firms stay in the implicit European economy.

## 6. Markets, Production, and Circulation

Markets trade existing resources along a cost curve (a bonding curve). Facilities, such as military procurement, produce new quantities within capacity, priced by the same kind of curve. A price curve never creates an asset by itself. Markets can be regional, with access rules and tariffs, and transport between regions is itself a capacity-limited facility, so trade restrictions have real effects.

To sustain five years of play, every round includes a recurring economic cycle, computed together rather than in sequence:

- **Production** from capacity and inputs.
- **Exchange**, with Money flowing to suppliers.
- **Income and expenditure**: wages, taxes, and public spending.
- **Capacity renewal** from investment.

Money paid into production always goes to a declared recipient, and stocks reconcile every round. Shortages therefore reflect choices and modeled scarcity, not missing accounting.

## 7. Engine and Policies

The design splits into two parts. The **engine** takes the World State and a round's Intents and produces the next World State; Intents are its only input, and it never knows how they were produced. Alongside it, a **Policy** is anything that produces Intents from what its Actor is allowed to see. There are three kinds:

- **Programs:** declarative rules for NPCs, Aggregate baselines, and preset player behavior, all on one shared interpreter. Reproducible from the game seed.
- **AI agents:** a language model playing a role from its briefing and view. Reproducible from its recorded Intents.
- **Humans:** a person whose typed text AI turns into Intents. Reproducible from recorded Intents.

An NPC's Policy is part of the scenario, such as a government that retaliates against tariffs or small businesses cutting investment when costs rise. A seat's Policy comes from outside, so the simulator can replace a human with an agent or program without changing the game. In-world Policies are fixed at initialization, but their actions change as the world changes; a player who changes a rule changes the conditions they respond to, never the Policies themselves.

**Negotiation is propose, then sign.** Each round, a round runner has every Policy (seats, NPCs, and Aggregates alike) publish Proposals, then sign the ones naming it that it endorses. Publishing implies the author's signature, so an ordinary action needs nothing more. A deal is a Proposal that also needs the counterparty's signature, and signing it is the acceptance, so the two sides can never disagree about terms. Humans agree terms out of band, then one proposes and the other signs; programs sign by rule, and agents choose. An unsigned deal lapses, and a counteroffer is a new Proposal for the next round.

## 8. Representation and Collective Procedures

Two mechanisms let smaller players matter:

- **Representation.** A small-business owner's decisions are treated as evidence about similar businesses. If they buy less Energy than usual, the Global SMEs Aggregate buys somewhat less in the following round. An estimator shrinks and caps the adjustment and gives it a little seeded variance. The owner's own purchase still executes once, from their own small budget.
- **Collective procedure.** A bloc's decision is weighted signing on the bloc's own Proposal. The bloc proposes an action, and each member state supports it by signing. Signed member weights are added to the bloc's own weight; if the total exceeds the threshold, the bloc acts that same round with its own resources and authority. Otherwise the Proposal lapses.

For Representation the engine only computes and stores the estimate; the Aggregate's own Policy applies it next round. Neither mechanism gives the Actor the Aggregate's holdings or authority. Because players know their choices move the Aggregate, they are not unbiased samples. That influence is part of the design: it is kept explicit and bounded, and the simulator measures it as a balancing parameter.

## 9. Resolution

Every path ends in the same place: seat decisions, NPC decisions, Aggregate behavior, and signed deals all become Intents in signed Proposals, and these are resolved together as one bundled round update with uniform market prices. The engine never authors or rewrites an Intent. There is no ordering of actions within a round, and everything a round computes (prices and representative estimates) becomes visible next round. The canonical sequence is defined in [`02_type_system.md` §8.1](02_type_system.md#81-canonical-resolution-sequence).

## 10. AI Interface and Reporting

Players never touch the type system directly. AI turns their typed decisions into structured Intents, confirms materially ambiguous ones, and records the accepted Intents so a game can be replayed without reinterpreting text. Input is text; a speech-to-text front end can be added later without changing the interpreter.

The engine, not the language model, decides what happens. AI then writes a fictional financial-news front page in the style of the *Financial Times* or *The Economist*, and each player receives a balance sheet. **AI interprets inputs and communicates results; the engine determines outcomes.** Reporting must stay grounded in the resolved world state.

## 11. Evaluation

There is no single winner. Each Actor has a role-specific objective with measurable criteria, such as holding at least a rival's military capacity or never running short of Energy, and the scenario defines system-level measures. Objectives are evaluated from the recorded game and never affect the economy. Outcomes are not reduced to Money; military capacity, influence, and resource security matter through these objectives.

Every role should have a meaningful capability, a dependency that others can provide or withhold, and an objective others can help or obstruct. A small-business owner doesn't need the resources of the United States, but their choices should materially alter some part of the economy.

## 12. Design Principles

**Minimal ontology.** A small set of types supports many scenarios. New phenomena tighten existing primitives rather than add new ones.

**One economy, many resolutions.** Explicit Actors and implicit Aggregates are different views of one economic world.

**Resolution consistency.** Instantiating the same gameboard at different levels of explicitness must preserve its accounts, liabilities, and authority before any decisions differ. Divergence comes from decisions, never from accounting accidents.

**Uniform action semantics.** Intents are the only input; humans, agents, programs, NPCs, and Aggregates all act through them.

**The engine is an accountant.** It checks authority and signatures and keeps the books balanced. Negotiation, persuasion, and agreement happen out of band among players; the engine only sees what was proposed and who signed.

**Complete circulation.** Resources are produced, exchanged, distributed, and renewed, so the economy sustains itself.

**Intrinsic asymmetry.** Participants need not be equal in resources, authority, or influence.

**Cross-scale participation.** Representation and collective procedures let small Actors and member states influence Aggregates without unrealistic control.

**AI-mediated experience.** Natural language makes the system accessible without making the language model the authority over outcomes.

**Purity.** The whole system is pure. Randomness, player input, AI interpretation and narration, persistence, and time are algebraic effects interpreted by handlers; the engine itself has no effects. The same round code runs live, in simulation, and in replay, with only the handlers changed.

**Reproducibility.** The same initial state, accepted actions, and seeds always produce the same outcome. Each game has exactly one game seed for in-world randomness, such as variance in representative sampling. The simulator adds a separate player seed for simulated player behavior, so a fixed game can be replayed under many player seeds to see a distribution of outcomes.

## 13. Document Map

| Document | Contents |
|---|---|
| [`02_type_system.md`](02_type_system.md) | Types, glossary, round resolution, invariants, numerics |
| [`03_gameboard.md`](03_gameboard.md) | How a scenario is configured |
| [`04_instantiated_game.md`](04_instantiated_game.md) | How a game is initialized and run |
| [`05_test_instance.md`](05_test_instance.md) | The Fractured World reference scenario and fixtures |
| [`06_simulator.md`](06_simulator.md) | Simulation harness (seat Policies, replay, experiments) and dashboard |
| [`07_architecture.md`](07_architecture.md) | Codebase layout, module boundaries, and key interfaces |

The central idea: **economic participation does not require individual representation.** Actors and Aggregates act through the same economic system, so a small group of human decisions can move a much larger simulated economy.
