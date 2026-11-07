# Scenario Planning Game — Overview

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
| Primary mechanic | Strategic decisions and signed deals affecting interconnected markets |
| Input | Out-of-band negotiation, oral announcements, and AI-interpreted actions |
| Output | Economic outcomes, balance sheets, and a fictional financial-news front page |

Each round runs in five steps. The engine's sequence within a round is in [02 §8.1](02_type_system.md#81-canonical-resolution-sequence); the session procedure is in [04 §11](04_instantiated_game.md#11-round-execution).

1. **Briefing.** Players receive the current state of the world.
2. **Negotiation.** Players talk freely and strike deals. Negotiation happens entirely out of band; the system has no negotiation protocol.
3. **Submission.** Players announce their intended actions. One side of an agreed deal proposes it (for example, "20 Energy to Japan for 48 Money"), and the other side signs it. AI converts these into structured actions and confirms anything materially ambiguous before the deadline. A deal that isn't signed by everyone involved lapses.
4. **Resolution.** The engine resolves every player, NPC, and implicit-economy action together as one round update.
5. **Reporting.** Players receive updated balance sheets and a front page describing the round.

## 3. Three Architectural Layers

**Type System.** The reusable economic machinery: what entities exist, how resources are owned and priced, how decisions are expressed, and how explicit participants interact with the implicit economy. It prescribes no particular countries, resources, or conflicts.

**Gameboard.** A particular scenario: the larger economic world from which any game is drawn. It establishes sectors, populations, institutions, relationships, and persistent tensions, such as US–China competition or class conflict. Vertical reports, research documents on individual sectors, ground its configuration ([03](03_gameboard.md)).

**Instantiated Game.** One playthrough. It chooses the world's **resolution**: which parts are explicit Actors, which Actors are player **seats** and which are NPCs, and which parts remain aggregate. For example, the EU might be human-controlled in one game, an NPC in another, and implicit in a third. Every Actor that isn't selected falls back to a declared alternative ([04](04_instantiated_game.md)).

## 4. Core Types

The type system is built from a small set of primitives: Resources, Entities with accounts and capabilities, Policies, Markets, costed Transformations (Facilities), Aggregates, Representations, Intents, and Proposals. Their definitions are in [02 §2–§5](02_type_system.md); the glossary is [02 §2.5](02_type_system.md#25-glossary).

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

Markets trade existing resources along a cost curve (a bonding curve). Facilities, such as military procurement, produce new quantities within capacity, priced by the same kind of curve. A price curve never creates an asset by itself ([INV-07](02_type_system.md#inv-07)). Markets can be regional, with access rules and tariffs, and transport between regions is itself a capacity-limited facility, so trade restrictions have real effects.

To sustain five years of play, every round includes a recurring economic cycle, computed together rather than in sequence:

- **Production** from capacity and inputs.
- **Exchange**, with Money flowing to suppliers.
- **Income and expenditure**: wages, taxes, and public spending.
- **Capacity renewal** from investment.

Money paid into production always goes to a declared recipient, and stocks reconcile every round ([INV-05](02_type_system.md#inv-05)). Shortages therefore reflect choices and modeled scarcity, not missing accounting.

## 7. Engine and Policies

The engine takes the World State and a round's Intents and produces the next World State. It never knows how the Intents were produced. A **Policy** produces Intents from what its Actor is allowed to see. There are three kinds:

- **Programs:** declarative rules for NPCs, Aggregate baselines, and preset seat behaviour, all on one shared interpreter.
- **AI agents:** a language model playing a role from its briefing and view.
- **Humans:** a person whose typed text AI turns into Intents.

Programs are reproducible from their seed. AI agents and humans are reproducible from their recorded Intents ([02 §4](02_type_system.md#4-policies), [INV-13](02_type_system.md#inv-13)).

An NPC's Policy is part of the scenario, such as a government that retaliates against tariffs. A seat's Policy comes from outside, so the simulator can replace a human with an agent or program without changing the game. In-world Policies are fixed at initialization, but their actions change as the world changes. A player who changes a rule changes the conditions the Policies respond to, never the Policies themselves.

**Negotiation is propose, then sign.** Each round, every Policy publishes Proposals and then signs the ones that name it and that it endorses. Publishing implies the author's signature. A deal is a Proposal that also needs the counterparty's signature, so the two sides cannot disagree about terms. An unsigned deal lapses, and a counteroffer is a new Proposal for the next round ([02 §3.3](02_type_system.md#33-proposals-and-signatures)).

## 8. Representation and Collective Procedures

Two mechanisms let smaller players matter:

- **Representation.** A small-business owner's decisions are treated as evidence about similar businesses. If they buy less Energy than usual, the Global SMEs Aggregate buys somewhat less in the following round. An estimator shrinks and caps the adjustment and adds seeded variance. The owner's own purchase still executes once, from their own small budget.
- **Collective procedure.** A bloc acts by weighted signing on its own Proposal. Members sign to support it, and the bloc's residual weight always counts. If support exceeds the threshold, the bloc acts in that same round with its own resources and authority. Otherwise the Proposal lapses ([02 §2.2](02_type_system.md#22-three-projections-of-an-entity)).

For Representation the engine computes and stores the estimate, and the Aggregate's own Policy applies it next round. Neither mechanism gives the Actor the Aggregate's holdings or authority. Because players know their choices move the Aggregate, they are not unbiased samples. That influence is kept explicit and bounded, and the simulator measures it as a balancing parameter.

## 9. Resolution

Seat decisions, NPC decisions, Aggregate behaviour, and signed deals all become Intents in signed Proposals. These resolve together as one bundled round update with uniform market prices. The engine never authors or rewrites an Intent, nothing is ordered within a round, and everything a round computes becomes visible in the next round. The canonical sequence is [02 §8.1](02_type_system.md#81-canonical-resolution-sequence).

## 10. AI Interface and Reporting

Players never touch the type system directly. AI turns their typed decisions into structured Intents, confirms materially ambiguous ones, and records the accepted Intents so a game can be replayed without reinterpreting text ([04 §11](04_instantiated_game.md#11-round-execution)). Input is text; a speech-to-text front end can be added later without changing the interpreter.

The engine, not the language model, decides what happens. AI then writes a fictional financial-news front page in the style of the *Financial Times* or *The Economist*, and each player receives a balance sheet. **AI interprets inputs and communicates results; the engine determines outcomes.** Reporting must stay grounded in the resolved world state.

## 11. Evaluation

There is no single winner. Each Actor has a role-specific objective with measurable criteria, such as holding at least a rival's military capacity or never running short of Energy, and the scenario defines system-level measures ([03 §10.5](03_gameboard.md#105-evaluation)). Objectives are evaluated from the recorded game and never affect the economy. Outcomes are not reduced to Money; military capacity, influence, and resource security matter through these objectives.

Every role should have a meaningful capability, a dependency that others can provide or withhold, and an objective others can help or obstruct. A small-business owner doesn't need the resources of the United States, but their choices should materially alter some part of the economy.

## 12. Design Principles

- **Minimal ontology.** A small set of types supports many scenarios. New phenomena tighten existing primitives rather than add new ones.
- **One economy, many resolutions.** Explicit Actors and implicit Aggregates are different views of one economic world. Instantiating it at any resolution must preserve its accounts, liabilities, and authority ([INV-14](02_type_system.md#inv-14)).
- **Uniform action semantics.** Intents are the only input. Humans, agents, programs, NPCs, and Aggregates all act through them ([INV-01](02_type_system.md#inv-01)).
- **The engine is an accountant.** It checks authority and signatures and keeps the books balanced. Negotiation, persuasion, and agreement happen out of band; the engine only sees what was proposed and who signed.
- **Complete circulation.** Resources are produced, exchanged, distributed, and renewed, so the economy sustains itself (§6).
- **Intrinsic asymmetry.** Participants need not be equal in resources, authority, or influence.
- **Cross-scale participation.** Representation and collective procedures let small Actors and member states influence Aggregates without unrealistic control (§8).
- **AI-mediated experience.** Natural language makes the system accessible without making the language model the authority over outcomes (§10).
- **Purity.** The engine is a pure function. Randomness, player input, AI interpretation and narration, persistence, and time are declared effects interpreted by handlers ([02 §8.4](02_type_system.md#84-effects-and-handlers), [INV-36](02_type_system.md#inv-36)).
- **Reproducibility.** The same initial state, accepted Intents, and seeds always produce the same outcome ([INV-13](02_type_system.md#inv-13)). Each game has one game seed for in-world randomness. The simulator adds a separate player seed for simulated player behaviour, so one fixed game can be replayed under many player seeds ([02 §9.1](02_type_system.md#91-numerics-and-randomness)).

## 13. Document Map

| Document | Contents |
|---|---|
| [`02_type_system.md`](02_type_system.md) | Types, Intents and Proposals, Policies, markets, Representation, round sequence, glossary, invariants, numerics |
| [`03_gameboard.md`](03_gameboard.md) | How a scenario is configured |
| [`04_instantiated_game.md`](04_instantiated_game.md) | How a game is initialized and run |
| [`05_test_instance.md`](05_test_instance.md) | Meridian reference scenario and fixtures A–G |
| [`06_simulator.md`](06_simulator.md) | Simulation harness (seat Policies, replay, experiments), run store, dashboard, and mandatory tests |
| [`07_architecture.md`](07_architecture.md) | Codebase layout, module boundaries, and key interfaces |
| [`08_coverage.md`](08_coverage.md) | Audit of which tests exercise each feature, and the capstone run |

Economic participation does not require individual representation. Actors and Aggregates act through the same economic system, so a small group of human decisions can move a much larger simulated economy.
