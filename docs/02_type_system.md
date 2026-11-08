# Type System Specification

**Version:** 0.2  
**Scope:** Type System Layer  
**Implementation language:** PureScript

---

## 1. Purpose

The Type System Layer defines the reusable economic machinery of the Scenario Planning Game: a minimal set of types and operations through which human players, NPCs, and implicit populations act in one shared, deterministic economy. The engine is pure. All impurity is expressed as algebraic effects interpreted by handlers ([§8.4](#84-effects-and-handlers)). The layer is independent of any premise, resource taxonomy, or conflict. It stays small by giving existing primitives precise semantics rather than adding a primitive for every phenomenon.

## 2. Core Ontology

The economic engine is built around seven concepts.

| Type | Definition |
|---|---|
| **Resource** | An economic quantity that can be owned, exchanged, produced, consumed, reserved, or allocated. |
| **Entity** | An economic participant with accounts, capacities, capabilities, and a kind. |
| **Policy** | Anything that produces Intents from the authorized view of the Actor or Aggregate it drives: a declarative strategy, an AI agent, or a human ([§4](#4-policies)). |
| **Market** | A mechanism that exchanges existing resources, determines prices, and settles exchange. |
| **Aggregate** | An Entity representing the residual accounts and baseline behavior of an implicit population or institution. |
| **Representation** | A relationship through which an explicit Actor's sampled behavior adjusts an Aggregate's baseline through an estimator. |
| **Intent** | A typed action: `Trade`, `Route`, `Transfer`, `Transform`, or `ChangeRule` ([§3](#3-intents-and-economic-actions)). |

One supporting primitive is shared: **CostFunction**, a cumulative cost curve that prices quantity. It is used by exchange Markets and by facilities. It is not an Entity and never creates resources by itself.

Derived terms (Actor, Controller, NPC, Seat, Transformation, Proposal, World State, and the rest) are defined in the glossary ([§2.5](#25-glossary)). An Aggregate is a specialization of Entity, not a separate ontology.

### 2.1 Entity representation

Every Entity has the same economic record. Its `kind` says whether it is an Actor or an Aggregate.

```purescript
type Entity =
  { id :: EntityId
  , accounts :: Holdings
  , capacities :: Capacities
  , capabilities :: Array Capability
  , kind :: EntityKind
  }

data EntityKind
  = Actor Controller
  | Aggregate
      { policy :: PolicyId                                   -- baseline Policy
      , members :: Array EntityId                            -- descriptive membership
      , procedures :: Map CollectiveProcedureId CollectiveProcedure
      }

data Controller
  = Seat SeatId     -- Policy supplied from outside the premise; implementation bound at run time
  | NPC PolicyId    -- Policy defined by the premise
```

Identifiers, Holdings, Capacities, and units are domain types defined in code. The snippets in this document are interfaces, not complete modules.

Actors and Aggregates own accounts, hold capabilities, and act in the same Markets and Transformations. They differ only in how their decisions are produced: an Actor is driven by a seat or NPC Policy, an Aggregate by its baseline Policy and any collective procedures. A government, a corporation, or a small-business owner may be an Actor. Households, an industry, or a supranational bloc may be Aggregates. A government Actor holds government-controlled resources only ([§7.1](#71-aggregate-semantics)).

### 2.2 Three projections of an Entity

Membership, economic ownership, and collective decision-making are distinct relationships. The type system keeps them separate rather than letting "Aggregate" carry all three meanings.

| Projection | Question it answers | Rule |
|---|---|---|
| **Membership** | Which Entities belong to this population or organization? | An Aggregate's `members` list. It is descriptive and may overlap with other Aggregates. The one authority it confers is agency in a procedure the member holds no weight in (§2.2). |
| **Account ownership** | Who authoritatively owns each economic account and behavioral responsibility? | Every account and responsibility has exactly one owner ([INV-08](#inv-08)). |
| **Collective procedures and capabilities** | How does an institution decide, and what authority does it hold? | Authority belongs to the institutional Entity or a rule mechanism, never to members by virtue of membership ([INV-03](#inv-03)). |

```purescript
type CollectiveProcedure =
  { weights :: Map EntityId Ratio      -- member weights
  , residualWeight :: Ratio            -- weight of the Aggregate itself (the implicit members)
  , threshold :: Ratio                 -- passes when support exceeds it
  , authorizes :: Array Capability     -- what a passed Proposal may do
  }
```

A collective procedure is a weighted-signature rule on the Aggregate's own Proposals. The Aggregate's Policy publishes a **procedure Proposal** ([§3.3](#33-proposals-and-signatures)) whose Intents fall within `authorizes`, which uses the Aggregate's own accounts and authority, and whose signers are the procedure's members. A member supports by signing and declines by not signing:

**Support = residualWeight + Σ weightᵢ over the members who signed**

Publishing implies the author's signature, so the residual weight always counts. The Proposal executes in that round if support exceeds `threshold`. Otherwise it lapses. The engine records the signers, support, threshold, and outcome.

A member that is not explicit has its weight folded into `residualWeight`. Instantiation moves weight between the two, as part of resolution consistency ([INV-14](#inv-14)).

A member with no weight in a procedure acts for the Aggregate in that procedure without signing: its Intents may appear in the procedure Proposal and are executed from its own accounts. The EU's solidarity pool is such a member ([docs/05 §12.1](05_test_instance.md#121-germany--eu-collective-procedure)).

Germany is in the EU's `members` whether or not it signs in a given round. The EU Aggregate owns the residual European accounts. Germany influences EU procedures through its declared weight without owning EU accounts or exercising EU capabilities. A manufacturer may belong to the small-business population, the manufacturing sector, and a regional economy at once, but those memberships never let three Aggregates own its assets. Accounting is partitioned. Classification is not.

### 2.3 Resource

A Resource is an identifiable economic quantity with a unit and declared behavior. Behavior is configured through properties, not hard-coded per resource:

```purescript
type ResourceDefinition =
  { id :: ResourceId
  , unit :: Unit
  , transferability :: Transferability   -- Free | Restricted | NonTransferable
  , decay :: Maybe DecayRule
  , reservable :: Boolean
  }
```

`Restricted` means authority-gated transfer: its Transfers need `MayTransfer`. A `Free` Resource moves on its holder's signature alone. A `reservable` Resource can be reserved, for example by a military deployment, without being consumed. A Transformation declares the quantity it reserves per unit ordered (`reserves`, empty by default). The Gameboard rejects a reservation of a Resource that is not `reservable`. A Resource that no Transformation produces or consumes is conserved. This follows from the declared Transformations, not from a separate declaration.

A reservation lasts within one round, and its mechanics are given in [§8.2](#82-single-round-update-and-snapshot-discipline): the reserved quantity limits competing actions but is neither consumed nor transferred, and nothing stays reserved across a round boundary.

Not everything economically meaningful is a Resource. Production capacity, transport capacity, market access, political authority, and supply-chain efficiency are economic state or rules attached to Entities, Markets, and Transformations. A semiconductor producer can hold valuable productive capacity without the premise introducing a semiconductor Resource.

Holdings are distinct from capabilities. A government may hold authority to modify taxation without owning the resources of the affected private sector, and an Actor cannot spend, transfer, or create resources merely because it has political influence over their owners.

### 2.4 Capability

A Capability is a pattern over Intents: it says which actions an Entity is authorized to perform.

```purescript
data Capability
  = MayTrade MarketId
  | MayTransfer ResourceId
  | MayTransform TransformationId
  | MayEnact RuleKind

data RuleKind = TariffRule | AccessBanRule | WithholdingRule

authorized :: Array Capability -> EntityId -> Intent -> Boolean   -- INV-02
```

`authorized` is the matching function checked during Intent validation. A Repeal additionally needs enactment authority over the kind of the Rule it repeals (`authorizedIn`). Enacting or repealing a Rule also requires that the acting Entity governs it: its jurisdiction, or the owner of the capacity a Withholding withholds ([§3](#3-intents-and-economic-actions)). For example, a government may enact a tariff within its jurisdiction, while a small business may invest its own capital.

Institutional capabilities stay with the institutional Entity or rule mechanism that holds them. If an institution is not explicitly played, its capabilities remain with its Aggregate's collective procedures or with a declared rule, and never transfer automatically to a population Aggregate. Membership, Representation, and signing a procedure Proposal never transfer capabilities to the member or representative Actor.

### 2.5 Glossary

The seven concepts in §2 and this table are the only term definitions. The other specifications use these terms and do not redefine them.

| Term | Definition |
|---|---|
| **Actor** | An explicitly represented Entity, driven by a seat or NPC Policy. |
| **Explicit / implicit** | An Entity is explicit when it is an Actor, and implicit when its behavior and accounts sit in an Aggregate's residual. |
| **Controller** | What drives an Actor: a seat or an NPC Policy. |
| **NPC** | An Actor whose Policy is part of the premise. |
| **Seat** | An Actor whose Policy is supplied from outside the premise. Swapping the implementation never changes the Actor. |
| **Policy kind** | How a Policy is implemented: declarative strategy, AI agent, or human. |
| **Preset** | A named parameter set for a declarative strategy, such as the seat-policy preset `Conservative`. |
| **Strategy** | The declarative rules a Policy runs on the shared interpreter ([§4.4](#44-shared-decision-language-interpreter)). A Policy is the actor-level concept (a seat, NPC or Aggregate decides); its Strategy is the rules it runs. Only declarative Policies have one. |
| **View** | What a Policy sees of the Snapshot: its own Entity, prices, Rules in force, permitted events, stored adjustment factors, and the Proposals awaiting its signature. |
| **Decision-language interpreter** | The shared interpreter that runs every declarative Policy ([§4.4](#44-shared-decision-language-interpreter)). |
| **Round runner** | The effectful code that runs Propose and Sign for every Policy, then calls `resolveRound` ([§8.3](#83-functional-interface)). |
| **World State** | The economic state at a round boundary: Entities with accounts and capacities, Markets, Transformations, Representations, Rules in force and scheduled, Deliveries, stored adjustment factors, the numeraire, transport and jurisdiction links, the event history, the game seed, and the round number. |
| **Snapshot** | The World State a round ends with. The next round's decisions are all made on it, once Prepare has applied what is due. |
| **Account** | A quantity of a Resource, or a capacity, owned by exactly one Entity or Market. |
| **Money** | The numeraire Resource (World State `numeraire`): the Resource facility costs are paid in. |
| **Holding** | A quantity of a Resource held by an Entity or a Market's reserves. |
| **Capacity** | A throughput quantity an Entity holds, which Transformations draw on. Its owner is the Entity holding it. |
| **Residual** | The accounts and behavior of a population remaining in an Aggregate after explicitly modeled accounts are assigned elsewhere. |
| **Membership** | An Aggregate's `members` list: classification or organizational belonging. It confers no ownership, and agency only in procedures the member holds no weight in (§2.2). |
| **Collective procedure** | An Aggregate's weighted-signature rule for Proposals made in its name ([§2.2](#22-three-projections-of-an-entity)). |
| **Procedure Proposal** | A Proposal authored by an Aggregate under one of its collective procedures. It executes when the signed weight exceeds the procedure's threshold. |
| **Rule** | An institutional condition the engine interprets: a `Tariff`, an `AccessBan`, or a `Withholding` ([§3](#3-intents-and-economic-actions)). |
| **Jurisdiction** | The Entity that enacts and governs a Tariff or AccessBan against others. A Withholding is governed by its capacity's owner ([§2.4](#24-capability)). |
| **Proposal** | A message carrying one or more Intents and a set of signers. It executes only when its signature rule is met: every signer for a deal, or enough weight for a procedure Proposal ([§3.3](#33-proposals-and-signatures)). |
| **Signer** | An Entity whose signature a Proposal asks for. The author's signature is implicit in publishing. |
| **Lapse** | The outcome of a Proposal whose signature rule is not met by the end of the Sign step. It is a decline, not an error. |
| **Transformation** | A declared recipe that changes quantities, optionally priced by a CostFunction and limited by capacity. |
| **Facility** | The player-facing name for a costed Transformation, such as defense procurement. Its accounting is that of a Transformation, not a Market. |
| **Payee** | The Entity that receives Money paid into a facility. |
| **CostFunction** | A linear cumulative cost curve that prices quantity ([§5.1](#51-costfunction)). |
| **Uniform batch price** | The single price at which a Market clears its round batch ([§5.5](#55-simultaneous-clearing)). |
| **Best execution** | The Entity-facing form of an order that names a base and quote pair, not a Market. The engine routes it across the Markets for that pair the Entity can trade on ([§5.2](#52-exchange-markets)). |
| **Escrow** | The hold placed at screening on a Proposal's maximum cost, in any Resource, released at commit if unused ([§8.2](#82-single-round-update-and-snapshot-discipline)). |
| **Reservation** | A reserved quantity of a reservable Resource, such as deployed Military: unavailable within the round, not consumed. |
| **Shortfall** | An amount owed but not paid, because the payer's holdings were insufficient. Recorded, never driving a holding negative ([§8.2](#82-single-round-update-and-snapshot-discipline)). |
| **Delivery** | A scheduled change to a holding or capacity, applied at the start of the round it is due. |
| **Economic cycle** | The recurring production, exchange, income and expenditure, and capacity-renewal flows of [§6](#6-economic-circulation). |
| **Behavior domain** | The Resource whose purchases a Representation observes. The part of an Aggregate's baseline it may adjust ([§7.2](#72-representation-and-estimators)). |
| **Observation** | The admissible proposed buy quantity a Representation reads from its Actor's validated Proposals ([§7.3](#73-representation-pipeline)). |
| **Estimator** | The configured function that turns a representative's observed behavior into an adjustment factor ([§7.4](#74-estimation-semantics)). |
| **Adjustment factor** | The stored multiplier an Aggregate's baseline is scaled by in the following round ([§7.4](#74-estimation-semantics)). |
| **Fallback** | The declared resolution of an Actor definition that is not made explicit. |
| **Resolution** | A choice of which parts of a Gameboard's world are explicit and which are aggregate. |
| **Manifest** | The immutable record of a game's resolved configuration and initial World State, including the draw version. |
| **Round record** | The append-only record of one round's inputs, proposed and executed actions, and events. |
| **Effect** | A declared impure operation: seat input, interpretation, narration, recording, Run Store reads and writes, or an API call ([§8.4](#84-effects-and-handlers)). Randomness is not an effect. |
| **Game seed** | The single seed per game instance, fixed at initialization, from which every in-world random value is derived. |
| **Player seed** | A second seed, used only by the simulator, from which all seat-Policy randomness is derived. |
| **Draw key** | The semantic name of one random draw: round, source, Entity, category, index ([§9.1](#91-numerics-and-randomness)). |
| **Display kind** | One of Quantity, Price, or Ratio. Each has one display function ([§9.1](#91-numerics-and-randomness)). |
| **Premise** | The authored configuration a game is assembled from: the Gameboard, its instantiation configuration, the seat strategies, and the uncertainties it contains. Every scenario of a set shares it. A literal premise (`LiteralPremise`) writes its World State out in full, as a fixture does ([05 Part IV](05_test_instance.md#part-iv-playtest-fixtures)). |
| **Scenario** | One run of a premise with its uncertainties resolved: rounds recorded, narration produced. |
| **Scenario set** | The scenarios compared together, varying the uncertainties of one premise. |
| **Uncertainty** | An element a scenario set varies: a shock, a parameter override, the player seed, or a derived run. |
| **Gameboard** | The authored economic world of a premise: Resources, Markets, Facilities, Entities, Policies, Representation schemes, and Rules ([03](03_gameboard.md)). |
| **Game** | One run's assembled state, which every runner plays: the initial World State, the in-world Strategies, the rounds to play, the parameters, and one SeatBinding per seat. `assemble` builds it from a premise ([07 §4](07_architecture.md#4-key-interfaces)). |
| **SeatBinding** | What answers a seat's Propose and Sign steps: a `Strategy` (declarative rules), a `Script` (literal Proposals and signatures per round, including those read from stored round records), an `Agent` (a model call per round), or a `Human` (a transcript per round). |
| **Template** | A Strategy clause's proposal pattern: an Intent or Proposal whose quantities are expressions, or a combinator (`Choose`, `NoAction`, `ReciprocateLastRound`) ([§4.4](#44-shared-decision-language-interpreter)). |
| **Expect** | What a premise asserts about one round: sparse expected values, one per field, written as literals. It is checked against Observed. |
| **Observed** | What a round produced, read into the same record shape as Expect. |
| **Deal** | A Proposal that needs a signature from someone other than its author. It bundles only Transfers and Transforms and settles as a unit ([§3.3](#33-proposals-and-signatures)). |
| **Offer** | A deal as its author publishes it, before the other signers answer. |
| **Unilateral action** | A Proposal that needs no signature but its author's. |
| **Joint pass** | The one interpretation call that reads all of a round's transcripts together into Proposals ([§8.1](#81-canonical-resolution-sequence)). |
| **Run Store** | The persistent store of runs: manifests, specs, round records, snapshots, assertions, and metrics ([06 §8](06_simulator.md#8-persistence)). |
| **Spec** | A run's stored specification: the string fields it was launched or derived with, beside its manifest. |
| **Venue** | A Market as a Policy sees it in its View: its id, base, and quote ([§4](#4-policies)). |
| **Net worth** | Money plus each Resource that a Market trades against Money, valued at its marginal price. Where several Markets trade one Resource, its price is their mean, so a holding counts once. A Resource with no Market is not counted. |
| **Spread** | The highest minus the lowest final net worth of the players. |
| **Gini coefficient** | A measure of final net worths: 0 when every player holds the same and 1 − 1/n when one holds everything. It is n/a when the total is zero or a value is negative. |
| **Representation influence ratio** | Next-round Aggregate change attributable to a represented Actor, divided by that Actor's executed purchase. |
| **Vertical** | A researched sector, documented in a vertical report. An authoring concept that no schema field records. |
| **Strategic tension** | A persistent relationship among economic or political interests, expressed only through mechanically consequential data. |
| **Rerun** | The zero-change derivation of a run: its resolved configuration and both seeds with nothing changed, so its records must equal the source's. |
| **Counterfactual** | A derivation of a run that changes one thing by override and compares the result with the source. |
| **Propose deadline** | The end of a round's window for human transcripts in live play ([04 §8](04_instantiated_game.md)). |
| **Shape** | A typed description of a type that crosses a described boundary. One Shape gives the type's codec for each JSON format, its JSON Schema, and its prose ([§8.4](#84-effects-and-handlers)). |

## 3. Intents and Economic Actions

Intents are the only input to the engine, and every executed Intent came from a signed Proposal ([INV-01](#inv-01)). The engine never authors or rewrites an Intent, and it never knows how an Intent was produced ([§4](#4-policies)). The engine is an accountant: it checks authority and signatures and keeps the books balanced. Negotiation happens out of band.

The Intent algebra has four categories:

- **Trade:** An order on a public Market, with an optional limit price. A **Route** is a Trade with its Market omitted: a best-execution order for a base and quote pair ([§5.2](#52-exchange-markets)). A Route must carry a limit price.
- **Transfer:** A movement of a Resource from one Entity to another. Alone it is a gift or payment. Bundled in a multi-signer Proposal it is one side of a deal. Its `from` must be a required signer.
- **Transform:** Execute a declared Transformation, such as production, consumption, procurement, or investment. Its `by` must be a required signer.
- **ChangeRule:** Enact or repeal a Rule under the Entity's authority. A repeal names an existing Rule, and only the authority that governs it may repeal it ([§2.4](#24-capability)). Enactment takes effect at the next round boundary.

```purescript
type Order =
  { market :: MarketId
  , quantity :: Quantity          -- + buys, − sells the Market's base Resource
  , limitPrice :: Maybe Price
  }

type RouteOrder =                 -- a Trade with its Market omitted (§5.2)
  { pair :: { base :: ResourceId, quote :: ResourceId }
  , quantity :: Quantity
  , limitPrice :: Maybe Price     -- required
  }

type TransferSpec =
  { from :: EntityId, to :: EntityId, resource :: ResourceId, quantity :: Quantity }

data Intent
  = Trade Order
  | Route RouteOrder
  | Transfer TransferSpec
  | Transform { by :: EntityId, transformation :: TransformationId, quantity :: Quantity }
  | ChangeRule RuleChange

data Rule
  = Tariff { jurisdiction :: EntityId, origin :: EntityId, rate :: Price }
  | AccessBan { jurisdiction :: EntityId, target :: EntityId }
  | Withholding { capacity :: CapacityRef, units :: Quantity }

data RuleChange
  = Enact Rule
  | Repeal RuleId

type Proposal =
  { id :: ProposalId
  , author :: EntityId
  , intents :: Array Intent         -- settle atomically, together
  , signers :: Array EntityId       -- the author's signature is implicit
  , procedure :: Maybe CollectiveProcedureId   -- present on a procedure Proposal
  }
```

A Proposal binds Intents to the Entities asked to sign them. Each Intent is executed by the signer whose resources or authority it uses. A Proposal is visible to its signers. Gameboard visibility rules may make it more public. Intents are carried by Proposals and are never applied directly to state.

An Intent's economic meaning does not depend on who generated it: a seat, NPC, or Aggregate can all propose a Trade, and the engine determines whether it is permitted, feasible, and executable. Each Proposal records its author, the Entity whose seat or Policy produced it, in the round record.

### 3.1 Intent validation

Validation has two stages.

**Authorization validation** checks that the Entity exists, that its controller or Policy may submit the Intent, that the action falls within its capabilities ([§2.4](#24-capability)), and that any required Representation is valid.

**Economic feasibility validation** checks available (unreserved) resources, Market capacity, Transformation capacity, and other constraints. Joint feasibility is evaluated across the entire round, since individually valid actions may collectively exceed available resources.

The engine distinguishes a **valid Proposal** from its **executed outcome**. A Proposal may pass authorization and contribute an observation to a Representation, yet a Trade in it can still be partially filled or rejected at settlement. Both are recorded.

### 3.2 Infeasible actions

Infeasible actions are rejected or partially executed by deterministic rules. The engine must not silently invent resources or authority to make an action succeed.

- When an Entity's Proposals jointly exceed its opening available resources, measured by their escrowed maximum cost ([§8.2](#82-single-round-update-and-snapshot-discipline)), what it executes is scaled pro rata.
- When demand for a scarce quantity exceeds supply or capacity, fills are allocated pro rata by requested quantity.
- Residual indivisible units are allocated in canonical EntityId order, never by submission order.
- Orders whose limit price is violated at the batch price are removed at clearing ([§5.5](#55-simultaneous-clearing)).
- A multi-signer Proposal settles atomically, or is rejected and recorded as a unit.

Premises may declare alternative allocation rules, provided they are deterministic and order-independent.

### 3.3 Proposals and signatures

Every Intent reaches the engine inside a Proposal. Each round the round runner (`Effects.Round`) runs two steps against the same Snapshot ([§8.1](#81-canonical-resolution-sequence)):

- **Propose.** Every Policy publishes Proposals. The Proposal set is frozen at the end of this step.
- **Sign.** Every signer sees the Snapshot and the Proposals that name it as a signer, and signs those it endorses. Publishing implies the author's signature.

A **unilateral action** (a Market Trade, a gift Transfer, a Transform, a ChangeRule) has its author as the only required signer. An **offer** also requires its counterparties: SA's Proposal [Transfer 20 E to JP, Transfer 48 M to SA] with signers [SA, JP] is an offer, and JP's signature is the acceptance. An N-party deal requires N signers.

Multi-signer Proposals ("deals") may bundle only Transfers and Transforms. A deal containing a Trade, Route, or ChangeRule is rejected. Trades travel in single-author Proposals and follow the partial-fill rules ([§3.2](#32-infeasible-actions)). A deal is all-or-nothing: if its signed facility orders exceed a facility's capacity, every deal using that facility in that round is rejected as a unit and recorded. Single-author facility orders keep pro-rata fills. Each `ProposalId` is derived from (round, author, index in the author's submission), so recorded signatures replay deterministically.

A **procedure Proposal** is authored by an Aggregate under one of its collective procedures and names that procedure's members as signers. It executes when the signed weight exceeds the procedure's threshold ([§2.2](#22-three-projections-of-an-entity)), and is then the Aggregate's own action: it may carry any Intent the procedure authorizes, including a Trade, which fills under the normal partial-fill rules. The deal restrictions above do not apply to procedure Proposals.

A Proposal whose signature rule is not met **lapses**: it is recorded as a decline and the round resolves normally. Because acceptance is a signature on the offer's own terms, the two sides of a deal cannot disagree. A counteroffer is a new Proposal, signable next round, and the engine enforces no promises across rounds.

A fully signed Proposal is escrowed at screening like any action ([§8.2](#82-single-round-update-and-snapshot-discipline)). Each signer funds its own giving side from opening holdings. Its Intents settle atomically, or the whole Proposal is rejected and recorded, for example when escrow, transferability, or delivery fails.

Negotiation happens out of band for humans. The interpreter turns "we accept SA's deal" into a signature on that Proposal. Strategies propose and sign by rule (for example, "sign what is addressed to me while Energy is at most 2.6 M/E"). AI agents read the Proposals that name them and choose.

## 4. Policies

A Policy is anything that produces Intents from the authorized view of the Entity it drives:

**Policy + View + Memory + Turn → Intents**

```purescript
type View =                              -- what a Policy sees (INV-16)
  { round :: RoundIndex
  , self :: Entity
  , prices :: Map MarketId Price         -- beginning-of-round
  , rules :: Array Rule                  -- Rules in force
  , events :: Array Event                -- every recorded event, from prior rounds
  , factors :: Map BehaviorDomain Ratio  -- adjustment factors stored last round, for an Aggregate's Policy
  , toSign :: Array Proposal             -- Proposals requiring its signature (empty in Propose)
  , markets :: Array Venue               -- the Markets it has access to (id, base, quote), in id order
  }
```

The engine (World State + Intents → World State) is separate from the Policies that produce Intents. Policies come in three kinds:

| Kind | Implementation | Reproduced from |
|---|---|---|
| **Declarative strategy** | NPC rules, Aggregate baselines, seat-policy presets, run on the shared interpreter ([§4.4](#44-shared-decision-language-interpreter)) | The game seed |
| **AI agent** | A language model given the role's briefing, objectives, and authorized view | Recorded Intents |
| **Human** | A person whose audio submission, transcribed, is interpreted into Intents | Recorded transcripts and Intents |

Accepted Intents are always recorded, and replay starts from them. Deterministic strategies can also be regenerated from the seed.

A seat's Policy is supplied from outside and bound at run time. Swapping it (human, agent, or strategy) never changes the Actor's definition ([INV-30](#inv-30)).

**Policy memory is planned, not implemented.** No World State field holds it yet. Policies read the previous round's events and stored factors, and seat-Policy state lives in the run record ([06](06_simulator.md)).

### 4.1 NPC policies

An NPC is an explicit Actor whose Policy is part of the premise. Its decisions have no special authority and undergo ordinary validation. For example, an NPC government may respond to tariffs with retaliatory restrictions, or propose and sign offers with other Actors.

### 4.2 Aggregate policies

Every Aggregate has a baseline Policy governing the behavior of its implicit population. It declares which behavioral domains it controls, so Representations can adjust the correct outputs, and it decides when to publish a procedure Proposal ([§2.2](#22-three-projections-of-an-entity)).

Its generated actions provide the baseline behavior of the population. Each round it reads the adjustment factors stored in the previous round from its View and scales the matching baseline by the stored factor ([§7.4](#74-estimation-semantics)). With nothing stored, it follows its baseline.

Aggregates do not sign other Entities' Proposals: the round runner does not ask them to. Their own Proposals are signed by publishing. A Gameboard permission for Aggregates to sign multi-signer Proposals is not implemented.

### 4.3 Policy semantics

The rules below apply to in-world Policies (NPC and Aggregate). Their definitions are immutable and versioned for the duration of a game, including their conditional and time-dependent behavior ([INV-12](#inv-12)). Their outputs vary as the world evolves, because prices, shortages, rules, relationships, and observations are all World State that Policies react to.

An initialized Policy must:

- Operate only on an authorized view of the world.
- Produce typed Intents.
- Respect deterministic execution, using seeded randomness only where its definition explicitly calls for it.
- Remain distinct from institutional rules that players may be authorized to change. A seat enacting a tariff changes a Rule through `ChangeRule`. It does not rewrite any Entity's Policy.

### 4.4 Shared decision-language interpreter

All declarative strategies (NPC Policies, Aggregate Policies, and seat-policy presets) run on one shared **decision-language interpreter**. A strategy is a list of rules. Each rule has conditions (comparisons over prices, own holdings, stored factors, the round, and parameters, events of the previous round, Rules in force) and the templates it proposes: market and facility orders, deals bundling transfers and transforms, procedure Proposals, tariffs, access bans, withholdings, repeals of Rules the Entity governs, reciprocal restrictions, and a seeded choice among options. Sign rules cover procedures, facilities, or everything addressed to the Entity, each optionally guarded by conditions.

A Strategy names a Market by reference (`MarketRef`): by id (`Named`), or by a selector over the Markets the Entity has access to whose base is a Resource: `Each` (every one), `Cheapest` or `Costliest` (the one with the lowest or highest beginning-of-round price; ties keep the first by id, and a Market with no price is not a candidate). A clause that uses selectors is instantiated once per Market they pick, in Market-id order; distinct selectors in one clause range over the product of their picks, so one clause can buy at the cheapest and sell at the costliest. A selector that picks nothing instantiates nothing, so an Entity with no accessible Market for the Resource proposes nothing from that clause and the premise is not an error. A sign rule's conditions are instantiated the same way and hold when any instantiation holds. Selectors read `View.markets`; the Policy never sees World State. There is no notion of a home Market.

The interpreter drafts only the ordinary intents its Entity is authorized to execute, using the same `authorized` function the engine applies ([§2.4](#24-capability)). Procedure and deal intents are exempt, because the engine checks them against their own authority. A seat preset and an NPC Policy expressing the same rule therefore behave identically.

## 5. Cost Functions, Markets, and Transformations

### 5.1 CostFunction

Let C(q) be a cumulative cost function, where q is a cumulative quantity position. The marginal price is p(q) = dC(q)/dq, and the cost of moving the position by Δq is Cost(q, Δq) = C(q + Δq) − C(q).

```purescript
type CostFunction =
  { base :: Price, slope :: Price   -- p(q) = base + slope·q
  , domain :: Interval              -- supported position range
  }
```

Cost functions are linear. A Market's position persists across rounds. A facility's position is its total for the current round. A CostFunction prices quantity and never creates or destroys an asset by itself ([INV-07](#inv-07)).

### 5.2 Exchange Markets

An exchange Market trades existing resources held in its reserves.

```purescript
type Market =
  { id :: MarketId
  , base :: ResourceId
  , quote :: ResourceId
  , cost :: CostFunction
  , position :: Quantity
  , reserves :: Holdings
  , access :: Array EntityId        -- Entities that may trade on this Market
  }
```

The base Resource is the good being exchanged. The quote Resource is the denomination. The same cost function prices purchases and sales (positive and negative quantities), subject to domain and reserve constraints. Markets cannot supply resources they do not hold. A regional Market is expressed through `access`.

**Best execution (routing).** An Entity may place a routed order by naming a base and quote pair instead of a Market. From the Entity's point of view there is one market for the pair. The regional Markets remain the physical structure. In premise JSON a routed trade is a `route` (`base`, `quote`, `quantity`, `limit`), so it has no `market` field. The Route is a separate Intent constructor, so explicit Trades keep their encoding.

- **Candidates.** The Markets whose base and quote match, to which the Entity has access and for which it holds the trade capability. A routed order with no candidate is rejected at screening. An explicit order on an inaccessible Market is also rejected.
- **Limit.** A routed order must carry a limit price, for both sides. Screening rejects one without it. Its escrow ceiling is therefore its limit (for a buy, limit × quantity, for a sell, its quantity).
- **Merit order.** Candidates are taken in order of their opening marginal price: lowest first for a buy, highest first for a sell. Ties break by MarketId.
- **Split rule.** Routed orders are planned in canonical order (EntityId, ProposalId, Intent index), never submission order. Each candidate in merit order takes the largest quantity whose uniform batch price, including explicit orders and slices already planned there, stays within a cap: the limit, or the opening marginal price of the next candidate if that is tighter. Market bounds (domain and reserves) also bind. The rest passes to the next candidate. The last candidate's cap is the limit alone.
- **Clearing.** The planned slices are ordinary Market orders and clear with the rest of the Market's batch ([§5.5](#55-simultaneous-clearing)).
- **Guarantees.** The split is deterministic and order-independent ([INV-11](#inv-11)). An Entity's fills across Markets sum to at most its routed quantity, and no Market is filled beyond its domain or reserves ([INV-06](#inv-06)).
- **Non-guarantees.** The split does not equalise marginal prices across Markets. Explicit orders removed at clearing for their limit change the batch after planning, so a slice may stay unfilled rather than be re-routed. The unfilled remainder is not reported as a rejection.

### 5.3 Facilities (costed Transformations)

A facility is a capacity-constrained Transformation whose marginal cost is priced by a CostFunction.

```purescript
type Transformation =
  { id :: TransformationId
  , inputs :: Holdings            -- per unit
  , outputs :: Holdings           -- per unit
  , reserves :: Holdings          -- reserved per unit ordered (§2.3)
  , cost :: Maybe CostFunction    -- money price of congestion or increasing marginal cost
  , capacity :: Maybe CapacityRef -- limiting capacity, shared by facilities naming it; Nothing: uncapped
  , payee :: Maybe EntityId       -- recipient of money paid
  , delay :: RoundOffset
  , minimum :: Quantity           -- smallest order accepted
  , maximum :: Maybe Quantity     -- largest order accepted per Entity per round
  , capacityUse :: Quantity       -- capacity drawn per unit ordered
  , ownerSigns :: Boolean         -- the capacity's owner must sign every order
  , effects :: Array CapacityEffect
  }

type CapacityEffect =   -- per unit ordered, applied after the delay; holder defaults to the ordering Entity
  { capacity :: CapacityRef, holder :: Maybe EntityId, perUnit :: Quantity, rounds :: Maybe Int }
```

Investment and disruption are facilities with capacity effects: an investment raises a capacity after its delay, and a strike lowers another Entity's capacity for a limited number of rounds. Facilities that share a capacity draw on it by their `capacityUse` and share its limit. Those that also share a cost function share one uniform price, a single function of the pool's filled quantity. Escrow applies that function to the pool's ceiling: for a capped facility, the capacity ÷ the smallest `capacityUse` among the facilities drawing on it, rounded down, since any total of fills draws on the capacity at least that fast. For an uncapped facility (no capacity, or `capacityUse` zero), the top of its CostFunction's domain. Every member escrows at this one ceiling, and fills never exceed it, so the escrowed price bounds the settled price. When `ownerSigns` is set, the capacity's owner signs each order, which is how a transport owner chooses which shipments to carry.

Production, consumption, procurement, issuance, destruction, and investment are Transformations. They modify holdings only when the required inputs, capacities, and authority are available. Money paid to a facility goes to a declared payee Entity, so expenditure re-enters circulation. Facilities hold no reserves.

A facility may be presented as a "market" in the player interface, but its accounting is that of a Transformation: outputs are created only by the recipe, within capacity ([INV-07](#inv-07)).

### 5.4 Market scope and geography

Each commodity keeps a single Resource type. Geography is expressed through:

- Scoped or regional Market nodes, and market access rules that determine which Entities may trade.
- `Tariff` and `AccessBan` Rules applied to particular jurisdictions and origins ([§3](#3-intents-and-economic-actions)).
- Transport between nodes as a facility with limited capacity ([§5.3](#53-facilities-costed-transformations)). Two Entities are in the same region when some Market for the Resource lets both trade. A cross-region Transfer must be bundled with a matching order on the Resource's transport facility, executed by the sender and signed by the owner of that facility's capacity. An `AccessBan` makes the shipment infeasible. A `Tariff` adds its rate to the shipment, paid by the sender to the restricting jurisdiction.
- Facilities that share a capacity share its limit and its uniform price.

### 5.5 Simultaneous clearing

All orders in the round update are resolved as one batch, and each Market clears at a single uniform price. The steps below compute that price. They do not order individual orders. Tariffs do not enter clearing: they apply to shipped Transfers ([§5.4](#54-market-scope-and-geography)).

For a given Market:

1. Orders whose access rules fail are rejected.
2. Compatible purchases and sales are netted.
3. Residual net demand is executed against the cost function, within domain and reserve limits.
4. A uniform batch price is computed. The default is the average marginal cost over the net quantity, or the current marginal price when net demand is zero, rounded once before it multiplies quantities ([§9.1](#91-numerics-and-randomness)).
5. Orders whose limit price is violated at that price are removed, and the price is recomputed. Removed orders stay removed, so the procedure terminates and depends only on the set of orders. Partial fills follow [§3.2](#32-infeasible-actions).
6. Holdings and reserves change as part of the round's single net update.

The clearing result must not depend on the order in which submissions were received ([INV-11](#inv-11)).

### 5.6 Market-rule changes

A `Tariff` adds its rate to deliveries from the origin into the jurisdiction, paid to the jurisdiction's government. An `AccessBan` makes such deliveries infeasible. A `Withholding` removes units of a capacity. Rule changes take effect at the next round boundary ([§8.1](#81-canonical-resolution-sequence)).

## 6. Economic Circulation

Each round's single update includes a recurring stock-flow cycle with four components, computed together from the Snapshot ([§8.2](#82-single-round-update-and-snapshot-discipline)):

- **Production.** Authorized Transformations create outputs, such as Energy, from capacity and inputs.
- **Exchange.** Markets clear, and Money moves to suppliers and producers.
- **Income and expenditure.** Explicit rules distribute earnings, wages, taxes, public spending, and transfers among Entities.
- **Capacity renewal.** Investments, depreciation, and degradation adjust future capacities.

The cycle is data in World State and is applied by one function:

```purescript
type Circulation =
  { production :: Array { entity :: EntityId, resource :: ResourceId, capacity :: CapacityRef }  -- delivered next round
  , flows :: Array { from :: EntityId, to :: EntityId, resource :: ResourceId, amount :: Quantity } -- fixed income and expenditure
  , shares :: Array { from :: EntityId, to :: EntityId, share :: Ratio, source :: ShareSource }     -- variable flows
  , requirements :: Array Requirement }                                                              -- consumed at commit

data ShareSource = SalesOf ResourceId | PayeeReceipts   -- net sale revenue, or Money received as a facility payee

type Requirement =
  { entity :: EntityId, resource :: ResourceId
  , amount :: Maybe Quantity    -- Nothing: equal to this round's purchases of the Resource
  , penalty :: Quantity }       -- each unit unmet reduces the Entity's fixed incoming flows by this much
```

Every Resource stock reconciles each round: **Closing stock = Opening stock + Production + Issuance − Consumption − Destruction − Decay**, with transfers and trades summing to zero across all accounts, including Market reserves ([INV-05](#inv-05)).

## 7. Aggregates and Representation

Explicit Actors influence an implicit Aggregate in two distinct ways. **Representation** treats an Actor's behavior as a sample of the Aggregate's population and adjusts its baseline. A **collective procedure** lets members' signatures decide whether the Aggregate's own Proposal executes ([§2.2](#22-three-projections-of-an-entity)). Neither gives the Actor the Aggregate's resources or authority.

### 7.1 Aggregate semantics

An Aggregate maintains a `members` list, residual economic accounts and capacities, a baseline behavioral Policy, any collective procedures and institutional capabilities, and relationships to any representative Actors.

**An Aggregate's economic accounts are the residual after explicitly modeled accounts have been assigned to other Entities.** An explicit small-business Actor owns its own resources. The small-business Aggregate holds the resources attributed to the remaining, implicitly represented businesses. Making a government's head explicit extracts only the accounts and responsibilities assigned to that government Actor. The households and firms of that country remain in whichever Aggregate owns them.

### 7.2 Representation and estimators

A Representation links an explicit Actor to an Aggregate through an estimator.

```purescript
type Representation =
  { id :: RepresentationId
  , actor :: EntityId
  , aggregate :: EntityId
  , domain :: BehaviorDomain
  , weight :: Ratio                     -- this representative's share in the combination
  , estimator :: Estimator
  }

type Estimator =
  { reference :: Quantity               -- the representative's reference behavior
  , shrinkage :: Ratio                  -- 1 = deviation fully applied
  , deviationBound :: Maybe Interval    -- clamp on the deviation
  , noiseSpread :: Ratio                -- maximum seeded perturbation; 0 = none (§9.1)
  }
```

The domain constrains which part of the Actor's behavior may influence the Aggregate: a small-business owner's purchasing may inform small-business demand, but not the Aggregate's political behavior. Representation never transfers holdings or capabilities.

### 7.3 Representation pipeline

Representation proceeds through four stages, and the engine records each one:

1. **Observation.** Taken from a validated Proposal before funding and clearing. The observation is the actor's total proposed buy quantity of the behavior domain's Resource on Markets (Trades and Routes). It is what the actor asked to buy, not its executed fill, and it is not escrow-backed: funding may later scale it down.
2. **Estimate.** The adjustment factor, computed and stored by the engine.
3. **Proposed Aggregate Intent.** The Aggregate Policy's adjusted Proposal.
4. **Executed result.** The settled outcome after feasibility checks.

The observation and estimate belong to round r. The proposed Intent and executed result to round r + 1. Any difference between intended and executed actions is recorded.

### 7.4 Estimation semantics

Representation is an inference operation, never a resource multiplier. Each representative's deviation from its reference is perturbed, then clamped:

**Deviationᵢ = clamp(deviationBound, (observedᵢ − referenceᵢ) / referenceᵢ + εᵢ)**

**εᵢ = noiseSpreadᵢ × (2uᵢ − 1)**, where uᵢ is the seeded draw for this input ([§9.1](#91-numerics-and-randomness)).

Sampling noise models the uncertainty of inferring a population from a few representatives, so one decision moves the Aggregate by a slightly different amount in each game. Noise is added before clamping, so the deviation bound still caps the total effect, and it applies only to actual observations. A representative with a zero reference contributes no estimate.

**Adjustment factor = 1 + shrinkage × Σ(weightᵢ × deviationᵢ)**

The sum runs over the Representations targeting that Aggregate and domain. The engine computes the factor from round r's validated Proposals and stores it in World State. It never creates or changes a Proposal. In round r + 1 the Aggregate's Policy proposes:

**Adjusted target = Baseline × stored factor**

where the baseline is that of round r + 1, replaced exactly once.

Representatives are strategic players, not random samples, and have an incentive to act for influence. The type system does not eliminate this, because disproportionate influence for small players is a design goal. It makes influence explicit and bounded. Estimator parameters are declared balancing assumptions, not claims of unbiased inference. When no observation was made in the previous round, no factor is stored and the Aggregate follows its baseline.

### 7.5 Multiple representatives

Multiple Actors may represent the same Aggregate. Their deviations are combined by each Representation's `weight`, which accounts for overlapping and possibly biased samples. Representatives are not automatically treated as independent or equally informative.

### 7.6 Avoiding double-counting

**Economic accounting:** An explicit Actor's accounts are excluded from the target Aggregate's accounts, and its direct transactions execute once.

**Behavioral accounting:** In the following round, the Aggregate's adjusted behavior replaces the corresponding baseline behavior. It is never added to an already complete baseline action ([INV-09](#inv-09)).

A representative decision therefore has two independently validated effects: a direct effect on the representative Actor's own holdings, and an estimated effect on the Aggregate. Neither is counted twice.

## 8. World State and Round Resolution

World State is the complete economic snapshot at a round boundary ([§2.5](#25-glossary)). Policies read World State but cannot mutate it. They only submit Intents. The engine advances the world through one deterministic round-resolution function.

### 8.1 Canonical resolution sequence

This is the single authoritative resolution sequence. The Gameboard, Instantiated Game, and Simulator specifications reference it and do not redefine it.

**1. Prepare round** *(engine)*
Apply due deliveries and rule changes scheduled at the previous boundary.

**2. Create observations** *(engine)*
Produce the beginning-of-round Snapshot and each Entity's View of it ([§4](#4-policies)).

**3. Generate Proposals** *(round runner)*

Every seat without an in-world Policy speaks first. Their transcripts are read in one joint pass (`INTERPRET`), and the Proposals it reconciles join the set frozen in 3a. No joint call is made when nobody speaks.

- **3a Propose.** Every Policy publishes Proposals against the Snapshot at the same time: seats through whatever Policy drives them (human via the AI interpreter, AI agent, or strategy), NPC Policies, and Aggregate Policies, including any procedure Proposals. The set is frozen at the end of 3a.
- **3b Sign.** Every signer signs the Proposals naming it that it endorses ([§3.3](#33-proposals-and-signatures)).

**4. Screen and interpret** *(engine)*
Check authority, Entity identity, and each Proposal's signature rule: every signer for a deal, support above the threshold for a procedure Proposal. Proposals that fail their rule lapse and are recorded. Extract admissible observations from validated Proposals.

**5. Estimate** *(engine)*
Compute each Representation's adjustment factor ([§7.4](#74-estimation-semantics)) from this round's validated Proposals and store it in World State. The engine creates and modifies no Proposal.

**6. Resolve economy** *(engine)*
Fund every accepted action against the Snapshot, then settle and clear all of them as one simultaneous batch ([§8.2](#82-single-round-update-and-snapshot-discipline)). Markets clear at uniform prices. Transformations, signed deals, passed procedure Proposals, and the economic cycle ([§6](#6-economic-circulation)) are evaluated together, and the result is one net change to World State.

**7. Commit and record** *(engine)*
Atomically commit the new World State, and record the round's inputs and events.

### 8.2 Single round update and snapshot discipline

All Actor and Aggregate actions for a round are bundled into one round update and resolved together. Clearing is uniform, so no action settles "before" another and submission order is irrelevant ([INV-11](#inv-11)). The steps in [§8.1](#81-canonical-resolution-sequence) are functional dependencies, such as screening before estimation. They never order individual actions. Two rules make this hold:

- **Funding from the Snapshot.** Each Entity's bundle is funded from its available holdings at the beginning of the round. At screening, each signed Proposal escrows its maximum cost: for a buy, limit price × quantity (or the Market's domain upper-bound price when there is no limit). For a sell, its quantity. A routed order always has a limit ([§5.2](#52-exchange-markets)), so its escrow is limit × quantity for a buy and its quantity for a sell, whichever Markets it fills in. A facility order escrows at its pool ceiling ([§5.3](#53-facilities-costed-transformations)). A reservation escrows its `reserves` times its quantity as one more claim. Joint feasibility and pro-rata scaling ([§3.2](#32-infeasible-actions)) are checked against these escrows. Every deal of a party whose total deal demand exceeds its holdings is rejected, even if one of those deals alone could be funded. Unused escrow is released at commit, and a reservation consumes and transfers nothing.
- **Credit at commit.** Proceeds, production, income, and receipts from signed Proposals are credited when the round commits and become usable next round. An Entity cannot sell Energy and spend the proceeds in the same round. Selling and buying both draw on opening holdings.

Every payment out of an Entity is capped at what that Entity holds after the batch so far: facility payments, Transfers, tariffs, and circulation flows alike. Any remainder is recorded as a Shortfall rather than driving a holding negative. Escrow makes the cap bind only in error. It is a backstop, not a second pricing rule. Market fills are not capped: clearing bounds them and escrow funds them. If a settled holding or Market reserve would still be negative, the commit fails the round with an engine error, which is an invariant failure ([INV-04](#inv-04)), not an infeasible action.

Passive consumption rules (such as operating requirements), decay, and derived flows (such as revenue sharing) are functions of the batch result, evaluated on each Entity's net position at commit. All decisions in a round operate on the beginning-of-round Snapshot. Everything a round computes becomes observable in the next round, so there is no same-round feedback.

### 8.3 Functional interface

The engine's operation is a pure state transition, and the round runner wraps it with the effectful Propose and Sign steps:

```purescript
type SignedProposal = { proposal :: Proposal, signatures :: Array EntityId }

-- Engine (pure): signed Proposals are its only input
resolveRound :: WorldState -> Array SignedProposal
             -> Either EngineError RoundResult

-- Round runner (Effects.Round): runs phases 1–3 with effects, then resolveRound
runRound :: InWorld -> WorldState
         -> Run (SEAT_INPUT + INTERPRET + RECORD + NARRATE + r) (Either EngineError RoundResult)
```

`runRound` evaluates every Policy, seat or in-world, through the same code in live play and simulation. Its `InWorld` argument is defined in `core` (`Core.Strategy`). In-world declarative strategies run on the shared interpreter. `resolveRound` screens authority and signature rules, records lapsed and rejected Proposals, stores estimates, runs the economic cycle and clearing, and commits the new World State with the round record.

`EngineError` reports an invariant failure in settlement: a negative holding or Market reserve that settlement should have prevented ([INV-04](#inv-04)). Lapsed Proposals and ordinary infeasible actions are outcomes recorded as events and never fail the round.

### 8.4 Effects and handlers

The engine has no effects: `resolveRound` is a pure function of World State and signed Proposals. In-world randomness uses the pure keyed `draw` function ([§9.1](#91-numerics-and-randomness)) with the game seed held in World State, so it needs no effect.

Everything else impure is an algebraic effect. Signatures are declared in `Effects.Signatures` (and the API row in `Effects.Api`) with no implementation (the joint pass's `Transcript` and `JointProposal` are domain values in `Core.Proposal`), and the round runner `Effects.Round` is written against them:

| Effect row | Operation | Meaning |
|---|---|---|
| `SEAT_INPUT` | `Propose`, `Sign`, `Speak` | A seat's Proposals in Propose, its signatures in Sign, and its transcript in Speak |
| `INTERPRET` | `Interpret` | The round's transcripts to Intents, read together across players. No confirmation (live play) |
| `NARRATE` | `Narrate` | Resolved events to front-page and balance-sheet text, requested after each round. Adds no facts |
| `RECORD` | `Record` | Append a round result, with the transcripts its joint pass read, to the run's records. Each round's write is `RECORD` |
| `OBSERVE` | `ListRuns`, `ListSummaries`, `ReadManifest`, `ReadRounds` | Run Store reads. A read program names only this row, so it cannot write. Unreadable stored fields come back as `Left` |
| `STORE` | `CreateRun`, `SetStatus`, `FailRunning`, `SetSpec`, `SetArchived`, `DeleteRun` | Run Store lifecycle writes only ([`Effects.Store`](07_architecture.md#4-key-interfaces)). Round writes stay `RECORD` |
| `API` | `Call` (an `ApiRequest`, answered by a `Reply`) | Every route of the dashboard's HTTP API. Its paths and codecs are in `Handlers.Api.Route`, and the server supplies what each operation does ([07 §4](07_architecture.md#4-key-interfaces)) |

Operation payloads are plain records in `effects` today. Where a payload's type is an engine or premise-format type, its description is a `Shape` in `core` or `schema`, which gives its codec and JSON Schema from one declaration.

Handlers interpret these operations over typed backends; the adapters supply the raw I/O primitives ([07 §4](07_architecture.md#4-key-interfaces)).

Handlers choose the context (live session, simulation, replay, or test). The round runner is the same code under all of them. Determinism follows from purity: identical handler inputs (seeds and recorded seat input) give an identical run, and replay is running under the replay handler. The per-context behavior is in [04](04_instantiated_game.md) and [06](06_simulator.md).

## 9. Architectural Invariants and Numerics

This is the single numbered invariant table for the whole system. The Gameboard, Instantiated Game, and Simulator specifications cite these IDs and keep no tables of their own. Layer tags: **E** engine, **G** Gameboard, **I** instantiation, **H** harness.

| ID | Layer | Invariant | Requirement |
|---|---|---|---|
| <a id="inv-01"></a>INV-01 | E | Unified action semantics | All participants act through the same Intent and validation machinery. The engine never authors or rewrites Intents: every executed Intent came from a signed Proposal. |
| <a id="inv-02"></a>INV-02 | E | Authority separation | An Entity exercises only capabilities it has been granted, within scope. |
| <a id="inv-03"></a>INV-03 | E, G, I | No authority amplification | Membership, Representation, signing a procedure Proposal, and fallbacks never confer or remove an institution's capabilities or holdings. |
| <a id="inv-04"></a>INV-04 | E | Resource consistency | Holdings change only by authorized exchange, transformation, issuance, destruction, or decay, per the Resource's declared properties and Transformations. No holding and no Market reserve is negative. |
| <a id="inv-05"></a>INV-05 | E, G, I | Stock-flow reconciliation | Each round every Resource stock reconciles ([§6](#6-economic-circulation)), transfers and trades net to zero, every Money payment has a recorded recipient, and nothing accumulates in an account without a declared outflow rule. |
| <a id="inv-06"></a>INV-06 | E, G | Market solvency | Market positions stay within their cost function's domain, and facility fills never exceed their capacity or escrowed ceiling. |
| <a id="inv-07"></a>INV-07 | E, G | Price is not production | A CostFunction never creates an asset without an authorizing Transformation. |
| <a id="inv-08"></a>INV-08 | E, G, I | Account partition | Every account and behavioral responsibility has exactly one owner, however memberships overlap. |
| <a id="inv-09"></a>INV-09 | E | Representation consistency | A stored estimate replaces the matching baseline exactly once, in the following round, within estimator bounds, and never duplicates direct activity. |
| <a id="inv-10"></a>INV-10 | E | Signature completeness | A Proposal executes only if its signature rule is met: every required signer for a deal, or support above the threshold for a procedure Proposal. A deal's Intents settle atomically or are rejected together. |
| <a id="inv-11"></a>INV-11 | E | Order independence | All actions resolve as one bundled round update with uniform clearing. Permuting submissions never changes settlement, and no action depends on another settling first. |
| <a id="inv-12"></a>INV-12 | E, G | Policy reproducibility | Policy output is determined by its versioned definition, permitted view, World State memory, time, and declared randomness. |
| <a id="inv-13"></a>INV-13 | E, H | Deterministic replay | The same manifest, seeds (game, and player where applicable), and recorded seat input reproduce identical World States. Every random value is a keyed draw ([§9.1](#91-numerics-and-randomness)). |
| <a id="inv-14"></a>INV-14 | E, G, I, H | Resolution consistency | Instantiating a world at different levels of explicitness reconciles accounts, liabilities, and authority before any decisions differ. |
| <a id="inv-15"></a>INV-15 | E, I | Single authoritative state | All actions modify one shared World State and timeline. |
| <a id="inv-16"></a>INV-16 | E, I | Information isolation | Every Policy, in-world or seat, observes only what the Gameboard permits. |
| <a id="inv-17"></a>INV-17 | G | Type-system conformity | All premise definitions use supported types and operations. |
| <a id="inv-19"></a>INV-19 | G | Role consequence | Every Actor definition declares a capability, a dependency, and a measurable objective. |
| <a id="inv-21"></a>INV-21 | G | Instantiation feasibility | The Gameboard admits at least one valid initial world. |
| <a id="inv-22"></a>INV-22 | G | Version stability | A Gameboard version's definitions are immutable. |
| <a id="inv-23"></a>INV-23 | I | Unique control | Every explicit Actor has exactly one valid controller. |
| <a id="inv-24"></a>INV-24 | I | Valid participation | Every selected Actor and Representation is permitted by the Gameboard and validly parameterized. |
| <a id="inv-25"></a>INV-25 | I | Fallback completeness | Every unselected Actor definition has a permitted fallback. No required responsibility is left unowned. |
| <a id="inv-26"></a>INV-26 | I | Policy completeness | Every NPC and Aggregate has an executable initialized Policy. |
| <a id="inv-27"></a>INV-27 | I | Market feasibility | Initial Market positions, reserves, and capacities satisfy economic constraints. |
| <a id="inv-28"></a>INV-28 | I | Temporal consistency | Round intervals and Policy timing fit the simulated horizon. |
| <a id="inv-29"></a>INV-29 | H | Engine and interpreter reuse | Simulated games use the same engine, resolution sequence, and decision-language interpreter as real games. |
| <a id="inv-30"></a>INV-30 | H | Controller integrity | Swapping a seat's Policy implementation never changes the Actor's definition and never turns a seat into an NPC. |
| <a id="inv-31"></a>INV-31 | H | Authoritative records | Manifests, append-only round records, and snapshots are authoritative and immutable. Metrics are versioned projections. |
| <a id="inv-36"></a>INV-36 | E, H | Purity | The engine is a pure function. All impurity occurs only through declared effects interpreted by handlers ([§8.4](#84-effects-and-handlers)). |

IDs 18, 20, and 32–35 are retired.

**Rejection reasons in records.** A `RejectReason` is stored as its rendered text, so a stored event compares equal to the event that was recorded. Structured reasons would change the record format at the replay boundary, and no consumer needs them.

**Enforcement status. Predicates in `Core.Invariants` check INV-01 (settlement events against Intents and declared flows), 02, 03, 04 (no negatives and per-Entity reconciliation), 05, 06 (domain, capacity, and ceiling), 08 (one positive holder of a facility's capacity), 09 (factors within estimator bounds), and 10 (signatures and procedure support) on every round in the test suites and simulator. Gameboard validation and instantiation check parts of INV-03, 05, 08, 14, 17, 19, 21, 23–28. INV-07, 11, 12, 13, 15, 16, 29, 30, 31, and 36 have no predicate, for these reasons. INV-15, 29 and 36 are design rules that the types and the effect boundary enforce. INV-07 is implied by INV-05, since only a Transformation can source a stock change. INV-11, 12 and 13 are properties of two runs or of a replay, so they are checked by the named tests in the packages' test suites. INV-16 needs the Gameboard permitted views, so it is not a predicate on one round. INV-30 and 31 are harness properties, not round predicates. INV-22 has no check yet.

### 9.1 Numerics and randomness

Mechanics are deterministic by default. Randomness is introduced only where a Gameboard rule or estimator explicitly calls for it, such as estimator sampling noise.

**Seeds.** Each game instance has exactly one **game seed**, fixed in its configuration at initialization and recorded in the manifest. Every in-world draw (estimator sampling noise, declared Policy randomness) uses it. The simulator adds a **player seed** for all seat-Policy randomness. It exists only under the simulation handler, and live play has none. A seed is either injected or drawn once at initialization by the adapter layer, the only place OS entropy enters. The drawn value is recorded. During a run there is no entropy, so the same seed gives the same run. The two seeds never share draws, so varying the player seed never changes an in-world draw, and vice versa.

**Draws.** Every draw is one pure function, so randomness needs no effect:

```purescript
draw :: Seed -> DrawKey -> Q        -- pure, in core; a value in [0, 1) at 4-decimal precision
```

A `DrawKey` names the draw's semantic purpose: round, source (Representation or Policy), input Entity, category, and index. For example `round-3 / sme-energy-sample / SME / sampling / 0`. Because draws are keyed rather than consumed from a sequence, adding an Entity or changing evaluation order never changes unrelated draws. The PRNG algorithm and the key serialization are versioned in the manifest.

**Fixed point.** All quantities, prices, and settlement amounts use fixed-point arithmetic with **4 decimal places**, rounded **half-to-even** at each settlement step. Uniform batch prices are rounded before they multiply quantities. No floating-point arithmetic enters economic state. Other specifications reference this rule and do not restate it.

**Principle: storage is exact, display is shorter.** Storage, computation, JSON encodings, and parse rules stay at four decimal places. Only what a person reads changes, and it changes through one display function per display kind, so no call site formats a number itself.

The rules are implemented once, in `Core.Num` (`Q`). Every boundary imports that module. No other code parses, rounds, or formats an economic value.

- **Representation.** A `Q` is an arbitrary-precision integer count of 10⁻⁴ units. Addition, subtraction, and negation are exact. There is no overflow or underflow. A negative holding is an invariant violation ([INV-04](#inv-04)), not a numeric event.
- **Rounding.** A product or quotient off the 4-decimal grid is rounded half-to-even (ties to the even last digit, symmetric in sign). Rounding at the grid is the only rounding: nothing is truncated silently.
- **Remainders.** A total split among shares uses `proRata` (or `proRataWhole`, which first truncates the total toward zero to whole units): largest remainder, ties to the earlier index in canonical order. The shares sum to the total, so nothing is created or lost. A quotient used as a *scale* (a fraction of a claim that may be funded) uses `divFloor`, truncated toward zero, so that the scaled amount never exceeds its numerator. A half-even quotient can round up and overdraw.
- **Division by zero.** Explicit. `div` fails loudly. A caller whose divisor can be zero uses `divMaybe` or `divFloor` and handles `Nothing`.
- **Parse.** `Q.parse` is the only decimal parser: an optional sign, one or more digits, and optionally a point followed by one to four digits. Anything else, including a fifth decimal, is an error. No input is rounded silently.
- **Format.** `Q.format` is the exact formatter: an optional `-`, the integer part, and exactly four decimals, with no grouping and no trailing-zero trimming. It is the text of `show`, of JSON encoding, and of the overrides JSON. It is never shown as a display value.
- **Display.** `Q.display` dispatches on a `Q.Kind` (Quantity, Price, Ratio), and each kind has one function: `Q.displayQuantity`, `Q.displayPrice`, and `Q.displayRatio`. Quantity and Price currently display alike. Every view, table, chart, and CLI line that shows a number names its kind and calls one of these. All three round the exact value half-to-even at the display precision:
  - **Quantity** (Money and Resource counts): a whole number shows no decimals. Otherwise at most two decimals, trailing zeros trimmed (`12`, `12.5`, `12.25`). A value that is not zero but would round to zero shows as `<0.01` (or `-<0.01`).
  - **Price** (quote per base unit): two decimals, trailing zeros trimmed, under the same rule as Quantity.
  - **Ratio** (Gini, influence, shrinkage, factors, support and threshold, tariff rates): always two decimals.
  - **Chart axes:** `Q.displayTick` rounds an axis tick to the 0.0001 grid and displays it under the chart's kind. A tick label is never parsed back into a quantity.
- **Which kind.** The dashboard, the CLI, and the Compare views use these kinds as [06 §9](06_simulator.md#9-pages) states.
- **Floating point.** No `Number` enters economic state. `Q.toDisplayNumber` is the single conversion of a quantity to a float, for display only (chart plotting). Its result is never parsed back into a quantity. Integers that are not economic values (rounds, seeds, ports, counts) remain `Int`.

## 10. Design Boundaries

The type system deliberately does not prescribe the following:

**Specific resource classifications.** These belong to the Gameboard, informed by vertical research. The type system supplies only Resource properties.

**A universal macroeconomic model.** Economic behavior is expressed through configured Policies, Markets, Transformations, income rules, and institutional rules.

**A single estimator parameterization or procedure threshold.** Estimators and collective procedures are configured per premise.

**An LLM-based economic authority.** Language models interpret submissions and narrate outcomes. They do not bypass economic validation or directly mutate World State.

**A fixed degree of market realism.** Cost functions are linear. Other curve families or clearing mechanisms may be added later behind the same interfaces.
