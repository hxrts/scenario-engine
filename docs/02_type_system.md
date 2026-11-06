# Scenario Planning Game — Type System Specification

**Version:** 0.2  
**Scope:** Type System Layer  
**Implementation language:** PureScript

---

## 1. Purpose

The Type System Layer defines the reusable economic machinery of the Scenario Planning Game: a minimal set of types and operations through which human players, NPCs, and implicit populations act in one shared, deterministic economy. The whole system is pure: all impurity is expressed as algebraic effects interpreted by handlers ([§8.4](#84-effects-and-handlers)). It is independent of any scenario, resource taxonomy, or conflict; those come from the Gameboard and Instantiated Game layers. It stays small by giving existing primitives precise semantics rather than adding a primitive for every phenomenon.

## 2. Core Ontology

The economic engine is built around seven concepts.

| Type | Definition |
|---|---|
| **Resource** | An economic quantity that can be owned, exchanged, produced, consumed, reserved, or allocated. |
| **Entity** | An economic participant with accounts, capabilities, and state. |
| **Policy** | Anything that produces Intents from the authorized view of the Actor or Aggregate it drives: a declarative program, an AI agent, or a human ([§4](#4-policies)). |
| **Market** | A mechanism that exchanges existing resources, determines prices, and settles exchange. |
| **Aggregate** | An Entity representing the residual economic accounts and baseline behavior of an implicit population or institution. |
| **Representation** | A relationship through which an explicit Actor's sampled behavior adjusts an Aggregate's baseline through an estimator. |
| **Intent** | A typed economic or institutional action. |

One supporting mathematical primitive is shared across the system:

- **CostFunction:** A cumulative cost curve that prices quantity. It is used by exchange Markets and by facilities. It is not an Entity type and never creates resources by itself.

Derived concepts (Actor, Controller, NPC, Seat, Transformation, Proposal, World State) are defined in the glossary ([§2.5](#25-glossary)).

An Aggregate is a specialization of Entity, not a separate economic ontology.

### 2.1 Entity representation

Every Entity has the same economic record; its `kind` says whether it is an Actor or an Aggregate.

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
  = Seat SeatId     -- Policy supplied from outside the scenario; implementation bound at run time
  | NPC PolicyId    -- Policy defined by the scenario
```

Identifiers, Holdings, Capacities, and numerical units are domain types defined elsewhere. The snippets specify architectural interfaces rather than complete executable modules.

Actors and Aggregates own accounts, hold capabilities, and act in the same Markets and Transformations. They differ only in how their decisions are produced: an Actor is driven by a seat or NPC Policy, an Aggregate by its baseline Policy and any collective procedures.

A government, a corporation, or a small-business owner may be an Actor. Households, an industry, or a supranational bloc may be Aggregates. A government Actor holds government-controlled resources only ([§7.1](#71-aggregate-semantics)).

### 2.2 Three projections of an Entity

Membership, economic ownership, and collective decision-making are distinct relationships. The type system keeps them separate rather than letting the word "Aggregate" carry all three meanings.

| Projection | Question it answers | Rule |
|---|---|---|
| **Membership** | Which Entities belong to this population or organization? | An Aggregate's `members` list. It is descriptive, may overlap with other Aggregates, and confers nothing. |
| **Account ownership** | Who authoritatively owns each economic account and behavioral responsibility? | Every account and responsibility has exactly one owner. |
| **Collective procedures and capabilities** | How does an institution decide, and what authority does it hold? | Authority belongs to the institutional Entity or a rule mechanism, never to members by virtue of membership. |

```purescript
type CollectiveProcedure =
  { weights :: Map EntityId Ratio      -- member weights
  , residualWeight :: Ratio            -- weight of the Aggregate itself (the implicit members)
  , threshold :: Ratio                 -- passes when support exceeds it
  , authorizes :: Array Capability     -- what a passed Proposal may do
  }
```

A collective procedure is a weighted-signature rule on the Aggregate's own Proposals. The Aggregate's Policy publishes a **procedure Proposal** ([§3.3](#33-proposals-and-signatures)): its Intents fall within `authorizes` and use the Aggregate's own accounts and authority, and its signers are the procedure's members. A member supports by signing and declines by not signing:

**Support = residualWeight + Σ weightᵢ over the members who signed**

Publishing implies the author's signature, so the residual weight always counts. The Proposal executes in that round if support exceeds `threshold`; otherwise it lapses. The decision is proposed, signed, and executed within one round. The engine records the members who signed, the support, the threshold, and the outcome.

A member that is not explicit has its weight folded into `residualWeight`; instantiation moves weight between the two, as part of resolution consistency ([INV-14](#inv-14)).

For example, Germany is in the EU's `members` whether or not it signs in a given round. The EU Aggregate owns the residual European accounts not assigned to other Entities. Germany influences the EU's procedures by its declared weight without owning EU accounts or exercising EU capabilities.

A manufacturer may belong to the small-business population, the manufacturing sector, and a regional economy at once. Those memberships do not permit three Aggregates to own its assets. Accounting is partitioned; classification is not.

### 2.3 Resource

A Resource is an identifiable economic quantity with a defined unit and declared behavior.

Resource behavior is configured through properties, not hard-coded per resource:

```purescript
type ResourceDefinition =
  { id :: ResourceId
  , unit :: Unit
  , transferability :: Transferability
  , decay :: Maybe DecayRule
  , reservable :: Boolean
  }
```

`Transferability` distinguishes freely transferable, restricted (authority-gated), and non-transferable Resources. A `reservable` Resource can be reserved, for example by a military deployment, without being consumed. A Transformation declares the quantity it reserves per unit ordered (`reserves`, empty by default). The Gameboard rejects a reservation of a Resource that is not `reservable`. Which Transformations produce or consume a Resource completes its behavior: a Resource that none produces or consumes is conserved, which is derived, not declared.

Holdings associate Entities with quantities of Resources. A reservation lasts within one round and is carried by escrow ([§8.2](#82-single-round-update-and-snapshot-discipline)). At screening, a Transform order escrows its `reserves` times its quantity against the holder's opening holdings, as one more claim beside its input and price claims. Joint feasibility and pro-rata scaling count that claim exactly as they count any other, so an order scaled down reserves only the scaled amount, and a competing Transfer, Trade, or Transform by the same Entity can use only what the reservation leaves unreserved. At settlement a reservation consumes and transfers nothing: the reserved quantity stays in the holder's holdings and is released at commit, so a deployment with no inputs leaves its reserved Resource unchanged, and nothing stays reserved across a round boundary.

Not everything economically meaningful is a Resource. Production capacity, transport capacity, market access, political authority, and supply-chain efficiency are economic state or rules attached to Entities, Markets, and Transformations. A semiconductor producer can hold valuable productive capacity without the scenario introducing a semiconductor Resource.

An Entity's holdings are distinct from its capabilities. A government may possess authority to modify taxation or trade restrictions without owning the resources of the affected private sector.

Likewise, an Actor cannot spend, transfer, or create resources merely because it has political influence over their owners.

### 2.4 Capability

A Capability is a pattern over Intents: it says which actions an Entity is authorized to perform. Authorization is one matching function, checked during Intent validation.

```purescript
data Capability
  = MayTrade MarketId
  | MayTransfer ResourceId
  | MayTransform TransformationId
  | MayEnact RuleKind

data RuleKind = TariffRule | AccessBanRule | WithholdingRule

authorized :: Array Capability -> EntityId -> Intent -> Boolean
```

The scope of `MayEnact` is implied: the acting Entity must be the Rule's `jurisdiction`, or the owner of the capacity it withholds ([§3](#3-intents-and-economic-actions)). For example, a government may enact a tariff within its jurisdiction, while a small business may invest its own capital.

Institutional capabilities stay with the institutional Entity or rule mechanism that holds them. If an institution is not explicitly played, its capabilities remain with its Aggregate's collective procedures or with a declared rule, and are never transferred automatically to a population Aggregate.

Membership, Representation, and signing a procedure Proposal never transfer capabilities to the member or representative Actor.

### 2.5 Glossary

This table is the single source of term definitions. The other specifications use these terms and do not redefine them.

| Term | Definition |
|---|---|
| **Actor** | An explicitly represented Entity, driven by a seat or NPC Policy. |
| **Controller** | What drives an Actor: a seat or an NPC Policy. |
| **NPC** | An Actor whose Policy is part of the scenario. |
| **World State** | The current economic state: Entities with their accounts and capacities, Markets, the Rules in force, reservations, stored estimates, Policy memory, the game seed, and time. |
| **View** | What a Policy sees of the snapshot: its own Entity, prices, Rules in force, permitted events, stored estimates, and the Proposals awaiting its signature ([§4](#4-policies)). |
| **Rule** | An institutional condition the engine interprets: a tariff, an access ban, or a capacity withholding ([§3](#3-intents-and-economic-actions)). |
| **Account** | A quantity of a Resource, or a capacity, owned by exactly one Entity. |
| **Residual** | The accounts and behavior of a population remaining in an Aggregate after explicitly modeled accounts are assigned elsewhere. |
| **Membership** | An Aggregate's `members` list: classification or organizational belonging. It confers neither ownership nor authority. |
| **Collective procedure** | An Aggregate's weighted-signature rule for Proposals made in its name ([§2.2](#22-three-projections-of-an-entity)). |
| **Procedure Proposal** | A Proposal authored by an Aggregate under one of its collective procedures. It executes when the signed weight exceeds the procedure's threshold. |
| **Estimator** | The configured function that turns a representative's observed behavior into an Aggregate adjustment. |
| **Preset** | A named parameter set for a declarative program, such as the seat-policy preset `Conservative`. |
| **Transformation** | A declared recipe that changes quantities, optionally priced by a CostFunction and limited by capacity. |
| **Facility** | The player-facing name for a costed Transformation, such as defense procurement. Its accounting is that of a Transformation, not a Market. |
| **Payee** | The Entity that receives Money paid into a facility. |
| **Game seed** | The single seed per game instance, fixed at initialization, from which every in-world random value is derived. |
| **Player seed** | A second seed, used only by the simulation harness, from which all seat-Policy randomness is derived. |
| **Effect** | A declared impure operation (randomness, seat input, interpretation, narration, persistence, time); interpreted by a handler. |
| **Draw key** | The semantic name of one random draw (round, source, Entity, category, index). |
| **Proposal** | A message carrying one or more Intents and a set of signers. It executes only when its signature rule is met: every signer for a deal, or enough weight for a procedure Proposal ([§3.3](#33-proposals-and-signatures)). |
| **Signer** | An Entity whose signature a Proposal asks for. The author's signature is implicit in publishing; an offer also requires its counterparties, and a procedure Proposal names the procedure's members. |
| **Lapse** | The outcome of a Proposal whose signature rule is not met by the end of the Sign step. It is a decline, not an error. |
| **Seat** | An Actor whose Policy is supplied from outside the scenario; swapping the implementation never changes the Actor. |
| **Policy kind** | How a Policy is implemented: declarative program, AI agent, or human ([§4](#4-policies)). Recorded in the manifest and round records, not on Proposals. |
| **Escrow** | The hold placed at screening on a Proposal's maximum cost, in any Resource, released at commit if unused. |
| **Reservation** | A reserved quantity of a reservable Resource, such as deployed Military: unavailable but not consumed. |
| **Fallback** | The declared resolution of an Actor definition that is not made explicit. |
| **Snapshot** | The beginning-of-round World State on which all of that round's decisions are made. |
| **Round record** | The append-only record of one round's inputs, proposed and executed actions, and events. |
| **Manifest** | The immutable record of a game's resolved configuration and initial World State. |
| **Resolution** | A choice of which parts of a Gameboard's world are explicit and which are aggregate. |

## 3. Intents and Economic Actions

An Intent is the common action language of the game engine.

Intents are the only input to the engine. The engine never knows or cares how an Intent was produced ([§4](#4-policies)). **The engine never authors or rewrites Intents; every executed Intent came from a signed Proposal.**

**The engine is an accountant.** It checks authority and signatures and keeps the books balanced. Negotiation, persuasion, and agreement happen out of band among players; the engine only sees what was proposed and who signed.

The Intent algebra contains four categories:

- **Trade:** An order on a public Market, with an optional limit price.
- **Transfer:** A movement of a Resource from one Entity to another. Alone it is a gift or payment; bundled in a multi-signer Proposal it is one side of a deal. Its `from` must be a required signer.
- **Transform:** Execute a declared Transformation, such as production, consumption, procurement, or investment. Its `by` must be a required signer.
- **ChangeRule:** Enact or repeal a Rule under the Entity's authority. A repeal names an existing Rule, and only the authority that governs it (its jurisdiction, or a Withholding's capacity owner) may repeal it.

Intents are carried by Proposals; they are not immediately applied state mutations.

```purescript
type Order =
  { market :: MarketId
  , quantity :: Quantity          -- + buys, − sells the Market's base Resource
  , limitPrice :: Maybe Price
  }

type TransferSpec =
  { from :: EntityId, to :: EntityId, resource :: ResourceId, quantity :: Quantity }

data Intent
  = Trade Order
  | Transfer TransferSpec
  | Transform { by :: EntityId, transformation :: TransformationId, quantity :: Quantity }
  | ChangeRule RuleChange

data Rule
  = Tariff { jurisdiction :: EntityId, origin :: EntityId, rate :: Price }
  | AccessBan { jurisdiction :: EntityId, target :: EntityId }
  | Withholding { capacity :: CapacityRef, units :: Quantity }

data RuleChange
  = Enact Rule                                    -- effective at the next round boundary
  | Repeal RuleId                                 -- likewise

type Proposal =
  { id :: ProposalId
  , author :: EntityId
  , intents :: Array Intent         -- settle atomically, together
  , signers :: Array EntityId       -- the author's signature is implicit
  , procedure :: Maybe CollectiveProcedureId   -- present on a procedure Proposal
  }
```

A Proposal binds Intents to the Entities asked to sign them. Each Intent is executed by the signer whose resources or authority it uses. A Proposal is visible to its signers; Gameboard visibility rules may make more public.

An Intent's economic meaning is independent of who generated it: a seat, NPC, or Aggregate can all propose a Trade, and the engine determines whether it is permitted, feasible, and executable. Provenance (which seat or Policy produced a Proposal, and its Policy kind) is kept in the round record and manifest.

### 3.1 Intent validation

Validation has two stages.

**Authorization validation** checks whether the Entity exists, whether its controller or policy may submit the Intent, whether the requested action falls within its capabilities, and whether any required Representation is valid.

**Economic feasibility validation** checks available (unreserved) resources, market capacity, transformation capacity, and other constraints. Joint feasibility is evaluated across the entire round, since multiple individually valid actions may collectively exceed available resources.

The engine distinguishes a **valid Proposal** from its **executed outcome**. A Proposal may pass authorization and contribute an observation to a Representation, yet a Trade in it can still be partially filled or rejected at settlement. Both the proposal and the executed outcome are recorded.

### 3.2 Infeasible actions

Infeasible actions are rejected or partially executed according to deterministic rules. The engine must not silently invent resources or authority to make an action succeed.

The default rules are:

- When an Entity's Proposals jointly exceed its beginning-of-round available resources, measured by their escrowed maximum cost ([§8.2](#82-single-round-update-and-snapshot-discipline)), they are scaled pro rata, unless the controller supplied an explicit priority ordering.
- When demand for a scarce quantity exceeds supply or capacity, fills are allocated pro rata by requested quantity.
- Residual indivisible units are allocated in canonical EntityId order, never by submission order.
- Orders whose limit price cannot be satisfied are rejected.
- Settlement of each Entity's accepted actions is atomic.

Scenarios may declare alternative allocation rules, provided they are deterministic and order-independent.

### 3.3 Proposals and signatures

Every Intent reaches the engine inside a Proposal. Phase 3 of each round, run by the round runner in the Policy layer, has two steps against the same snapshot ([§8.1](#81-canonical-resolution-sequence)):

- **Propose.** Every Policy publishes Proposals. The Proposal set is frozen at the end of this step.
- **Sign.** Every Policy sees the snapshot and the Proposals that name it as a signer, and signs those it endorses. Publishing implies the author's signature.

A **unilateral action** (a Market Trade, a gift Transfer, a Transform, a ChangeRule) has its author as the only required signer. An **offer** also requires its counterparties: SA's Proposal [Transfer 20 E to JP, Transfer 48 M to SA] with signers [SA, JP] is an offer, and JP's signature is the acceptance. An N-party deal requires N signers. Multi-signer Proposals may bundle only Transfers and Transforms; Trades always travel in single-author Proposals and follow the partial-fill rules ([§3.2](#32-infeasible-actions)). A multi-signer Proposal is all-or-nothing, so any facility order inside it must be fillable in full: if the signed orders for a facility exceed its capacity, every multi-signer Proposal using that facility in that round is rejected as a unit and recorded. Single-author facility orders keep pro-rata fills. Each `ProposalId` is derived from (round, author, index in the author's submission), so recorded signatures replay deterministically.

A **procedure Proposal** is authored by an Aggregate under one of its collective procedures and names that procedure's members as signers. It proceeds when the signed weight exceeds the procedure's threshold ([§2.2](#22-three-projections-of-an-entity)), and is then the Aggregate's own action: it may carry any Intent the procedure authorizes, including a Trade, which fills under the normal partial-fill rules. The bundling and all-or-nothing restrictions above apply to deals, not to procedure Proposals.

Only Proposals whose signature rule is met proceed. A deal missing a signature, or a procedure Proposal short of its threshold, **lapses**: it is recorded as a decline and the round resolves normally. Because acceptance is a signature on the offer's own terms, the two sides of a deal can never disagree.

A fully signed Proposal is escrowed at screening like any action ([§8.2](#82-single-round-update-and-snapshot-discipline)); each signer funds its own giving side from opening holdings. Its Intents settle atomically, or the whole Proposal is rejected and recorded, for example when escrow, transferability, or delivery fails.

Negotiation happens out of band for humans; the interpreter turns "we accept SA's deal" into a signature on that Proposal. Programs propose by rule and sign by rule (for example, "sign any Energy offer at ≤ 2.6 M/E"); AI agents read the Proposals that name them and choose. A counteroffer is a new Proposal, signable next round. The engine enforces no promises across rounds.

The Propose/Sign structure is the extension point for further coordination: more Propose/Sign iterations within a round or information-only Proposals can be added without changing settlement.

## 4. Policies

A Policy is anything that produces Intents from the authorized view of the Entity it drives:

**Policy + Authorized View + Memory + Turn → Intents**

```purescript
type View =                              -- what a Policy sees (INV-16)
  { round :: RoundIndex
  , self :: Entity
  , prices :: Map MarketId Price         -- beginning-of-round
  , rules :: Array Rule                  -- Rules in force
  , events :: Array Event                -- public events and its own, from prior rounds
  , factors :: Map BehaviorDomain Ratio  -- Representation adjustment factors stored last round, for an Aggregate's Policy
  , toSign :: Array Proposal             -- Proposals requiring its signature (empty in Propose)
  }
```

The engine (World State + Intents → World State) is separate from the Policies that produce Intents. Policies come in three kinds:

| Kind | Implementation | Reproduced from |
|---|---|---|
| **Declarative program** | NPC rules, Aggregate baselines, seat-policy presets, run on the shared interpreter ([§4.4](#44-shared-decision-language-interpreter)) | The game seed |
| **AI agent** | A language model given the role's briefing, objectives, and authorized view | Recorded Intents |
| **Human** | A person whose typed text the AI interpreter converts into Intents | Recorded Intents |

Accepted canonical Intents are always recorded, and replay starts from them; deterministic programs can also be regenerated from the seed.

An NPC's Policy is part of the scenario. A seat's Policy is supplied from outside and bound at run time; swapping it (human, agent, or program) never changes the Actor's definition. Memory of in-world Policies lives in World State; memory of seat Policies lives in the session or run record, because seat Policies are not part of the game world.

### 4.1 NPC policies

An NPC is an explicit Actor whose Policy is part of the scenario. Its decisions have no special authority and undergo ordinary validation. For example, an NPC government may respond to tariffs with retaliatory restrictions, or propose and sign offers with other Actors.

### 4.2 Aggregate policies

Every Aggregate has a baseline Policy governing the behavior of its implicit population.

An Aggregate Policy might determine consumption, investment, production, borrowing, or other economic activity based on available information. It declares which behavioral domains it controls, so Representations can adjust the correct outputs. It also decides when to publish a procedure Proposal under one of the Aggregate's collective procedures ([§2.2](#22-three-projections-of-an-entity)).

Its generated actions provide the baseline behavior of the population. Each round it reads the adjustment factors stored in the previous round from its View and scales the matching baseline by the stored factor ([§7.4](#74-estimation-semantics)). With nothing stored, it follows its baseline. Aggregates sign only their own Proposals unless the Gameboard explicitly allows them to sign multi-signer Proposals.

### 4.3 Policy semantics

The rules below apply to in-world Policies (NPC and Aggregate). Their definitions are immutable and versioned for the duration of a game, including their conditional and time-dependent behavior.

Their outputs vary as the world evolves: prices, shortages, institutional rules, relationships, and observations are all mutable World State that Policies react to. Their decision rules do not change because a language model interprets an event differently.

An initialized Policy must:

- Operate only on an authorized view of the world.
- Produce typed Intents.
- Store any persistent memory in World State.
- Respect deterministic execution, using seeded randomness only where its definition explicitly calls for it.
- Remain distinct from institutional rules that players may be authorized to change.

A seat enacting a tariff changes a Rule through `ChangeRule`. It does not rewrite the behavioral Policy of any affected Entity.

The consequences of that tariff emerge when Markets and Policies respond to the new conditions.

### 4.4 Shared decision-language interpreter

All declarative programs (NPC Policies, Aggregate Policies, and seat-policy presets) run on one shared **decision-language interpreter**. A program is a list of rules, each a set of conditions (comparisons over prices, own holdings, stored factors, the round, and parameters; events of the previous round; rules in force) and the templates it proposes (market orders, facility orders, deals that bundle transfers and transforms, procedure Proposals, tariffs, access bans, withholdings, repeals of Rules the Entity governs, reciprocal restrictions, and a seeded choice among options), plus sign rules (procedures, facilities, or everything addressed to it, each optionally guarded by conditions). The market `$home` names the Entity's home market: the first Market, by id, that the Entity has access to. An Entity that acts (a seat, or one whose Policy runs) must have one; a scenario that gives it none fails at startup, naming the Entity. The interpreter drafts only the ordinary intents its Entity is authorized to execute, using the same `authorized` function the engine applies ([§2.4](#24-capability)). Procedure and deal intents are exempt, because the engine checks them against their own authority. The language grows only when a documented Policy needs more. They differ only in where they are configured and which Entity they drive. A seat preset and an NPC Policy expressing the same rule therefore behave identically.

## 5. Cost Functions, Markets, and Transformations

### 5.1 CostFunction

A CostFunction is a reusable pricing primitive.

Let C(q) be a cumulative cost function, where q is a cumulative quantity position.

The marginal price is:

**p(q) = dC(q) / dq**

The cost of moving the position by Δq is:

**Cost(q, Δq) = C(q + Δq) − C(q)**

```purescript
type CostFunction =
  { base :: Price, slope :: Price   -- p(q) = base + slope·q
  , domain :: Interval              -- supported position range
  }
```

Cost functions are linear. A Market's position persists across rounds; a facility's position is its total for the current round.

A CostFunction prices quantity. It never creates or destroys an asset by itself.

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

The base Resource is the good being exchanged. The quote Resource is the denomination used to price it.

The same cost function prices purchases and sales using positive and negative quantities, subject to inventory and reserve constraints.

Markets cannot supply resources they do not hold. A regional Market is expressed through `access`.

### 5.3 Facilities (costed Transformations)

A facility is a capacity-constrained Transformation whose marginal cost is priced by a CostFunction.

```purescript
type Transformation =
  { id :: TransformationId
  , inputs :: Holdings            -- per unit
  , outputs :: Holdings           -- per unit
  , cost :: Maybe CostFunction    -- money price of congestion or increasing marginal cost
  , capacity :: Maybe CapacityRef -- limiting capacity, shared by facilities naming it; Nothing: uncapped
  , payee :: Maybe EntityId       -- recipient of money paid
  , delay :: RoundOffset
  , minimum :: Quantity           -- smallest order accepted, e.g. a major military action
  , maximum :: Maybe Quantity     -- largest order accepted per Entity per round
  , capacityUse :: Quantity       -- capacity drawn per unit ordered
  , ownerSigns :: Boolean         -- the capacity's owner must sign every order
  , effects :: Array CapacityEffect -- capacity changes applied after the delay
  }
```

```purescript
type CapacityEffect =   -- per unit ordered; holder defaults to the ordering Entity
  { capacity :: CapacityRef, holder :: Maybe EntityId, perUnit :: Quantity, rounds :: Maybe Int }
```

Investment and disruption are facilities with capacity effects: an investment raises a capacity from the round after its delay, and a strike lowers another Entity's capacity for a limited number of rounds. Facilities that share a capacity draw on it by their `capacityUse` and share its limit; those that also share a cost function share one uniform price. That price is a single function of the pool's filled quantity (the average of the CostFunction over the fills). Escrow applies the same function to the pool's ceiling. For a capped facility the ceiling is the capacity ÷ the smallest `capacityUse` among the facilities drawing on that capacity, rounded down, since any total of fills draws on the capacity at least that fast; every member escrows at this one ceiling, whatever its own use. For an uncapped facility (no capacity, or `capacityUse` zero) the ceiling is the top of its CostFunction's domain. Fills never exceed that ceiling, so the escrowed price bounds the settled price. When `ownerSigns` is set, the capacity's owner signs each order, which is how a transport owner chooses which shipments to carry.

Production, consumption, procurement, issuance, destruction, and investment are Transformations. They modify holdings only when the required inputs, capacities, and authority are available.

Money paid to a facility goes to a declared payee Entity, such as the producer, its workforce, or its suppliers, so that expenditure re-enters circulation. Facilities hold no reserves; they pay a payee.

A facility may be presented as a "market" in the player interface, but its accounting is that of a Transformation: outputs are created only by the recipe, within capacity.

Resource transfers preserve quantities. Production and consumption account for changes according to declared recipes. Financial claims obey their associated issuance and liability rules.

### 5.4 Market scope and geography

Each commodity keeps a single Resource type. Geography is expressed through:

- Scoped or regional Market nodes.
- Market access rules that determine which Entities may trade.
- `Tariff` and `AccessBan` Rules applied to particular jurisdictions and origins ([§3](#3-intents-and-economic-actions)).
- Transport between nodes as a facility with limited capacity ([§5.3](#53-facilities-costed-transformations)). Two Entities are in the same region when some Market for the Resource lets both trade. A cross-region Transfer must be bundled with a matching order on the Resource's transport facility, executed by the sender and signed by the owner of that facility's capacity. An `AccessBan` makes the shipment infeasible; a `Tariff` adds its rate to the shipment, paid by the sender to the restricting jurisdiction.
- Facilities that share a capacity share its limit and its uniform price; for example, two procurement facilities with different payees can draw on one production capacity.

Trade restrictions, tariffs, and sanctions therefore have identifiable mechanical effects: they change access, prices, or throughput for specific participants.

### 5.5 Simultaneous clearing

All orders in the round update are resolved as one batch, and each Market clears at a single uniform price. The steps below define how that price is computed. They do not order individual orders.

For a given Market:

1. Orders whose access rules fail are rejected.
2. Tariffs and taxes are applied to effective prices.
3. Compatible purchases and sales are netted.
4. Residual net demand is executed against the cost function, within domain and reserve limits.
5. A uniform batch price is computed by the Market's declared rule. The default is the average marginal cost over the net quantity, or the current marginal price when net demand is zero.
6. Orders whose limit price is violated at the computed price are removed, and the price is recomputed. Removed orders stay removed, so the procedure terminates and depends only on the set of orders. Partial fills follow the rules in [§3.2](#32-infeasible-actions).
7. Holdings and reserves change as part of the round's single net update.

The clearing result must not depend on the order in which submissions were received.

### 5.6 Market-rule changes

Institutional actions modify market conditions by enacting or repealing Rules: a `Tariff` adds its rate to deliveries from the origin into the jurisdiction, paid to the jurisdiction's government; an `AccessBan` makes such deliveries infeasible; a `Withholding` removes units of a capacity.

A rule change always takes effect at the next round boundary.

## 6. Economic Circulation

The economy must sustain repeated rounds. A world in which resources can only be spent, and never earned or produced, would make shortages indistinguishable from incomplete modeling.

Each round's single update includes a recurring stock-flow cycle. Its four components are computed together from the snapshot, not in sequence ([§8.2](#82-single-round-update-and-snapshot-discipline)):

- **Production.** Authorized Transformations create outputs, such as Energy, from capacity and inputs.
- **Exchange.** Markets clear, and Money moves to suppliers and producers.
- **Income and expenditure.** Explicit rules distribute earnings, wages, taxes, public spending, and transfers among Entities.
- **Capacity renewal.** Investments, depreciation, and degradation adjust future capacities.

This requires no additional Resources. It requires production rules, accounting-consistent financial flows, and productive-capacity state, held in World State as data and applied by one function:

```purescript
type Circulation =
  { production :: Array { entity :: EntityId, resource :: ResourceId, capacity :: CapacityRef } -- delivered next round
  , flows :: Array { from :: EntityId, to :: EntityId, resource :: ResourceId, amount :: Quantity } -- fixed income and expenditure
  , shares :: Array { from :: EntityId, to :: EntityId, share :: Ratio, source :: ShareSource } -- variable flows
  , requirements :: Array Requirement }                                    -- consumed at commit

data ShareSource = SalesOf ResourceId | PayeeReceipts   -- net sale revenue, or Money received as a facility payee

type Requirement =
  { entity :: EntityId, resource :: ResourceId
  , amount :: Maybe Quantity    -- Nothing: equal to this round's purchases of the Resource
  , penalty :: Quantity }       -- each unit unmet reduces the Entity's fixed incoming flows by this much
```

Every Resource stock reconciles each round:

**Closing stock = Opening stock + Production + Issuance − Consumption − Destruction − Decay**

For each Resource, transfers and trades sum to zero across all accounts, including Market reserves.

Every unit of Money paid out has a recorded recipient.

## 7. Aggregates and Representation

Explicit Actors influence an implicit Aggregate in two distinct ways. **Representation** treats an Actor's behavior as a sample of the Aggregate's population and adjusts its baseline. A **collective procedure** lets members' signatures decide whether the Aggregate's own Proposal executes ([§2.2](#22-three-projections-of-an-entity)). Neither gives the Actor the Aggregate's resources or authority: a small-business owner can move the same economy as a government, and a member state can influence a bloc without owning it.

### 7.1 Aggregate semantics

An Aggregate maintains:

- A `members` list.
- Residual economic accounts and capacities.
- A baseline behavioral Policy.
- Any collective procedures and institutional capabilities.
- Relationships to any representative Actors.

**An Aggregate's economic accounts are the residual after explicitly modeled accounts have been assigned to other Entities.**

An explicit small-business Actor owns its own resources. The small-business Aggregate holds the resources attributed to the remaining, implicitly represented businesses.

Making the head of a government explicit extracts only the accounts and responsibilities assigned to that government Actor. The households and firms of that country remain in whichever Aggregate owns them.

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

The domain constrains which part of the Actor's behavior may influence the Aggregate: a small-business owner's purchasing may inform small-business demand, but does not authorize taxation or determine the Aggregate's political behavior. Representation never transfers holdings or capabilities.

### 7.3 Representation pipeline

Representation proceeds through four stages, and the engine records each one:

1. **Observation.** Derived from a validated, escrowed Proposal, before clearing. Escrow means every observation is backed by real resources. A Representation's behavior domain names the Resource whose purchases are observed: the observation is the actor's total proposed buy quantity of that Resource on Markets.
2. **Estimate.** The adjustment factor, computed and stored by the engine.
3. **Proposed Aggregate Intent.** The Aggregate Policy's adjusted Proposal.
4. **Executed result.** The settled outcome after feasibility checks.

The record spans two rounds: the observation and estimate belong to round r, and the proposed Intent and executed result to round r + 1.

An observation is taken from the representative's admissible intended action, not its executed fill. Any difference between intended and executed actions is recorded.

### 7.4 Estimation semantics

Representation is an inference operation, never a resource multiplier.

Each representative's deviation from its reference is perturbed, then clamped:

**Deviationᵢ = clamp(deviationBound, (observedᵢ − referenceᵢ) / referenceᵢ + εᵢ)**

**εᵢ = noiseSpreadᵢ × (2uᵢ − 1)**, where uᵢ is the seeded draw for this input ([§9.1](#91-numerics-and-randomness)).

Sampling noise models the uncertainty of inferring a population from a few representatives, so one decision moves the Aggregate by a slightly different amount in each game. The noise is added before clamping, so the deviation bound still caps the total effect. Noise applies only to actual observations.

**Adjustment factor = 1 + shrinkage × Σ(weightᵢ × deviationᵢ)**

The sum runs over the Representations targeting that Aggregate and domain. The engine computes the factor from round r's Proposals and stores it in World State; it never creates or changes a Proposal. In round r + 1 the Aggregate's Policy proposes:

**Adjusted target = Baseline × stored factor**

where the baseline is that of round r + 1, replaced exactly once.

Representatives are strategic players, not random samples, and have an incentive to act for influence. The type system does not try to eliminate this, because disproportionate influence for small players is a design goal. It makes influence explicit and bounded. Estimator parameters are declared balancing assumptions, not claims of unbiased inference.

When no observation was made in the previous round, no factor is stored and the Aggregate follows its baseline.

### 7.5 Multiple representatives

Multiple Actors may represent the same Aggregate.

Their deviations are combined by each Representation's `weight`, which accounts for overlapping and possibly biased samples. Representatives are not automatically treated as independent or equally informative.

### 7.6 Avoiding double-counting

Two rules are essential.

**Economic accounting:** An explicit Actor's accounts are excluded from the target Aggregate's accounts. Its direct economic transactions are executed once.

**Behavioral accounting:** In the following round, the Aggregate's adjusted behavior replaces the corresponding baseline behavior. It is never added to an already complete baseline action.

A representative decision may therefore have two distinct effects:

1. A direct effect on the representative Actor's own holdings.
2. An estimated effect on the Aggregate.

Both effects are independently validated, and neither is counted twice.

## 8. World State and Round Resolution

World State is the complete economic snapshot at a round boundary.

It includes Entities, their accounts and capacities, Markets, facilities, the Rules in force, rule changes scheduled for the next boundary, pending deliveries, stored estimates, Policy memory, the event history, and the current point in simulated time. It also names the numeraire (the Resource facility costs are paid in), each Resource's transport facility, and each Entity's jurisdiction where it is not the Entity itself.

Policies read World State but cannot mutate it; they only submit Intents.

The engine advances the world through a deterministic round-resolution function.

### 8.1 Canonical resolution sequence

This is the single authoritative resolution sequence. The Gameboard, Instantiated Game, and Simulation Harness specifications reference it and do not redefine it.

**1. Prepare round**

Apply delayed production deliveries, scheduled shocks, and rule changes enacted in the previous round.

**2. Create observations**

Produce the beginning-of-round snapshot and each Policy's View of it ([§4](#4-policies)).

**3. Generate Proposals** *(round runner, Policy layer)*

- **3a Propose.** Every Policy publishes Proposals against the snapshot at the same time: seats through whatever Policy drives them (human via the AI interpreter, AI agent, or program), NPC Policies, and Aggregate Policies, including any procedure Proposals. The set is frozen at the end of 3a.
- **3b Sign.** Every signer signs the Proposals naming it that it endorses ([§3.3](#33-proposals-and-signatures)).

Phases 1–2 and 4–7 are engine phases.

**4. Screen and interpret**

Check authority, entity identity, and each Proposal's signature rule: every signer for a deal, support exceeding the threshold for a procedure Proposal. Proposals that fail it lapse and are recorded. Extract admissible observations from validated Proposals.

**5. Estimate**

Compute each Representation's adjustment factor ([§7.4](#74-estimation-semantics)) from this round's validated Proposals, and store it in World State. The engine creates and modifies no Proposal; Aggregate Policies apply the stored factors next round.

**6. Resolve economy**

Resolve every accepted action of the round as one simultaneous batch against the prepared snapshot ([§8.2](#82-single-round-update-and-snapshot-discipline)). Markets clear at uniform prices. Transformations, signed deals, passed procedure Proposals, and the economic cycle (production, exchange, income and expenditure, capacity renewal) are evaluated together, and the result is a single net change to World State.

**7. Commit and record**

Atomically commit the new World State. Record intended and executed actions, events, diagnostics, and balance-sheet data for the reporting layer.

### 8.2 Single round update and snapshot discipline

All Actor and Aggregate actions for a round are bundled into a single round update and resolved together. Clearing is uniform, so there is no sequencing of actions within a round: no action settles "before" another, and submission order is irrelevant ([INV-11](#inv-11)). The phases in [§8.1](#81-canonical-resolution-sequence) are functional dependencies between stages, such as screening before estimation. They never order individual actions.

Two rules make this hold:

- **Funding from the snapshot.** Each Entity's bundle is funded from its available holdings at the beginning of the round. At screening, each signed Proposal escrows its maximum cost: for a buy, limit price × quantity (or the Market's domain upper-bound price when there is no limit); for a sell, its quantity; for a facility order, its price at its pool ceiling (the capacity ÷ the smallest `capacityUse` in its pool, or the top of the cost domain for an uncapped facility, [§5.3](#53-facilities-costed-transformations)). Joint feasibility and pro-rata scaling ([§3.2](#32-infeasible-actions)) are checked against those escrows, and unused escrow is released at commit.
- **Credit at commit.** Proceeds, production, income, and receipts from signed Proposals this round are credited when the round commits and become usable next round. An Entity cannot sell Energy and spend the proceeds in the same round. Selling and buying in the same round both draw on opening holdings.

Every payment out of an Entity is capped at what that Entity holds after the batch so far: facility payments, Transfers, tariffs, and circulation flows alike. Any remainder is recorded as a Shortfall rather than driving a holding negative. Escrow makes the cap bind only in error, and it is the engine's backstop, not a second pricing rule. Market fills are not capped: clearing bounds them and escrow funds them.

Passive consumption rules (such as operating requirements), decay, and derived flows (such as revenue sharing) are functions of the batch result. They are evaluated on each Entity's net position at commit, not as later steps.

All decisions in a round operate on the beginning-of-round snapshot. Everything a round computes (prices and estimates) becomes observable in the next round, so there is no same-round feedback.

### 8.3 Functional interface

The fundamental engine operation is a pure state transition.

```purescript
type SignedProposal = { proposal :: Proposal, signatures :: Array EntityId }

-- Engine: signed Proposals are its only input
resolveRound :: WorldState -> Array SignedProposal
             -> Either EngineError RoundResult

-- Round runner (`Effects.Round`): effectful, written against effect signatures (§8.4)
runRound :: Policies -> WorldState
         -> Run (SEAT_INPUT + RECORD + r) (Either EngineError RoundResult)
runRound policies world0 = do
  let world = prepareRound world0                           -- phase 1 (engine, pure)
      views = observe world                                 -- phase 2 (engine, pure)
  proposals <- propose policies views                       -- 3a: seats, NPCs, Aggregates
  signed    <- sign policies views proposals                -- 3b
  let result = resolveRound world signed                    -- phases 4–7 (engine, pure)
  traverse_ record result
  pure result
```

The round runner evaluates every Policy, seat or in-world, through the same code in live play and simulation; in-world declarative programs run on the shared interpreter. `resolveRound` screens authority and signature rules, records lapsed Proposals, stores estimates, runs the economic cycle and clearing, and commits the new World State with accepted and rejected actions, intended-versus-executed records, and events.

`EngineError` represents a failure of the configured simulation or its execution. Lapsed Proposals and ordinary infeasible actions are reported as outcomes and never fail the round.

### 8.4 Effects and handlers

The whole system is pure. The engine has no effects at all: `resolveRound` is a pure function of World State and signed Proposals. In-world randomness, such as estimator sampling noise, uses the pure keyed `draw` function ([§9.1](#91-numerics-and-randomness)) with the game seed held in World State, so it needs no effect.

Everything impure is an algebraic effect, and only the round runner and the code around it (sessions, the harness, Dashboard reads) use effects. The signatures are declared in `Effects.Signatures`, with no implementation; the round runner is `Effects.Round`, which is written against them and never concrete IO:

| Effect | Operation |
|---|---|
| `SeatInput` | A seat's Proposals in Propose and its signatures in Sign |
| `Interpret` | Text → candidate Intents, with confirmation |
| `Narrate` | Resolved events → front page and balance-sheet text |
| `Record` | Append round records and snapshots |
| `Clock` | Session deadlines (live play only) |

Handlers choose the context; the round runner is the same code under all of them:

- **Live session:** `SeatInput` via humans and `Interpret`; `Clock` enforces deadlines.
- **Simulation:** `SeatInput` via preset programs or AI agents, whose randomness draws from the player seed.
- **Replay:** `SeatInput` feeds recorded Proposals and signatures.
- **Test:** all handlers pure.

Determinism follows from purity: identical handler inputs (seeds and recorded seat input) give an identical run, and replay is simply running under the replay handler.

## 9. Architectural Invariants and Numerics

This is the single numbered invariant table for the whole system. The Gameboard, Instantiated Game, and Simulation Harness specifications cite these IDs and keep no tables of their own. Each simulator required assertion maps to one ID. Layer tags: **E** engine, **G** Gameboard, **I** instantiation, **H** harness.

| ID | Layer | Invariant | Requirement |
|---|---|---|---|
| <a id="inv-01"></a>INV-01 | E | Unified action semantics | All participants act through the same Intent and validation machinery. The engine never authors or rewrites Intents: every executed Intent came from a signed Proposal. |
| <a id="inv-02"></a>INV-02 | E | Authority separation | An Entity exercises only capabilities it has been granted, within scope. |
| <a id="inv-03"></a>INV-03 | E, G, I | No authority amplification | Membership, Representation, signing a procedure Proposal, and fallbacks never confer or remove an institution's capabilities or holdings. |
| <a id="inv-04"></a>INV-04 | E | Resource consistency | Holdings change only by authorized exchange, transformation, issuance, destruction, or decay, per the Resource's declared properties and Transformations; no prohibited negative holdings. |
| <a id="inv-05"></a>INV-05 | E, G, I | Stock-flow reconciliation | Each round every stock reconciles ([§6](#6-economic-circulation)), every Money payment has a recorded recipient, and nothing accumulates in an account without a declared outflow rule. |
| <a id="inv-06"></a>INV-06 | E, G | Market solvency | Markets and Transformations respect reserves, capacities, and declared limits. |
| <a id="inv-07"></a>INV-07 | E, G | Price is not production | A CostFunction never creates an asset without an authorizing Transformation. |
| <a id="inv-08"></a>INV-08 | E, G, I | Account partition | Every account and behavioral responsibility has exactly one owner, however memberships overlap. |
| <a id="inv-09"></a>INV-09 | E | Representation consistency | A stored estimate replaces the matching baseline exactly once, in the following round, within estimator bounds, and never duplicates direct activity. |
| <a id="inv-10"></a>INV-10 | E | Signature completeness | A Proposal executes only if its signature rule is met: every required signer for a deal, or support exceeding the threshold for a procedure Proposal. A deal's Intents settle atomically or are rejected together. |
| <a id="inv-11"></a>INV-11 | E | Order independence | All actions resolve as one bundled round update with uniform clearing; permuting submissions never changes settlement, and no action depends on another settling first. |
| <a id="inv-12"></a>INV-12 | E, G | Policy reproducibility | Policy output is determined by its versioned definition, permitted view, World State memory, time, and declared randomness. |
| <a id="inv-13"></a>INV-13 | E, H | Deterministic replay | The same manifest, seeds (game, and player where applicable), and recorded seat input reproduce identical World States. Every random value is a keyed draw from a named stream ([§9.1](#91-numerics-and-randomness)). |
| <a id="inv-14"></a>INV-14 | E, G, I, H | Resolution consistency | Instantiating a world at different levels of explicitness reconciles accounts, liabilities, and authority before any decisions differ. |
| <a id="inv-15"></a>INV-15 | E, I | Single authoritative state | All actions modify one shared World State and timeline. |
| <a id="inv-16"></a>INV-16 | E, I | Information isolation | Every Policy, in-world or seat, observes only what the Gameboard permits. |
| <a id="inv-17"></a>INV-17 | G | Type-system conformity | All scenario definitions use supported types and operations. |
| <a id="inv-19"></a>INV-19 | G | Role consequence | Every Actor definition declares a capability, a dependency, and a measurable objective. |
| <a id="inv-21"></a>INV-21 | G | Instantiation feasibility | The Gameboard admits at least one valid initial world. |
| <a id="inv-22"></a>INV-22 | G | Version stability | A Gameboard version's definitions are immutable. |
| <a id="inv-23"></a>INV-23 | I | Unique control | Every explicit Actor has exactly one valid controller. |
| <a id="inv-24"></a>INV-24 | I | Valid participation | Every selected Actor and Representation is permitted by the Gameboard and validly parameterized. |
| <a id="inv-25"></a>INV-25 | I | Fallback completeness | Every unselected Actor definition has a permitted fallback; no required responsibility is left unowned. |
| <a id="inv-26"></a>INV-26 | I | Policy completeness | Every NPC and Aggregate has an executable initialized Policy. |
| <a id="inv-27"></a>INV-27 | I | Market feasibility | Initial Market positions, reserves, and capacities satisfy economic constraints. |
| <a id="inv-28"></a>INV-28 | I | Temporal consistency | Round intervals and Policy timing fit the simulated horizon. |
| <a id="inv-29"></a>INV-29 | H | Engine and interpreter reuse | Simulated games use the same engine, resolution sequence, and decision-language interpreter as real games. |
| <a id="inv-30"></a>INV-30 | H | Controller integrity | Swapping a seat's Policy implementation never changes the Actor's definition and never turns a seat into an NPC. |
| <a id="inv-31"></a>INV-31 | H | Authoritative records | Manifests, append-only round records, and snapshots are authoritative and immutable; metrics are versioned projections. |
| <a id="inv-36"></a>INV-36 | E, H | Purity | The engine is a pure function; all impurity (randomness, input, interpretation, narration, persistence, time, output) occurs only through declared effects interpreted by handlers ([§8.4](#84-effects-and-handlers)). |

IDs 18, 20, and 32–35 are retired.

### 9.1 Numerics and randomness

Mechanics are deterministic by default. Randomness is introduced only where a Gameboard rule or estimator explicitly calls for it, such as estimator sampling noise.

Randomness is drawn once per run, before initialization, at the program boundary. A run has one seed per stream, and each is either injected explicitly or drawn once from the operating system when absent. The drawn value is the concrete seed the run uses, and it is recorded in the manifest or spec. No other code reads entropy, so during a run there is no entropy: **the same seed gives the same run**, and recording the seed makes the run replayable. The simulation core takes concrete seeds only.

Each game instance has exactly one **game seed**, fixed in its configuration at initialization and recorded in the manifest. Every in-world draw (estimator sampling noise, declared Policy randomness, declared shocks) uses it. The simulation harness adds a **player seed** for all seat-Policy randomness; it exists only under the simulation handler, and live play has none. Every draw is made by one pure function, so randomness needs no effect: the round runner passes in-world Policies draws on the game seed, and the seat handler passes seat Policies draws on the player seed. The two streams are the two seeds:

```purescript
draw   :: Seed -> DrawKey -> UnitInterval     -- pure, in core

data Stream = Game | Player                   -- Game: in-world draws; Player: seat draws (simulation only)
```

The streams never share draws, so varying the player seed never changes an in-world draw, and vice versa.

A `DrawKey` names the draw's semantic purpose: round, Representation or Policy, input Entity, draw category, and index (for example `round-3 / sme-energy-sample / SME / sampling / 0`). Because draws are keyed rather than consumed from a sequence, adding an Entity or changing evaluation order never changes unrelated draws. The PRNG algorithm and the key serialization are versioned in the manifest. Each draw is converted to a quantity under the fixed-point rule below.

All quantities, prices, and settlement amounts use fixed-point arithmetic with **4 decimal places**, rounded **half-to-even** at each settlement step. Uniform batch prices are rounded before they are multiplied by quantities. No floating-point arithmetic enters economic state. Other specifications reference this rule and do not restate it.

The numeric rules below are implemented once, in `Core.Num` (`Q`). Every boundary imports that module; no other code parses, rounds, or formats an economic value.

- **Representation.** A `Q` is an arbitrary-precision integer count of 10⁻⁴ units (`BigInt`). Addition, subtraction and negation are exact.
- **Rounding.** A product or quotient that leaves the 4-decimal grid is rounded half-to-even (ties to the even last digit, symmetric in sign). Uniform batch prices are rounded once, before they multiply quantities. Rounding at the grid is the only rounding: nothing is truncated silently.
- **Remainders.** A total split among shares uses `proRata` (or `proRataWhole`, which first floors the total to whole units): largest remainder, ties to the earlier index in canonical order. The shares sum to the total exactly, so nothing is created or lost. A quotient used as a *scale* (a fraction of a claim that may be funded) uses `divFloor`, rounded toward zero, so that the scaled amount never exceeds its numerator; a half-even quotient can round up and overdraw.
- **Overflow and underflow.** None. Arbitrary precision means no quantity saturates or wraps. A negative holding is an invariant violation (INV-04), not a numeric event.
- **Division by zero.** Explicit. `div` fails loudly; a caller whose divisor can be zero uses `divMaybe` or `divFloor` and handles `Nothing`.
- **Parse.** `Q.parse` is the only decimal parser: an optional sign, one or more digits, and optionally a point followed by one to four digits. Anything else, including a fifth decimal, is an error; no input is rounded silently. JSON decoding, the DSL's quantity literals, and any dashboard value read back all use it.
- **Format.** `Q.format` is the only formatter: an optional `-`, the integer part, and exactly four decimals, with no grouping and no trailing-zero trimming. `show`, JSON encoding, the CLI, the harness views and the dashboard all reach it.
- **Floating point.** No `Number` enters economic state. `Q.toDisplayNumber` is the single conversion to a float, for display only (chart axis scaling); its result is never parsed back into a quantity. Integers that are not economic values (rounds, seeds, ports, counts) remain `Int`.

## 10. Design Boundaries

The type system deliberately does not prescribe the following:

**Specific resource classifications.** These belong to the Gameboard, informed by vertical research. The type system supplies only Resource properties.

**A universal macroeconomic model.** Economic behavior is expressed through configured Policies, Markets, Transformations, income rules, and institutional rules.

**A single estimator parameterization or procedure threshold.** Estimators and collective procedures are configured per scenario.

**An LLM-based economic authority.** Language models interpret submissions and narrate outcomes. They do not bypass economic validation or directly mutate World State.

**A fixed degree of market realism.** Cost functions are linear; other curve families or clearing mechanisms may be added later behind the same interfaces.

## 11. Summary

The system has one economic ontology, one shared World State, and one resolution mechanism. Entities own accounts and capabilities, with membership, ownership, and collective decision-making kept separate; Actors are explicit seats or NPCs, Aggregates hold residual accounts and baseline behavior. Intents are the engine's only input and express every economic or institutional action; Policies of any kind produce them, Representations turn sampled decisions into bounded Aggregate adjustments, collective procedures let member signatures decide an institution's own Proposals, and CostFunctions price both Market exchange and facility production in a circulating economy resolved as one bundled round update. A game instance is a particular resolution of a larger economic world, and changing that resolution must preserve the world's economic meaning.
