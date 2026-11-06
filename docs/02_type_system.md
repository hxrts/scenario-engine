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
| **Representation** | A relationship through which an explicit Actor's decisions influence an Aggregate through a configured estimator. |
| **Intent** | A typed economic or institutional action. |

One supporting mathematical primitive is shared across the system:

- **CostFunction:** A cumulative cost curve that prices quantity. It is used by exchange Markets and by facilities. It is not an Entity type and never creates resources by itself.

The following concepts are defined through these primitives rather than requiring independent economic machinery:

- **Actor:** An explicitly represented Entity capable of submitting Intents.
- **Controller:** The Policy that drives an Actor.
- **NPC:** An Actor whose Policy is part of the scenario.
- **Seat:** An Actor whose Policy is supplied from outside the scenario: a human player, an AI agent, or a preset program.
- **Transformation:** A declared economic process (production, consumption, issuance, destruction, investment) that changes quantities according to a recipe, optionally priced by a CostFunction and limited by capacity.
- **Proposal:** A signed message carrying one or more Intents to an audience. It executes only when every required signer has signed it ([§3.3](#33-proposals-and-signatures)).
- **World State:** The current economic state, including Entity accounts, capacities, Markets, institutional rules, reservations, Policy memory, and time.

An Aggregate is a specialization of Entity, not a separate economic ontology.

### 2.1 Entity representation

Entities have two possible forms: Actor and Aggregate.

An Actor is individually represented and is either a seat or an NPC. An Aggregate is represented in aggregate and governed by a baseline Policy, optionally together with a collective decision procedure.

```purescript
data Controller
  = Seat SeatId     -- Policy supplied from outside the scenario; implementation bound at run time
  | NPC PolicyId    -- Policy defined by the scenario

type ActorState =
  { id :: EntityId
  , controller :: Controller
  , accounts :: Holdings
  , capacities :: Capacities
  , capabilities :: Array Capability
  }

type AggregateState =
  { id :: EntityId
  , membership :: MembershipDefinition
  , residual :: Population
  , accounts :: Holdings
  , capacities :: Capacities
  , policy :: PolicyId
  , procedure :: Maybe CollectiveProcedureId
  , capabilities :: Array Capability
  }

data Entity
  = Actor ActorState
  | Aggregate AggregateState
```

Identifiers, Holdings, Capacities, Population, and numerical units are domain types defined elsewhere. The snippets specify architectural interfaces rather than complete executable modules.

This construction enforces the primary representation distinction:

- Actors have a seat or NPC controller.
- Aggregates have a membership definition, a residual population, a baseline Policy, and optionally a collective decision procedure.
- Both possess economic accounts and can generate economic effects.
- Both participate in the same Markets and Transformations.

A country's government, a corporation, or a small-business owner may be an Actor. Households, consumers, an entire industry, or a supranational bloc may be represented by Aggregates.

A government Actor holds government-controlled resources only ([§7.1](#71-aggregate-semantics)).

### 2.2 Three projections of an Entity

Population membership, economic ownership, and collective decision-making are distinct relationships. The type system keeps them separate rather than letting the word "Aggregate" carry all three meanings.

| Projection | Question it answers | Rule |
|---|---|---|
| **Membership** | Which populations or organizations does an Entity belong to? | Memberships may overlap freely. |
| **Account ownership** | Who authoritatively owns each economic account and behavioral responsibility? | Every account and responsibility has exactly one owner. |
| **Collective procedure and capabilities** | How does an institution decide, and what authority does it hold? | Authority belongs to the institutional Entity or a rule mechanism, never to members by virtue of membership. |

```purescript
type Membership =
  { member :: EntityId
  , organization :: EntityId
  , role :: MembershipRole
  }

type CollectiveProcedure =
  { id :: CollectiveProcedureId
  , institution :: EntityId
  , residualInput :: PolicyId
  , rule :: DecisionRule
  , authorizes :: Array Capability
  }
```

For example, Germany is a member of the EU. That membership holds whether or not Germany exercises influence in a given round. The EU Aggregate owns the residual European accounts not assigned to other Entities. The EU's collective procedure combines member inputs with its residual institutional Policy and exercises EU-wide capabilities. Germany influences that procedure without owning EU accounts or exercising EU capabilities.

A manufacturer may simultaneously belong to the small-business population, the manufacturing sector, and a regional economy. Those memberships do not permit three Aggregates to own its assets. Accounting is partitioned; classification is not.

### 2.3 Resource

A Resource is an identifiable economic quantity with a defined unit and declared behavior.

Resource behavior is configured through properties, not hard-coded per resource:

```purescript
data ConservationClass
  = Conserved        -- changes only by transfer, or authorized issuance/destruction
  | ProducibleStock  -- produced and consumed through declared Transformations
  | Capacity         -- acquired, reserved, degraded, and restored
  | Influence        -- produced, spent, and subject to decay

type ResourceDefinition =
  { id :: ResourceId
  , unit :: Unit
  , conservation :: ConservationClass
  , transferability :: Transferability
  , decay :: Maybe DecayRule
  , reservable :: Boolean
  }
```

`Transferability` distinguishes freely transferable, restricted (authority-gated), and non-transferable Resources. A `reservable` Resource can be reserved, for example by a military deployment, without being consumed.

Holdings associate Entities with quantities of Resources. Reserved quantities are tracked separately from available quantities.

Not everything economically meaningful is a Resource. Production capacity, transport capacity, market access, political authority, and supply-chain efficiency are economic state or rules attached to Entities, Markets, and Transformations. A semiconductor producer can hold valuable productive capacity without the scenario introducing a semiconductor Resource.

An Entity's holdings are distinct from its capabilities. A government may possess authority to modify taxation or trade restrictions without owning the resources of the affected private sector.

Likewise, an Actor cannot spend, transfer, or create resources merely because it has political influence over their owners.

### 2.4 Capability

A Capability describes which classes of actions an Entity is authorized to perform and over what scope.

Capabilities can represent economic, financial, regulatory, or political authority.

They are checked during Intent validation.

For example, a government may possess the capability to set a tariff within its jurisdiction, while a small business may possess the capability to invest its own capital.

Institutional capabilities stay with the institutional Entity or rule mechanism that holds them. If an institution is not explicitly played, its capabilities remain with its Aggregate's collective procedure or with a declared rule, and are never transferred automatically to a population Aggregate.

Neither Membership nor Representation ever transfers capabilities to the member or representative Actor.

### 2.5 Glossary

This table is the single source of term definitions. The other specifications use these terms and do not redefine them.

| Term | Definition |
|---|---|
| **Account** | A quantity of a Resource, or a capacity, owned by exactly one Entity. |
| **Residual** | The accounts and behavior of a population remaining in an Aggregate after explicitly modeled accounts are assigned elsewhere. |
| **Membership** | Classification or organizational belonging. It confers neither ownership nor authority. |
| **Collective procedure** | An institution's declared rule for combining member inputs with its residual Policy, exercising the institution's own capabilities. |
| **Procedure input** | A member's stance (`Support`, `Neutral`, `Oppose` = +1, 0, −1) submitted to a collective procedure via `ChangeRule`. It moves no resources. |
| **Estimator** | The configured function that turns representative observations into an Aggregate adjustment. |
| **Preset** | A named parameter set for an estimator or a declarative program, such as `statistical`, `constituent`, or the seat-policy preset `Conservative`. |
| **Transformation** | A declared recipe that changes quantities, optionally priced by a CostFunction and limited by capacity. |
| **Facility** | The player-facing name for a costed Transformation, such as defense procurement. Its accounting is that of a Transformation, not a Market. |
| **Payee** | The Entity that receives Money paid into a facility. |
| **Game seed** | The single seed per game instance, fixed at initialization, from which every in-world random value is derived. |
| **Player seed** | A second seed, used only by the simulation harness, from which all seat-Policy randomness is derived. |
| **Effect** | A declared impure operation (randomness, seat input, interpretation, narration, persistence, time); interpreted by a handler. |
| **Draw key** | The semantic name of one random draw (round, source, Entity, category, index). |
| **Proposal** | A message carrying one or more Intents, with an audience and a set of required signers ([§3.3](#33-proposals-and-signatures)). |
| **Signer** | An Entity whose signature a Proposal requires. The author's signature is implicit in publishing; an offer also requires its counterparties. |
| **Audience** | The Entities that can see a Proposal: public, or named Entities. |
| **Lapse** | The outcome of a Proposal that is not signed by every required signer by the end of the Sign step. It is a decline, not an error. |
| **Seat** | An Actor whose Policy is supplied from outside the scenario; swapping the implementation never changes the Actor. |
| **Policy kind** | How a Policy is implemented: declarative program, AI agent, or human ([§4](#4-policies)). |
| **Escrow** | The hold placed at screening on a Proposal's maximum cost, in any Resource, released at commit if unused. |
| **Reservation** | A reserved quantity of a reservable Resource, such as deployed Military: unavailable but not consumed. |
| **Fallback** | The declared resolution of an Actor definition that is not made explicit. |
| **Snapshot** | The beginning-of-round World State on which all of that round's decisions are made. |
| **Round record** | The append-only record of one round's inputs, proposed and executed actions, and events. |
| **Manifest** | The immutable record of a game's resolved configuration and initial World State. |
| **Resolution** | A choice of which parts of a Gameboard's world are explicit and which are aggregate. |

## 3. Intents and Economic Actions

An Intent is the common action language of the game engine.

Intents are the only input to the engine. The engine never knows or cares how an Intent was produced ([§4](#4-policies)).

The Intent algebra contains four categories:

- **Trade:** An order on a public Market, with an optional limit price and maximum spend.
- **Transfer:** A movement of a Resource from one Entity to another. Alone it is a gift or payment; bundled in a multi-signer Proposal it is one side of a deal. Its `from` must be a required signer.
- **Transform:** Execute a declared Transformation, such as production, consumption, procurement, or investment.
- **ChangeRule:** Exercise authorized institutional or market-rule authority, or submit a procedure input to a collective procedure in which the Entity is a member.

Intents are carried by Proposals; they are not immediately applied state mutations.

```purescript
type Order =
  { market :: MarketId
  , quantity :: Quantity          -- + buys, − sells the Market's base Resource
  , limitPrice :: Maybe Price
  , maxSpend :: Maybe Quantity
  }

type TransferSpec =
  { from :: EntityId, to :: EntityId, resource :: ResourceId, quantity :: Quantity }

data Intent
  = Trade Order
  | Transfer TransferSpec
  | Transform TransformationId Quantity
  | ChangeRule RuleChange

data RuleChange
  = SetRule RuleId RuleValue EffectiveTime
  | ProcedureInput CollectiveProcedureId Stance

data Stance = Support | Neutral | Oppose   -- +1, 0, -1

data Origin
  = SeatInput SeatId PolicyKind
  | PolicyOutput PolicyId
  | Estimated (Array RepresentationId)

data PolicyKind = Program | Agent | Human

type Proposal =
  { id :: ProposalId
  , author :: EntityId
  , intents :: Array Intent         -- settle atomically, together
  , audience :: Audience
  , signers :: Array EntityId       -- required signers; the author's signature is implicit
  , origin :: Origin
  }

data Audience = Public | Entities (Array EntityId)
```

A Proposal binds Intents to the Entities that must sign them and records its origin. Each Intent is executed by the signer whose resources or authority it uses.

This separation is important because an Intent's economic meaning is independent of who generated it.

A seat, NPC, or Aggregate can all propose a Trade. The engine determines whether the action is permitted, feasible, and executable.

The origin also preserves an audit trail from economic outcomes back to human decisions, policies, or statistical estimates.

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
- Orders whose limit price or maximum spend cannot be satisfied are filled up to the binding constraint and the remainder rejected.
- Settlement of each Entity's accepted actions is atomic.

Scenarios may declare alternative allocation rules, provided they are deterministic and order-independent.

### 3.3 Proposals and signatures

Every Intent reaches the engine inside a Proposal. Phase 3 of each round, run by the round runner in the Policy layer, has two steps against the same snapshot ([§8.1](#81-canonical-resolution-sequence)):

- **Propose.** Every Policy publishes Proposals to their audiences. The Proposal set is frozen at the end of this step.
- **Sign.** Every Policy sees the snapshot and the Proposals addressed to it, and signs those it endorses. Publishing implies the author's signature.

A **unilateral action** (a Market Trade, a gift Transfer, a Transform, a ChangeRule or procedure input) has its author as the only required signer. An **offer** also requires its counterparties: SA's Proposal [Transfer 20 E to JP, Transfer 48 M to SA] with signers [SA, JP] is an offer, and JP's signature is the acceptance. An N-party deal requires N signers. Multi-signer Proposals may bundle only Transfers and Transforms; Trades always travel in single-author Proposals and follow the partial-fill rules ([§3.2](#32-infeasible-actions)). Each `ProposalId` is derived from (round, author, index in the author's submission), so recorded signatures replay deterministically.

Only fully signed Proposals proceed. A Proposal missing a signature **lapses**: it is recorded as a decline and the round resolves normally. Because acceptance is a signature on the offer's own terms, the two sides of a deal can never disagree.

A fully signed Proposal is escrowed at screening like any action ([§8.2](#82-single-round-update-and-snapshot-discipline)); each signer funds its own giving side from opening holdings. Its Intents settle atomically, or the whole Proposal is rejected and recorded, for example when escrow, transferability, or delivery fails.

Negotiation happens out of band for humans; the interpreter turns "we accept SA's deal" into a signature on that Proposal. Programs propose by rule and sign by rule (for example, "sign any Energy offer at ≤ 2.6 M/E"); AI agents read the Proposals addressed to them and choose. A counteroffer is a new Proposal, signable next round. The engine enforces no promises across rounds.

The Propose/Sign structure is the extension point for further coordination: more Propose/Sign iterations within a round, information-only Proposals, or threshold signer rules can be added without changing settlement.

## 4. Policies

A Policy is anything that produces Intents from the authorized view of the Entity it drives:

**Policy + Authorized View + Memory + Turn → Intents**

The engine (World State + Intents → World State) is separate from the Policies that produce Intents. Policies come in three kinds:

| Kind | Implementation | Reproduced from |
|---|---|---|
| **Declarative program** | NPC rules, Aggregate baselines, seat-policy presets, run on the shared interpreter ([§4.4](#44-shared-decision-language-interpreter)) | The game seed |
| **AI agent** | A language model given the role's briefing, objectives, and authorized view | Recorded Intents |
| **Human** | A person whose speech the AI interpreter converts into Intents | Recorded Intents |

Accepted canonical Intents are always recorded, and replay starts from them; deterministic programs can also be regenerated from the seed.

An NPC's Policy is part of the scenario. A seat's Policy is supplied from outside and bound at run time; swapping it (human, agent, or program) never changes the Actor's definition. Memory of in-world Policies lives in World State; memory of seat Policies lives in the session or run record, because seat Policies are not part of the game world.

### 4.1 NPC policies

An NPC is an explicit Actor whose Policy is part of the scenario. Its decisions have no special authority and undergo ordinary validation. For example, an NPC government may respond to tariffs with retaliatory restrictions, or propose and sign offers with other Actors.

### 4.2 Aggregate policies

Every Aggregate has a baseline Policy governing the behavior of its implicit population.

An Aggregate Policy might determine consumption, investment, production, borrowing, or other economic activity based on available information. It declares which behavioral domains it controls, so Representations can adjust the correct outputs.

Its generated actions provide the baseline behavior of the population in the absence of representative inputs. Aggregates sign only their own Proposals unless the Gameboard explicitly allows them to sign multi-signer Proposals.

### 4.3 Policy semantics

The rules below apply to in-world Policies (NPC and Aggregate). Their definitions are immutable and versioned for the duration of a game, including their conditional and time-dependent behavior.

Their outputs vary as the world evolves: prices, shortages, institutional rules, relationships, and observations are all mutable World State that Policies react to. Their decision rules do not change because a language model interprets an event differently.

An initialized Policy must:

- Operate only on an authorized view of the world.
- Produce typed Intents.
- Store any persistent memory in World State.
- Respect deterministic execution, using seeded randomness only where its definition explicitly calls for it.
- Remain distinct from institutional rules that players may be authorized to change.

A seat changing a tax rate changes an economic rule through `ChangeRule`. It does not rewrite the behavioral Policy of any affected Entity.

The consequences of that tax change emerge when Markets and Policies respond to the new conditions.

### 4.4 Shared decision-language interpreter

All declarative programs (NPC Policies, Aggregate Policies, and seat-policy presets) run on one shared **decision-language interpreter**. They differ only in where they are configured and which Entity they drive. A seat preset and an NPC Policy expressing the same rule therefore behave identically.

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
  { curve :: CurveSpec          -- linear, piecewise-linear, or convex family
  , domain :: Interval          -- supported position range
  , reset :: Maybe ResetRule    -- e.g. position resets each round
  }
```

The type system does not require a particular curve family. Implementations may begin with linear or piecewise-linear curves and later support more general convex cost functions.

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
  , access :: AccessRules
  , scope :: MarketScope
  }
```

The base Resource is the good being exchanged. The quote Resource is the denomination used to price it.

The same cost function prices purchases and sales using positive and negative quantities, subject to inventory and reserve constraints.

Markets cannot supply resources they do not hold.

### 5.3 Facilities (costed Transformations)

A facility is a capacity-constrained Transformation whose marginal cost is priced by a CostFunction.

```purescript
type Transformation =
  { id :: TransformationId
  , inputs :: Holdings            -- per unit
  , outputs :: Holdings           -- per unit
  , cost :: Maybe CostFunction    -- money price of congestion or increasing marginal cost
  , capacity :: CapacityRef
  , payee :: Maybe EntityId       -- recipient of money paid
  , delay :: RoundOffset
  , authority :: Capability
  }
```

Production, consumption, procurement, issuance, destruction, and investment are Transformations. They modify holdings only when the required inputs, capacities, and authority are available.

Money paid to a facility goes to a declared payee Entity, such as the producer, its workforce, or its suppliers, so that expenditure re-enters circulation. Facilities hold no reserves; they pay a payee.

A facility may be presented as a "market" in the player interface, but its accounting is that of a Transformation: outputs are created only by the recipe, within capacity.

Resource transfers preserve quantities. Production and consumption account for changes according to declared recipes. Financial claims obey their associated issuance and liability rules.

### 5.4 Market scope and geography

Each commodity keeps a single Resource type. Geography is expressed through:

- Scoped or regional Market nodes.
- Market access rules that determine which Entities may trade.
- Tariffs and taxes applied to particular participants or origins.
- Transport-capacity constraints linking nodes.

Trade restrictions, tariffs, and sanctions therefore have identifiable mechanical effects: they change access, prices, or throughput for specific participants.

### 5.5 Simultaneous clearing

All orders in the round update are resolved as one batch, and each Market clears at a single uniform price. The steps below define how that price is computed. They do not order individual orders.

For a given Market:

1. Orders whose access rules fail are rejected.
2. Tariffs and taxes are applied to effective prices.
3. Compatible purchases and sales are netted.
4. Residual net demand is executed against the cost function, within domain and reserve limits.
5. A uniform batch price is computed by the Market's declared rule. The default is the average marginal cost over the net quantity, or the current marginal price when net demand is zero.
6. Orders whose limit price or maximum spend is violated at the computed price are removed or reduced, and the price is recomputed. Removed orders stay removed, so the procedure terminates and depends only on the set of orders. Partial fills follow the rules in [§3.2](#32-infeasible-actions).
7. Holdings and reserves change as part of the round's single net update.

The clearing result must not depend on the order in which submissions were received.

### 5.6 Market-rule changes

Institutional actions may modify market conditions, including taxes, tariffs, access, permitted participants, transport capacity, or available capacities.

A rule change must have a declared scope and effective time. By default it takes effect at the next round boundary.

## 6. Economic Circulation

The economy must sustain repeated rounds. A world in which resources can only be spent, and never earned or produced, would make shortages indistinguishable from incomplete modeling.

Each round's single update includes a recurring stock-flow cycle. Its four components are computed together from the snapshot, not in sequence ([§8.2](#82-single-round-update-and-snapshot-discipline)):

- **Production.** Authorized Transformations create outputs, such as Energy, from capacity and inputs.
- **Exchange.** Markets clear, and Money moves to suppliers and producers.
- **Income and expenditure.** Explicit rules distribute earnings, wages, taxes, public spending, and transfers among Entities.
- **Capacity renewal.** Investments, depreciation, and degradation adjust future capacities.

This requires no additional Resources. It requires production rules, accounting-consistent financial flows, and productive-capacity state.

Every Resource stock reconciles each round:

**Closing stock = Opening stock + Production + Issuance − Consumption − Destruction − Decay**

For each Resource, transfers and trades sum to zero across all accounts, including Market reserves.

Every unit of Money paid out has a recorded recipient.

## 7. Aggregates and Representation

Representation allows an explicit Actor to influence the behavior of an implicit Aggregate.

This mechanism is necessary because the game supports players operating at very different economic scales and institutional positions.

A small-business owner can affect the same economy as a government, but should not receive the government's resources or authority. A member state can influence a bloc without owning the bloc.

### 7.1 Aggregate semantics

An Aggregate maintains:

- A membership definition and any statistical strata.
- Residual economic accounts and capacities.
- A baseline behavioral Policy.
- An optional collective decision procedure and institutional capabilities.
- Relationships to any representative Actors.

**An Aggregate's economic accounts represent the residual population after explicitly modeled accounts have been assigned to other Entities.**

An explicit small-business Actor owns its own resources. The small-business Aggregate holds the resources attributed to the remaining, implicitly represented businesses.

Making the head of a government explicit extracts only the accounts and responsibilities assigned to that government Actor. The households and firms of that country remain in whichever Aggregate owns them.

### 7.2 Representation and estimators

A Representation links an explicit Actor to an Aggregate through one estimator interface.

```purescript
type Representation =
  { id :: RepresentationId
  , actor :: EntityId
  , aggregate :: EntityId
  , domain :: BehaviorDomain
  , estimator :: Estimator
  }

type Estimator =
  { weights :: Map EntityId Ratio       -- member or sampling weights
  , shrinkage :: Ratio                  -- 1 = inputs fully applied
  , deviationBound :: Maybe Interval    -- clamp on each input's deviation
  , delay :: RoundOffset
  , backing :: Maybe BackingRequirement -- reservation or payment required for an input to count
  , procedure :: Maybe CollectiveProcedureId
  , noise :: Maybe SamplingNoise        -- seeded sampling variance (§9.1)
  }

type SamplingNoise =
  { spread :: Ratio }                   -- maximum perturbation of a deviation, in either direction
```

Two named presets cover the common cases. They are configurations of the same type, not separate mechanisms.

| Preset | Configuration | Typical use |
|---|---|---|
| `constituent` | Known member weights, shrinkage 1, no deviation bound, no noise, linked to a collective procedure. | A member state influencing a bloc's decision. |
| `statistical` | Sampling weights, shrinkage < 1, sampling noise, bounded deviation, optional backing. | A small-business owner informing small-business demand. |

Neither preset transfers holdings or capabilities. The domain constrains which parts of the Actor's behavior may influence the Aggregate: a small-business owner's purchasing may inform small-business demand, but does not authorize taxation or determine the Aggregate's political behavior.

### 7.3 Representation pipeline

Representation proceeds through four stages, and the engine records each one:

1. **Observation.** Derived from a validated proposal or procedure input, before clearing.
2. **Estimate.** The estimator's output.
3. **Proposed Aggregate Intent.** The reconciled Aggregate action.
4. **Executed result.** The settled outcome after feasibility checks.

An observation is taken from the representative's admissible intended action, not its executed fill. Any difference between intended and executed actions is recorded.

### 7.4 Estimation semantics

Representation is an inference or aggregation operation, never a resource multiplier.

For behavioral quantities, each input's deviation from its reference is clamped and combined:

**Deviationᵢ = clamp(deviationBound, (observedᵢ − referenceᵢ) / referenceᵢ + εᵢ)**

**εᵢ = spread × (2uᵢ − 1)**, where uᵢ is the seeded draw for this input ([§9.1](#91-numerics-and-randomness)). Without `noise`, εᵢ = 0.

Sampling noise models the uncertainty of inferring a population from a few representatives. One representative decision therefore moves the Aggregate by a slightly different amount in each game. The noise is added before clamping, so the deviation bound still caps the total effect. Noise applies only to actual observations.

**Adjusted baseline = Baseline × (1 + shrinkage × Σ(weightᵢ × deviationᵢ))**

For collective procedures, inputs are stances, and the procedure combines them with the residual Policy's own input:

**Support = Σ(weightᵢ × stanceᵢ) + residualWeight × residualInput**

The procedure's decision rule, for example a threshold, determines which actions the institution proposes, exercised under its own capabilities and paid from its own accounts. A procedure input is independent of resource transfers; a procedure may separately require a contribution to fund an action.

An optional delay applies an input in a later round. An optional backing requirement makes an input count only if the representative reserves or spends real resources behind it.

Representatives are strategic players, not random samples, and have an incentive to act for influence. The type system does not try to eliminate this, because disproportionate influence for small players is a design goal. It makes influence explicit and bounded. Estimator parameters are declared balancing assumptions, not claims of unbiased inference.

When no input is available, the Aggregate follows its baseline Policy.

### 7.5 Multiple representatives

Multiple Actors may represent the same Aggregate or different strata within it.

Inputs are combined by the estimator's weights, which account for overlapping and possibly biased samples. Representatives are not automatically treated as independent or equally informative.

### 7.6 Avoiding double-counting

Two rules are essential.

**Economic accounting:** An explicit Actor's accounts are excluded from the target Aggregate's accounts. Its direct economic transactions are executed once.

**Behavioral accounting:** The Aggregate's representative-adjusted behavior replaces the corresponding baseline behavior. It is never added to an already complete baseline action.

A representative decision may therefore have two distinct effects:

1. A direct effect on the representative Actor's own holdings.
2. An estimated or procedural effect on the Aggregate.

Both effects are independently validated, and neither is counted twice.

## 8. World State and Round Resolution

World State is the complete economic snapshot at a round boundary.

It includes Entities, their accounts and capacities, Markets, institutional rules, reservations, scheduled events, Policy memory, and the current point in simulated time.

Policies read World State but cannot mutate it; they only submit Intents.

The engine advances the world through a deterministic round-resolution function.

### 8.1 Canonical resolution sequence

This is the single authoritative resolution sequence. The Gameboard, Instantiated Game, and Simulation Harness specifications reference it and do not redefine it.

**1. Prepare round**

Apply delayed production deliveries, scheduled shocks, and rule changes whose effective time has arrived.

**2. Create observations**

Produce the beginning-of-round snapshot and each controller's permitted view of it.

**3. Generate Proposals** *(round runner, Policy layer)*

- **3a Propose.** Every Policy publishes Proposals against the snapshot at the same time: seats through whatever Policy drives them (human via the AI interpreter, AI agent, or program), NPC Policies, and Aggregate baseline Policies. The set is frozen at the end of 3a.
- **3b Sign.** Every required signer signs the Proposals addressed to it that it endorses ([§3.3](#33-proposals-and-signatures)).

Phases 1–2 and 4–7 are engine phases.

**4. Screen and interpret**

Check authority, entity identity, and signature completeness. Incompletely signed Proposals lapse and are recorded. Extract admissible observations and procedure inputs from validated Proposals.

**5. Reconcile Aggregates**

Combine each Aggregate's baseline behavior with its estimates and collective-procedure outcomes, exactly once.

**6. Resolve economy**

Resolve every accepted action of the round as one simultaneous batch against the prepared snapshot ([§8.2](#82-single-round-update-and-snapshot-discipline)). Markets clear at uniform prices. Transformations, signed multi-party Proposals, and the economic cycle (production, exchange, income and expenditure, capacity renewal) are evaluated together, and the result is a single net change to World State.

**7. Commit and record**

Atomically commit the new World State. Record intended and executed actions, events, diagnostics, and balance-sheet data for the reporting layer.

### 8.2 Single round update and snapshot discipline

All Actor and Aggregate actions for a round are bundled into a single round update and resolved together. Clearing is uniform, so there is no sequencing of actions within a round: no action settles "before" another, and submission order is irrelevant ([INV-11](#inv-11)). The phases in [§8.1](#81-canonical-resolution-sequence) are functional dependencies between stages, such as screening before reconciliation. They never order individual actions.

Two rules make this hold:

- **Funding from the snapshot.** Each Entity's bundle is funded from its available holdings at the beginning of the round. At screening, each signed Proposal escrows its maximum cost: limit price × quantity, maximum spend, or a facility's full-capacity price. Joint feasibility and pro-rata scaling ([§3.2](#32-infeasible-actions)) are checked against those escrows, and unused escrow is released at commit.
- **Credit at commit.** Proceeds, production, income, and receipts from signed Proposals this round are credited when the round commits and become usable next round. An Entity cannot sell Energy and spend the proceeds in the same round. Selling and buying in the same round both draw on opening holdings.

Passive consumption rules (such as operating requirements), decay, and derived flows (such as revenue sharing) are functions of the batch result. They are evaluated on each Entity's net position at commit, not as later steps.

All decisions in a round operate on the beginning-of-round snapshot. A price change caused by this round's clearing becomes observable in the next round.

This prevents circular behavior in which a representative alters Aggregate demand, which changes prices, which changes the representative's observation, which changes Aggregate demand again.

If same-round feedback is ever desired, it must be implemented as an explicitly specified equilibrium procedure, not as an accidental effect of execution order.

### 8.3 Functional interface

The fundamental engine operation is a pure state transition.

```purescript
type SignedProposal = { proposal :: Proposal, signatures :: Array EntityId }

-- Engine: signed Proposals are its only input
resolveRound :: Engine -> WorldState -> Array SignedProposal
             -> Either EngineError RoundResult

-- Round runner (Policy layer): effectful, written against effect signatures (§8.4)
runRound :: Engine -> Policies -> WorldState
         -> Run (RANDOM + SEAT_INPUT + RECORD + r) (Either EngineError RoundResult)
runRound engine policies world0 = do
  let world = prepareRound engine world0                    -- phase 1 (engine, pure)
      views = observe engine world                          -- phase 2 (engine, pure)
  proposals <- propose policies views                       -- 3a: seats, NPCs, Aggregates
  signed    <- sign policies views proposals                -- 3b
  let result = resolveRound engine world signed             -- phases 4–7 (engine, pure)
  traverse_ record result
  pure result
```

The round runner evaluates every Policy, seat or in-world, through the same code in live play and simulation; in-world declarative programs run on the shared interpreter. `resolveRound` screens authority and signature completeness, records lapsed Proposals, reconciles Aggregates once, runs the economic cycle and clearing, and commits the new World State with accepted and rejected actions, intended-versus-executed records, and events.

`EngineFailure` represents a failure of the configured simulation or its execution. Lapsed Proposals and ordinary infeasible actions are reported as outcomes and never fail the round.

### 8.4 Effects and handlers

The whole system is pure. The engine has no effects at all: `resolveRound` is a pure function of World State and signed Proposals. In-world randomness, such as estimator sampling noise, uses the pure keyed `draw` function ([§9.1](#91-numerics-and-randomness)) with the game seed held in World State, so it needs no effect.

Everything impure is an algebraic effect, and only the round runner and the code around it (sessions, the harness, Dashboard reads) use effects. That code is written against effect signatures, never concrete IO:

| Effect | Operation |
|---|---|
| `Random` | Keyed draws from a named stream: `draw :: Stream -> DrawKey -> UnitInterval` |
| `SeatInput` | A seat's Proposals in Propose and its signatures in Sign |
| `Interpret` | Transcript → candidate Intents, with confirmation |
| `Narrate` | Resolved events → front page and balance-sheet text |
| `Record` | Append round records and snapshots |
| `Clock` | Session deadlines (live play only) |

Handlers choose the context; the round runner is the same code under all of them:

- **Live session:** `SeatInput` via humans and `Interpret`; `Clock` enforces deadlines.
- **Simulation:** `SeatInput` via preset programs or AI agents; `Random` serves the game and player streams.
- **Replay:** `SeatInput` feeds recorded Proposals and signatures.
- **Test:** all handlers pure.

Determinism follows from purity: identical handler inputs (seeds and recorded seat input) give an identical run, and replay is simply running under the replay handler.

## 9. Architectural Invariants and Numerics

This is the single numbered invariant table for the whole system. The Gameboard, Instantiated Game, and Simulation Harness specifications cite these IDs and keep no tables of their own. Each simulator required assertion maps to one ID. Layer tags: **E** engine, **G** Gameboard, **I** instantiation, **H** harness.

| ID | Layer | Invariant | Requirement |
|---|---|---|---|
| <a id="inv-01"></a>INV-01 | E | Unified action semantics | All participants act through the same Intent and validation machinery. |
| <a id="inv-02"></a>INV-02 | E | Authority separation | An Entity exercises only capabilities it has been granted, within scope. |
| <a id="inv-03"></a>INV-03 | E, G, I | No authority amplification | Membership, Representation, and fallbacks never confer or remove an institution's capabilities or holdings. |
| <a id="inv-04"></a>INV-04 | E | Resource consistency | Holdings change only by authorized exchange, transformation, issuance, destruction, or decay, per conservation class; no prohibited negative holdings. |
| <a id="inv-05"></a>INV-05 | E, G, I | Stock-flow reconciliation | Each round every stock reconciles ([§6](#6-economic-circulation)), every Money payment has a recorded recipient, and nothing accumulates in an account without a declared outflow rule. |
| <a id="inv-06"></a>INV-06 | E, G | Market solvency | Markets and Transformations respect reserves, capacities, and declared limits. |
| <a id="inv-07"></a>INV-07 | E, G | Price is not production | A CostFunction never creates an asset without an authorizing Transformation. |
| <a id="inv-08"></a>INV-08 | E, G, I | Account partition | Every account and behavioral responsibility has exactly one owner, however memberships overlap. |
| <a id="inv-09"></a>INV-09 | E | Representation consistency | Adjustments replace the matching baseline exactly once, within estimator bounds, and never duplicate direct activity. |
| <a id="inv-10"></a>INV-10 | E | Signature completeness | A multi-signer Proposal executes only if every required signer signed it; its Intents settle atomically or are rejected together. |
| <a id="inv-11"></a>INV-11 | E | Order independence | All actions resolve as one bundled round update with uniform clearing; permuting submissions never changes settlement, and no action depends on another settling first. |
| <a id="inv-12"></a>INV-12 | E, G | Policy reproducibility | Policy output is determined by its versioned definition, permitted view, World State memory, time, and declared randomness. |
| <a id="inv-13"></a>INV-13 | E, H | Deterministic replay | The same manifest, seeds (game, and player where applicable), and recorded seat input reproduce identical World States. Every random value is a keyed draw from a named stream ([§9.1](#91-numerics-and-randomness)). |
| <a id="inv-14"></a>INV-14 | E, G, I, H | Resolution consistency | Instantiating a world at different levels of explicitness reconciles accounts, liabilities, and authority before any decisions differ. |
| <a id="inv-15"></a>INV-15 | E, I | Single authoritative state | All actions modify one shared World State and timeline. |
| <a id="inv-16"></a>INV-16 | E, I | Information isolation | Every Policy, in-world or seat, observes only what the Gameboard permits. |
| <a id="inv-17"></a>INV-17 | G | Type-system conformity | All scenario definitions use supported types and operations. |
| <a id="inv-19"></a>INV-19 | G | Role consequence | Every Actor definition declares a capability, a dependency, and an objective. |
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

Each game instance has exactly one **game seed**, fixed at initialization and recorded in the manifest. Every in-world draw (estimator sampling noise, declared Policy randomness, declared shocks) uses it. The simulation harness adds a **player seed** for all seat-Policy randomness; it exists only under the simulation handler, and live play has none. Nothing reads system randomness. Draws are made through one pure function over two named streams:

```purescript
data Stream = Game | Player
draw :: Seed -> DrawKey -> UnitInterval   -- the stream selects the seed
```

The streams never share draws, so varying the player seed never changes an in-world draw, and vice versa.

A `DrawKey` names the draw's semantic purpose: round, Representation or Policy, input Entity, draw category, and index (for example `round-3 / sme-energy-sample / SME / sampling / 0`). Because draws are keyed rather than consumed from a sequence, adding an Entity or changing evaluation order never changes unrelated draws. The PRNG algorithm and the key serialization are versioned in the manifest. Each draw is converted to a quantity under the fixed-point rule below.

All quantities, prices, and settlement amounts use fixed-point arithmetic with **4 decimal places**, rounded **half-to-even** at each settlement step. Uniform batch prices are rounded before they are multiplied by quantities. No floating-point arithmetic enters economic state. Other specifications reference this rule and do not restate it.

## 10. Design Boundaries

The type system deliberately does not prescribe the following:

**Specific resource classifications.** These belong to the Gameboard, informed by vertical research. The type system supplies only Resource properties.

**A universal macroeconomic model.** Economic behavior is expressed through configured Policies, Markets, Transformations, income rules, and institutional rules.

**A single estimator parameterization or collective procedure.** Representation provides one estimator interface with configurable presets and procedures.

**An LLM-based economic authority.** Language models interpret submissions and narrate outcomes. They do not bypass economic validation or directly mutate World State.

**A fixed degree of market realism.** The same interfaces can support simple bonding curves or more sophisticated clearing mechanisms.

## 11. Summary

The system has one economic ontology, one shared World State, and one resolution mechanism. Entities own accounts and capabilities, with membership, ownership, and collective decision-making kept separate; Actors are explicit seats or NPCs, Aggregates hold residual accounts and baseline behavior. Intents are the engine's only input and express every economic or institutional action; Policies of any kind produce them, Representations turn explicit decisions into bounded Aggregate adjustments through one estimator interface, and CostFunctions price both Market exchange and facility production in a circulating economy resolved as one bundled round update. A game instance is a particular resolution of a larger economic world, and changing that resolution must preserve the world's economic meaning.
