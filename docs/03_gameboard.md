# Scenario Planning Game — Gameboard Specification

**Version:** 0.2
**Scope:** Gameboard Layer
**Implementation language:** PureScript

---

## 1. Purpose

The Gameboard defines a reusable economic and strategic scenario. It configures the primitives of the Type System into a coherent world: resources, Markets, Transformations, institutions, populations, and structural relationships.

A Gameboard defines the **persistent conditions and tensions of a scenario**, not the decisions or outcomes of any playthrough. For example, it might establish US–China competition, semiconductor dependencies, and capital–labor tension. These stay the same across playthroughs; participants' actions and the resulting history differ.

The Gameboard defines the available economic world; instantiation chooses its **resolution** (Type System §2.5). Changing that resolution must preserve the world's initial economic meaning (INV-14).

Layer responsibilities, the glossary, invariants, numerics, and the canonical round sequence are defined in the Type System specification (§1, §2.5, §9, §9.1, §8.1). This document uses those terms and IDs and does not restate them.

The Gameboard is declarative: it configures existing types, implements no simulation machinery, sustains circulation across the horizon (INV-05), and is versioned and research-grounded (INV-22).

Conceptually:

**Type System + Gameboard Definition → Validated Gameboard**

**Validated Gameboard + Instantiation Configuration → Initial World State**

The Gameboard holds no player assignments, submitted Intents, transactions, or evolving state. It configures the phase schedule within the canonical sequence but does not redefine it.

## 2. Gameboard Structure

| Domain | Responsibility |
|---|---|
| **Scenario** | Setting, time horizon, central tensions, strategic questions. |
| **Verticals** | Research-based models of economic sectors and their dependencies. |
| **Economy** | Resource definitions, exchange Markets, Transformations, the recurring cycle, geography, constraints. |
| **Entities** | Actor definitions, Aggregates, memberships, account ownership, collective procedures, capabilities. |
| **Relationships** | Persistent geopolitical, economic, and social relationships. |
| **Policies** | NPC and Aggregate decision programs. |
| **Representations** | Eligible relationships between Actors and Aggregates, with estimator presets. |
| **Game Rules** | Phase schedule, visibility, available actions, evaluation, initialization constraints. |

These are configuration domains, not additional primitives.

```purescript
type Gameboard =
  { id :: GameboardId
  , version :: Version
  , scenario :: Scenario
  , verticals :: Array Vertical
  , economy :: EconomicStructure
  , entities :: EntityCatalog
  , relationships :: Array StructuralRelation
  , policies :: PolicyCatalog
  , representations :: Array RepresentationScheme
  , rules :: GameRules
  }
```

## 3. Scenario

The Scenario defines:

- **Context:** the circumstances in which the game begins.
- **Time horizon:** ordinarily five simulated years.
- **Central tensions:** the competing interests that give the game its structure.
- **Strategic questions:** the uncertainties the game explores.
- **Evaluation dimensions:** outcomes measured at the end (§10.5).

The Scenario says which forces are relevant, not how they resolve.

### 3.1 Strategic tensions

A strategic tension is a persistent relationship among economic or political interests: great-power competition, labor versus capital, incumbents versus new technologies, economic security versus trade, competition for scarce inputs.

Every tension must be **mechanically consequential**: connected to resources, capabilities, Transformations, income rules, or market access that participants can act on. Geopolitical competition can appear as industrial dependencies, scoped market access, and competing demand. Class conflict can appear as wage and income rules, a labor capability to reduce production throughput, and the division of production revenue between capital and workers.

A tension never requires Actors to behave antagonistically. Cooperation and unexpected alliances must remain possible.

## 4. Verticals

A Vertical is a researched sector of the economy, derived from a **vertical report** prepared before initialization.

### 4.1 Vertical definition

| Component | Description |
|---|---|
| **Scope** | Activities and boundaries of the sector. |
| **Resources** | Inputs, outputs, and assets, with declared Resource properties. |
| **Capacities** | Productive, transport, or market-access capacity held as economic state. |
| **Markets and Transformations** | How the sector exchanges and produces. |
| **Circulation** | How the sector earns, pays, and reinvests. |
| **Participants** | Important institutions, businesses, and populations. |
| **Dependencies** | Relationships with other Verticals. |
| **Behavioral assumptions** | Inputs to Policy configuration. |
| **Constraints** | Scarcity, capacity limits, regulation. |

Not every meaningful quantity is a Resource. Production capacity, market access, transport capacity, and supply-chain efficiency are usually economic state or rules. A semiconductor producer can hold valuable capacity without a Semiconductor Resource.

### 4.2 Vertical compilation

**Vertical Report → Validated Economic Definitions → Gameboard**

Reports are research documents, not executable models. Compilation translates findings into explicit configuration and preserves provenance: sources, units, uncertainty, simplifications. Facts unsuitable for mechanical execution stay as context.

Verticals share common Resources, Markets, and Entities rather than duplicating them. One Vertical may consume another's outputs, compete for inputs, or pay revenue into it, so changes propagate through shared mechanisms without a prescribed narrative.

## 5. Economic Structure

```purescript
type EconomicStructure =
  { resources :: Array ResourceDefinition
  , markets :: Array MarketDefinition
  , transformations :: Array TransformationDefinition
  , cycle :: EconomicCycle
  , geography :: Geography
  , constraints :: Array EconomicConstraint
  }
```

### 5.1 Resources

Each Resource definition declares its identity, unit, and the properties defined in Type System §2.3: conservation class, transferability, decay rule, and whether it can be reserved. The engine accounts for a Resource from these properties; no behavior is attached to particular Resource names. Definitions must be consistent across Verticals and Entities.

### 5.2 Exchange Markets

An exchange Market trades existing reserves along a CostFunction. Each definition declares:

- Base and quote Resources.
- CostFunction family and parameters.
- Liquidity and reserve behavior.
- Order features (limit price, maximum spend) and the Type System's deterministic allocation rules.
- Participation, access, and tariff rules (§5.5).

The Gameboard defines behavior; the Instantiated Game supplies the initial reserves and position.

### 5.3 Facilities

A facility consumes declared inputs and issues declared outputs under a production rule, priced by the same CostFunction primitive. Each definition declares:

- Inputs, outputs, and ratios.
- The capacity limiting throughput, and its owner.
- An optional CostFunction for congestion or rising marginal cost.
- The **payee**.
- Delivery timing.
- Required capabilities.

Defense procurement and public campaigns are typical facilities. Facilities hold no reserves; they pay a payee (INV-05, INV-07).

### 5.4 The recurring economic cycle

The Gameboard configures the cycle defined in Type System §6. Its components are computed together from the snapshot within the single round update:

- **Production** from capacity and inputs.
- **Exchange**, with Money flowing to suppliers.
- **Income and expenditure**: earnings, wages, taxes, public spending, investment.
- **Capacity renewal**: investment, depreciation, damage.

```purescript
type EconomicCycle =
  { production :: Array ProductionRule
  , income :: Array IncomeRule
  , renewal :: Array CapacityRule
  , period :: SimulatedDuration
  }
```

Income rules connect the explicit and implicit economy. A government Actor holds only government-controlled resources and receives declared tax or revenue from private economies held by other Entities or Aggregates. Workforces receive wages from production; small businesses earn revenue from household demand. Rates are per simulated period, scaled to each round.

Prefer a simple, complete cycle to sophisticated pricing on an economy that cannot sustain itself.

### 5.5 Geography and market access

One Resource per commodity, scoped by:

- **Regional Market nodes** for the same Resource.
- **Access rules** on who may trade.
- **Tariffs** on specified buyers, sellers, or origins.
- **Transport capacity** between nodes, held by an owning Entity.

A trade restriction enacted through `ChangeRule` changes access or tariff state, giving restrictions and disruptions identifiable effects without new Resources.

### 5.6 Constraints

Persistent limits on capacity, availability, regulation, financial obligations, authority, and geography, enforced by Type System validation.

## 6. Entity Catalog

The catalog defines possible participants: institutions that may be explicit and populations that may stay aggregate.

### 6.1 Projections

The catalog configures the three projections of Type System §2.2 separately: **membership** (may overlap), **account ownership** (one owner per account and responsibility, INV-08), and **collective procedure and capabilities**. Membership does not imply ownership; ownership does not imply authority; influence over a collective procedure does not grant its capabilities (INV-03).

### 6.2 Actor definitions

An Actor definition declares:

- Identity and scope, including exactly which accounts and responsibilities it owns when explicit.
- Memberships and capabilities.
- Whether it may be a seat, an NPC, or either, and its applicable NPC Policies.
- Initial-state constraints.
- A **fallback** (§6.5).

It also declares three scenario properties (INV-19):

- **Meaningful capability:** an intervention beyond simple trading, such as controlling transport capacity, organizing a strike, or supplying production capacity.
- **Economic dependency:** something others can provide or withhold.
- **Strategic objective:** an outcome others can help or obstruct.

These use existing capabilities, Transformations, capacity state, and rule changes, so every role has consequential choices beyond its starting holdings. A government Actor holds government-controlled resources only.

### 6.3 Aggregate definitions

An Aggregate definition declares:

- A membership definition and strata.
- The accounts and responsibilities it owns as residual.
- A baseline Policy.
- An optional collective procedure and its capabilities.
- Constraints on estimated behavior and eligibility for Representation.

Examples: households, small businesses, industrial producers, a regional bloc.

When a member is made explicit, only the accounts and responsibilities it owns leave the residual; it stays in the membership definition (Type System §7.1).

### 6.4 Overlap

Memberships may overlap (a manufacturer may be a small business, a manufacturer, and part of a regional economy). Duplicate ownership or duplicate execution may not. The Gameboard assigns every account class and responsibility one canonical owner, independent of classification.

### 6.5 Fallbacks

Every eligible Actor definition declares a fallback (INV-25):

| Fallback | Meaning |
|---|---|
| **Implicit economic coverage** | A named Aggregate owns its accounts and activity. |
| **Institutional Policy** | An implicit Policy or rule mechanism retains its decisions and capabilities. |
| **Permitted omission** | It may be absent without leaving required responsibilities unowned. |

These are not interchangeable: a government's regulatory authority never moves to a household or business Aggregate (INV-03).

## 7. Structural Relationships

| Category | Example |
|---|---|
| **Economic dependency** | A manufacturer depends on imported inputs. |
| **Competition** | Two institutions compete for investment or access. |
| **Institutional authority** | A government regulates particular Markets. |
| **Membership** | A state belongs to a bloc whose collective procedure it can influence. |
| **Alignment** | Shared objectives or agreements. |
| **Distributional conflict** | Costs and benefits fall unequally. |
| **Exposure** | Vulnerability to a market condition. |

These are configuration, not execution primitives. The Gameboard fixes persistent structure; World State records evolving rules, reservations, and positions.

Every relationship must have a mechanical interpretation: constraining actions; configuring Policies, access, or income rules; defining dependencies and exposures; or contributing to evaluation. A purely descriptive relationship stays context and never alters outcomes. The engine infers no mechanical effects from narrative at runtime.

## 8. Policy Catalog

The catalog holds in-world Policies: declarative, versioned programs in the shared decision language, run by the interpreter of Type System §4 (INV-12, INV-29). Seat Policies (human, AI agent, or preset program) are supplied outside the scenario and are not catalogued here. The catalog holds parameterized definitions, not runtime assignments.

- **NPC Policies** express objectives, conditional responses to indicators and to other Actors, and time-dependent behavior, including which Proposals to publish to other Actors and which Proposals addressed to them to sign.
- **Aggregate Policies** produce baseline population behavior (demand, production, investment, consumption, employment) and declare the behavioral domains they control, so an estimator adjusts each output exactly once (INV-09). An Aggregate with a collective procedure uses its Policy for the residual input.

The Gameboard declares input views, parameter ranges, and defaults; the Instantiated Game selects values. Policy memory lives in World State. Programs never change during play; decisions change because World State changes.

Observation permissions are declared per Policy category, so NPCs do not see the whole simulation (INV-16).

## 9. Representation Schemes

A scheme declares **which populations may be represented and how inputs are interpreted**. Actual representatives are assigned at instantiation.

```purescript
type RepresentationScheme =
  { id :: SchemeId
  , eligibleActors :: Array EntityDefinitionId
  , target :: AggregateDefinitionId
  , stratum :: StratumId
  , domain :: BehaviorDomain
  , estimator :: Estimator
  }
```

`Estimator` is defined in Type System §7. A scheme normally starts from a named preset:

| Preset | Use | Typical parameters |
|---|---|---|
| `statistical` | Infer population behavior from sampled representatives. | Sampling weights, shrinkage < 1, sampling noise spread, deviation bound, optional delay and backing. |
| `constituent` | Combine known members' procedure inputs through a collective procedure. | Declared member and residual weights, shrinkage 1, no bound. |

For `statistical` schemes, representatives know their choices move the Aggregate, so observations are not independent samples. The Gameboard accepts this as design and keeps the influence explicit and bounded; estimator strength is a balancing parameter, not a claim of unbiased inference. A backing requirement can make an observation count only if the representative reserves or spends matching resources.

For `constituent` schemes, members submit procedure inputs, which move no resources. A Gameboard may separately require a contribution to fund a collective action, but contributing is never the only way to express support.

Every scheme separates the representative's direct actions, the Aggregate's baseline, and the adjustment, which replaces the matching baseline once (INV-09). Multiple representatives are combined by the estimator. Representation never transfers holdings or capabilities (INV-03); resulting Aggregate Intents pass ordinary validation.

## 10. Game Rules

### 10.1 Proposal rules

Agreements reach the engine only as Proposals that need more than one signer (Type System §3.3, INV-10). The Gameboard declares which Resources may move through multi-signer Proposals, between which Entity categories, and which audiences a Proposal may have. NPC Policies may publish and sign Proposals like any other Policy. Aggregates sign only their own Proposals unless the Gameboard explicitly allows an Aggregate to sign multi-signer Proposals (Type System §4.2).

### 10.2 Temporal structure and phase schedule

The Gameboard declares the horizon, supported round counts, round-to-time mapping, facility delivery delays, rule-change effective times (default: next round boundary), and scheduled shocks. Deliveries, shocks, and newly effective rules apply in the Prepare Round phase. A typical Gameboard covers five years in four to eight rounds; the Instantiated Game picks the schedule.

Mechanics are deterministic; randomness appears only where a declared shock or estimator calls for it, such as `statistical` sampling noise. The Gameboard declares each estimator's permitted noise range; every in-world draw comes from the single game seed (Type System §9.1).

### 10.3 Information visibility

The Gameboard declares what is public, private, or capability-gated: prices, holdings, indicators, Proposals beyond their audience. The same rules constrain every Policy, whether NPC or seat (INV-16). Visibility never changes economic facts.

### 10.4 Available actions

The Gameboard selects which Intent categories and Transformations are meaningful (for example trade, production, taxation, regulation, and procedure inputs) and which Resources may move in multi-signer Proposals. All use the Type System's Intent language and deterministic rejection and partial-fill rules. No unrestricted state mutations.

### 10.5 Evaluation

- **Actor objectives** derived from each Actor definition's strategic objective.
- **System metrics** such as resource security, distributional outcomes, market functioning, and institutional resilience.

No single winner, and outcomes are not reduced to Money. Measuring an outcome does not change it.

## 11. Initialization Constraints

The Gameboard defines defaults, ranges, and consistency constraints, not all initial values. Parameters may cover initial distributions, reserves and liquidity, capacities, production and income rates, population sizes and strata, Policy parameters, representative participation and estimator strength.

Persistent strategic structures do not change between instantiations; only these parameters vary.

Actors may be mandatory or optional, with compatibility conditions. A scenario might require explicit US and China while letting the EU be human, NPC, or covered by its fallback.

When a population is partially made explicit, initialization must:

- Allocate the explicit Actor's owned accounts to it and leave the rest residual.
- Exclude directly modeled activity from the residual baseline.
- Preserve memberships.
- Give capabilities and collective procedures valid owners.
- Redirect income rules that referenced moved accounts.

The output is a complete World State, with no unresolved choices about controllers, ownership, or coverage.

## 12. Validation and Compilation

Validation establishes that the scenario conforms to the Type System (INV-17) and admits at least one consistent initialization (INV-21).

**Structural checks.** References exist and identifiers are unique. Units are compatible and every Resource declares its properties. Facilities declare inputs, outputs, capacity owners, and payees. Policy outputs use supported Intents. Scopes are well-defined. Every Actor declares capability, dependency, objective, and fallback. Schemes reference valid strata, members, domains, and estimators.

**Economic checks.** CostFunctions suit their mechanisms. Accounting matches conservation classes. The cycle reconciles and leaves no inert accumulation. Reserves and capacities are feasible. Financial claims have issuance and liability semantics. Required activity has an owner.

**Representation checks.** Eligibility matches population, membership, and domain. `statistical` estimators declare shrinkage, noise spread, and bound. `constituent` estimators declare a complete procedure. Accounting partitions consistently. No duplicate adjustments. No authority granted.

**Resolution consistency.** For every supported pair of explicitness configurations, total stocks, liabilities, capabilities and owners, and authorized recurring activity reconcile (INV-14). Later differences must come from decisions, not from duplicated resources, deleted liabilities, or altered rules.

**Feasibility.** Compatibility rules may restrict optional configurations; for example, reject removing an institution while also removing every mechanism for a liability it carries.

```purescript
validateGameboard
  :: Gameboard
  -> Either (Array ValidationError) ValidGameboard

instantiate
  :: ValidGameboard
  -> InstantiationConfig
  -> Either (Array InitializationError) InitialWorld
```

A validated Gameboard is reusable; each instantiation is still checked against its constraints.

## 13. Provenance and Versioning

Each Vertical references its research, and important parameters keep provenance. The Gameboard distinguishes researched facts, modeling simplifications, hypothetical conditions, and balancing choices (including estimator strengths). This matters when the game is used for serious scenario planning.

Every Gameboard has a stable identifier and immutable version (INV-22). Any change to Markets, Transformations, the cycle, Policies, populations, or relationships is a new version. Instantiated Games reference the exact version.

## 14. Illustrative Gameboard

A five-year scenario of US–China industrial competition, with geopolitical rivalry and capital–labor conflict as persistent tensions.

- **Verticals:** semiconductors, energy, finance, manufacturing, sharing Resources, Markets, and income flows.
- **Economy:** Energy trades in regional Market nodes linked by transport capacity, with tariffs available to governments. Defense procurement is a single global facility paying defense manufacturers. Each year producers generate Energy, workers receive wages, and governments collect revenue.
- **Entities:** US and Chinese governments, EU Commission, selected corporations, a labor federation, representative small-business owners. A shipping operator controls transport capacity; a labor federation can strike to reduce throughput. Aggregates include households, workforces, small businesses, producers, and a European bloc with a collective procedure.
- **Policies:** NPC Policies for eligible institutions; Aggregate Policies for consumption, investment, production, and bloc residual input.
- **Representations:** a small-business owner represents Energy demand through a `statistical` estimator; a member state submits procedure inputs to the bloc through a `constituent` estimator.
- **Instantiation options:** the EU may be human or NPC; small-business representatives may be present or absent. Both games reconcile to the same initial accounts and then diverge through decisions.

---

## 15. Summary

The Gameboard is a declarative specification of a playable economic world: Resources, Markets, facilities, a recurring cycle, populations, institutions, relationships, Policies, Proposal rules, and Representation schemes. Vertical reports ground it; mechanically consequential tensions make it worth playing.

It does not decide who is human-controlled, what anyone does, or how events unfold. Its responsibility is to define **a coherent, self-sustaining world that is economically identical at the outset across every supported resolution**.
