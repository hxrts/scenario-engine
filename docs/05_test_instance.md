# Scenario Planning Game — Reference Instance: The Fractured World

**Version:** 0.2  
**Status:** Experimental configuration; all scenario choices and numerical parameters are provisional  
**Scenario ID:** `fractured-world-v0`  
**Implementation language:** PureScript

---

## 1. Purpose

This document defines a concrete reference instance of the Scenario Planning Game, provisionally titled **The Fractured World**.

It exercises the architecture set out in the Type System, Gameboard, and Instantiated Game specifications. Unlike them, it makes specific choices about resources, participants, markets, facilities, circulation, policies, and rules. These choices are experimental; the engine does not require them.

Terms follow the Type System Glossary ([§2.5](02_type_system.md#25-glossary)). Invariants are cited by their IDs in the Type System master table ([§9](02_type_system.md#9-architectural-invariants-and-numerics)). Arithmetic follows the Type System numerics rule ([§9.1](02_type_system.md#91-numerics-and-randomness)).

**Changing this instance should normally mean changing data, not engine code.** If a mechanic cannot be expressed with the existing types, record that as a finding instead of adding a special case.

### 1.1 Test objectives

The instance should demonstrate that:

1. Fifteen seats and one NPC share one economic world through the same interfaces ([INV-01](02_type_system.md#inv-01)).
2. A government can be explicit while its country remains a member of an implicit Aggregate, without conflating membership, account ownership, and collective decision-making ([INV-08](02_type_system.md#inv-08)).
3. Representation (Small-Business Owner → Global SMEs) and collective procedures (Germany → EU) give small Actors and member states bounded influence ([INV-09](02_type_system.md#inv-09), [INV-03](02_type_system.md#inv-03)).
4. Four Resources with differing configurable semantics generate meaningful strategic interaction.
5. Recurring production, income, expenditure, and capacity renewal sustain five years of activity ([INV-05](02_type_system.md#inv-05)).
6. Prices respond to players and the implicit economy, and trade restrictions have identifiable mechanical effects.
7. Every Actor has a distinct capability, a dependency on others, and a measurable objective others can help or obstruct ([INV-19](02_type_system.md#inv-19)).
8. Policies respond to changing conditions without being rewritten during play ([INV-12](02_type_system.md#inv-12)).
9. Deals negotiated out of band enter as multi-signer Proposals and settle atomically once every required signer signs ([INV-10](02_type_system.md#inv-10)).
10. The same Gameboard supports different levels of explicitness while preserving resolution consistency ([INV-14](02_type_system.md#inv-14)).

---

# Part I — Type System Configuration

## 2. Resources

The instance uses exactly four Resources. All quantities are abstract game units, not empirical measures. Their differences are declared Resource properties, not hard-coded logic.

| Resource | Unit | Created / destroyed by | Transferability | Decay | Reservable |
|---|---|---|---|---|---|
| **Money** | M | Nothing (issuance disabled in v0), so it is conserved | Transferable and tradable | None | No |
| **Energy** | E | Production / consumption | Transferable and tradable | None | No |
| **Military** | Mil | Procurement / declared attrition | Government-to-government Transfer only, under procurement authority | None | Yes, by deployment |
| **Soft Power** | S | Campaigns and aid / spending and decay | Non-transferable | 10% per simulated year | No |

Any Resource can be escrowed at screening (Type System [§8.2](02_type_system.md#82-single-round-update-and-snapshot-discipline)). Energy is one undifferentiated commodity; geography is expressed through regional Markets and transport capacity rather than additional Resources. The game does not model tactical combat.

### 2.1 Economic state that is not a Resource

| Economic state | Owner | Purpose |
|---|---|---|
| Energy production capacity | Energy producers | Energy created each round |
| Transport capacity | SHIP | Capacity of the `energy-shipping` facility ([§3.2](#32-transport-capacity-and-trade-restrictions)): Energy moved between regions |
| Semiconductor capacity | CHIP | 15 units/round; backs procurement (1 unit per 2 Mil) and Energy capacity investment |
| Procurement capacity | DEF | Annual Military production |
| Rules in force (`Tariff`, `AccessBan`, `Withholding`) | Governments; the EU collective procedure; CHIP | Who may ship across regions and at what surcharge; withheld capacity |

## 3. Markets and Facilities

Both kinds of priced mechanism use the shared CostFunction primitive. Exchange Markets trade existing reserves; facilities create output only through a validated recipe and pay a declared payee ([INV-07](02_type_system.md#inv-07)).

### 3.1 Regional Energy Markets

| Market ID | Region | Home Entities | Money reserve | Energy reserve | Operational interval |
|---|---|---|---:|---:|---|
| `energy-atlantic` | Atlantic | US, DE, UK, BR, NG, ENCO, DEF, LABOR, SME, AID, EU, US_WORKERS, GLOBAL_SMES | 700 M | 350 E | −80 ≤ x ≤ 300 |
| `energy-asia` | Asia | CN, IN, JP, SA, CHIP, CN_PRIVATE, WORLD_HOUSEHOLDS | 500 M | 250 E | −80 ≤ x ≤ 200 |

SHIP trades on both nodes. `ENERGY_SUPPLIERS` offers into both according to its Policy. Every other Entity trades only on its home node; Energy reaches the other region only through a shipped cross-region Transfer ([§3.2](#32-transport-capacity-and-trade-restrictions)) or through SHIP's own trading on both nodes from its holdings, which is its arbitrage capability and needs no shipping order.

Both nodes use the same curve:

**p(x) = 2 + 0.02x**, **C(x) = 2x + 0.01x²**

where x is the node's cumulative net Energy sold. The initial price is 2 M/E. Each upper bound sits 50 E below the node's Energy reserve; at the lower bound a node pays out C(0) − C(−80) = 96 M, within both Money reserves ([INV-06](02_type_system.md#inv-06)).

**Orders** carry a quantity and an optional limit price (maximum average price for a purchase, minimum for a sale). Clearing follows Type System [§5.5](02_type_system.md#55-simultaneous-clearing), with each node's uniform price (C(x₀ + Δ) − C(x₀)) / Δ, or the current marginal price at zero net demand. When an operational bound or reserve binds, the binding side is filled pro rata. Affordability is checked at screening against escrow, not at clearing: a buy escrows limit price × quantity, or the node's upper-bound price × quantity when it has no limit, and a sell escrows its quantity.

Submission order never affects the result ([INV-11](02_type_system.md#inv-11)).

### 3.2 Transport capacity and trade restrictions

Cross-region Energy movement goes through one facility.

**Transformation ID:** `energy-shipping`

- **Capacity:** 60 E per round (SHIP's transport capacity); logistics investment adds capacity from the next round.
- **Cost:** flat 0.3 M per E (CostFunction base 0.3, slope 0), with no delay.
- **Payee:** SHIP.
- **Authority:** every shipping order requires SHIP's signature.

A cross-region Energy Transfer is valid only when it is bundled in the same Proposal with a `Transform energy-shipping` order of the same quantity, executed by the sender, with SHIP among the signers. SHIP prioritizes by choosing which Proposals to sign. Such a Proposal is all-or-nothing (Type System [§3.3](02_type_system.md#33-proposals-and-signatures)): if the shipments SHIP signs in a round total more than the capacity, every shipping Proposal of that round is rejected as a unit and recorded.

Governments restrict shipping by enacting Rules through `ChangeRule`, effective at the next round boundary:

| Restriction | Rule | Effect |
|---|---|---|
| **Shipment ban** | `AccessBan` | Shipping orders between the restricting and target jurisdictions' Entities are infeasible |
| **Import tariff** | `Tariff` | Adds t M/E (t ≤ 1.0) to shipping orders delivering into the restricting jurisdiction from the origin, paid to the restricting government |

Germany holds no tariff authority of its own; EU-wide restrictions belong to the EU collective procedure ([§12.1](#121-germany--eu-collective-procedure)). Because regional prices can diverge, restrictions produce measurable price gaps, shipping volumes, and tariff revenue.

### 3.3 Defense procurement (facility)

**Transformation ID:** `defense-procurement`

One global facility produces Military:

- **Capacity:** 30 Mil per round, backed by CHIP's 15 semiconductor units (1 unit per 2 Mil). Units CHIP withholds, or that are consumed by capacity investment, reduce capacity proportionally.
- **Cost:** **p(q) = 8 + 0.5q**, **C(q) = 8q + 0.25q²**, where q is total Military ordered this round. Every order settles at the uniform price C(Q)/Q.
- **Recipe:** each Mil also consumes 2 E.
- **Payee:** DEF for every buyer except CN, whose payments go to CN_PRIVATE (Chinese defense production). DEF passes 25% of its procurement revenue to US_WORKERS as variable wages.
- **Constraints:** buyers need procurement authority; Money and Energy are escrowed at submission at the full-capacity price (15.5 M/Mil); delivery is at the Prepare phase of the next round; over-subscription is allocated pro rata in whole units, remaining units by largest remainder, ties by canonical EntityId.

### 3.4 Public attention (facility)

**Transformation ID:** `public-attention`

- **Cost:** **p(q) = 3 + 0.2q**, **C(q) = 3q + 0.1q²**, where q is total Soft Power generated this round; uniform price C(Q)/Q.
- **Capacity:** 50 S per round globally, 10 S per Actor; over-subscription pro rata.
- **Payee:** WORLD_HOUSEHOLDS, standing in for the media sector.

## 4. Transformations

| Transformation | Inputs | Outcome | Payee |
|---|---|---|---|
| **Military Procurement** | Money + 2 E per Mil | Military delivered next round | DEF, or CN_PRIVATE for CN ([§3.3](#33-defense-procurement-facility)) |
| **Public Campaign** | Money | Soft Power | WORLD_HOUSEHOLDS |
| **Humanitarian Aid** | 10 M + 5 E | 4 S to the provider; the 5 E is consumed in relief; at most twice per Actor per round | WORLD_HOUSEHOLDS |
| **Military Deployment** (`military-deployment`, uncapped) | 1 Mil per unit, returned at commit | Reservation for the round; no loss | — |
| **Major Military Action** (`major-military-action`, uncapped, minimum 12) | 1 Mil per unit, 0.75 returned at commit | Reservation for the round; 25% attrition ([§13.3](#133-military-deployment)) | — |
| **Energy Shipping** | 0.3 M per E, paid by the sender; SHIP must sign | Makes a bundled cross-region Energy Transfer of the same quantity valid ([§3.2](#32-transport-capacity-and-trade-restrictions)) | SHIP |
| **Logistics Investment** | 10 M + 3 E (SHIP only) | +10 E/round `energy-shipping` capacity from next round | GLOBAL_SMES |
| **Energy Capacity Investment** | 12 M + 3 E + 1 semiconductor unit bought from CHIP by signed Proposal (producers only) | +2 E/round production capacity from next round | GLOBAL_SMES |
| **Strike** | 8 S + 5 M strike pay (LABOR only) | −25% ENCO production and procurement capacity next round | US_WORKERS |

Investments change future capacity, which is economic state; they never create Energy or Military directly.

## 5. Intents and Capabilities

The instance uses the four Intent categories of the Type System:

- **Trade:** a regional Energy order.
- **Transfer:** a one-way gift or payment of a transferable Resource; a cross-region Energy Transfer must be bundled with a shipping order ([§3.2](#32-transport-capacity-and-trade-restrictions)).
- **Transform:** procurement, campaigns, aid, deployment, shipping, investment, strikes. Each names the Entity that executes it.
- **ChangeRule:** enacting or repealing a Rule (`Tariff` for import tariffs, `AccessBan` for shipment bans, `Withholding` for CHIP's semiconductor units).

Every Intent travels in a Proposal. Deals negotiated between players are Proposals bundling each side's Transfers, signed by every party ([§13.5](#135-proposals-and-signatures)).

Government Actors hold only government-controlled resources and enact rules only within their jurisdictions. Corporations trade, transform, and allocate the capacities they own, but cannot regulate or deploy Military; DEF receives procurement revenue without gaining authority to conduct operations. Civil organizations and the small-business owner act through their own holdings, Soft Power, strikes, aid, signed deals, and Representation, and never acquire the authority of the populations they represent ([INV-03](02_type_system.md#inv-03)).

---

# Part II — Gameboard Configuration

## 6. Scenario: The Fractured World

### 6.1 Setting

A hypothetical near-future economy marked by intensifying geopolitical competition, regionally divided energy markets, and unequal access to industrial and military capability. The United States and China are the principal rivals. European governments must coordinate while keeping national priorities. Energy producers hold leverage over importers, and shipping links the Atlantic and Asian regions. Corporations control key capacities; workers, small businesses, and civil institutions try to influence more powerful Actors.

The scenario asks whether participants can preserve cooperation under conditions that reward competition.

### 6.2 Time horizon

Five simulated years, played as five one-year rounds. Other instantiations may use four to eight rounds; annual flows scale to round duration.

### 6.3 Central tensions

| Tension | Mechanism |
|---|---|
| **Geopolitical competition** | Procurement, deployment, China's conditional Policy, shipment bans |
| **Energy security** | Regional nodes, production and transport capacity, tariffs, the Round 3 disruption |
| **European coordination** | Germany's signatures on the EU's procedure Proposals |
| **Distributional conflict** | Wage, tax, and spending flows; DEF's variable wages; LABOR's strike; household and small-business exposure to Energy prices |

## 7. Economic Verticals

Four provisional Verticals, placeholders for research-backed vertical reports, share Resources, Markets, and capacities:

| Vertical | Resources and state | Mechanism |
|---|---|---|
| **Energy and Industry** | Energy, Money, production capacity, semiconductor capacity | Production, investment, industrial dependency |
| **Security and Defense** | Military, Energy, Money, procurement capacity | Procurement, deployment, attrition |
| **Finance and Trade** | Money, Energy, transport capacity, tariffs | Circulation, regional exchange, restrictions |
| **Information and Diplomacy** | Soft Power, Money | Campaigns, aid, strikes |

Procurement consumes Energy and depends on semiconductor capacity, so Energy scarcity constrains military capacity; capacity investment raises future supply at the cost of present Money, Energy, and chips. These effects arise from mechanisms, not scripted events.

## 8. Economic Circulation

Each round's single update includes the Type System's economic cycle (production, exchange, income and expenditure, capacity renewal), evaluated together with every other action (Type System [§8.2](02_type_system.md#82-single-round-update-and-snapshot-discipline)). All amounts are annual and provisional.

### 8.1 Energy production

Production is computed during economic resolution and delivered at the Prepare phase of the following round.

| Producer | Capacity (E/round) |
|---|---:|
| ENERGY_SUPPLIERS | 60 |
| SA | 25 |
| ENCO | 25 |
| BR | 13 |
| NG | 12 |
| IN | 5 |
| **Total** | **140** |

### 8.2 Operating Energy requirements

Consumed at commit, from each Entity's net Energy position (opening holdings plus this round's receipts, minus sales and other uses). This is evaluated on the batch result, not as a later step:

| Entity | E/round |
|---|---:|
| US, CN | 10 each |
| DE, IN, JP, EU | 8 each |
| UK, CHIP | 6 each |
| SHIP | 4 |
| **Explicit and bloc subtotal** | **68** |
| CN_PRIVATE, US_WORKERS, WORLD_HOUSEHOLDS, GLOBAL_SMES | Their adjusted purchase target (baseline 12 + 8 + 12 + 40 = 72) |

At baseline, production equals operating requirements: **140 E per round**. Procurement, aid, and investment draw on stocks beyond that, and the Round 3 disruption ([§13.6](#136-energy-disruption)) is the scenario's designed scarcity.

Aggregate market demand is 80 E per round (EU 8, CN_PRIVATE 12, US_WORKERS 8, WORLD_HOUSEHOLDS 12, GLOBAL_SMES 40), against `ENERGY_SUPPLIERS`' offer of 40 E. Market prices therefore rise unless the explicit producers (SA, ENCO, BR, NG, and IN, producing 80 E) sell roughly 40 E per round. This is deliberate: it is the producers' leverage.

**Shortfall rule:** each unit of unmet requirement reduces the Entity's fixed incoming transfers for the round ([§8.3](#83-fixed-income-and-expenditure)) by 2 M, which the payers keep.

### 8.3 Fixed income and expenditure

Money transfers included in every round update, funded from opening holdings:

| Flow | Payer → Payee | M/round |
|---|---|---:|
| **Taxes** | US_WORKERS → US | 15 |
| | GLOBAL_SMES → US | 5 |
| | CN_PRIVATE → CN | 20 |
| | EU → DE (German private-economy revenue, held in the EU residual) | 10 |
| | WORLD_HOUSEHOLDS → IN / JP / UK / BR / NG | 10 / 10 / 8 / 6 / 4 |
| **Public spending** | US → US_WORKERS | 12 |
| | CN → CN_PRIVATE | 18 |
| | DE → EU | 8 |
| | IN / JP / UK / BR / NG → WORLD_HOUSEHOLDS | 8 / 8 / 6 / 5 / 3 |
| **Wages** | ENCO → US_WORKERS | 6 |
| | DEF → US_WORKERS | 6 |
| | CHIP → WORLD_HOUSEHOLDS | 6 |
| | SHIP → WORLD_HOUSEHOLDS | 5 |
| | GLOBAL_SMES → WORLD_HOUSEHOLDS | 10 |
| | GLOBAL_SMES → US_WORKERS | 6 |
| **Sales and dues** | DEF → CHIP (semiconductor supply contract) | 6 |
| | US_WORKERS → GLOBAL_SMES | 12 |
| | WORLD_HOUSEHOLDS → GLOBAL_SMES | 12 |
| | US_WORKERS → SME | 3 |
| | WORLD_HOUSEHOLDS → SHIP | 4 |
| | US_WORKERS → LABOR (dues) | 3 |
| | EU → AID (donations) | 3 |

Net per round:

| Entity | Net M | Entity | Net M |
|---|---:|---|---:|
| US | +8 | ENCO | −6 |
| CN | +2 | DEF | −12 |
| DE, IN, JP, UK | +2 each | CHIP | 0 |
| BR, NG | +1 each | SHIP | −1 |
| SME, LABOR, AID, GLOBAL_SMES | +3 each | US_WORKERS | −3 |
| | | CN_PRIVATE | −2 |
| | | EU | −5 |
| | | WORLD_HOUSEHOLDS | −3 |
| **Total inflow** | **+32** | **Total outflow** | **−32** |

The schedule conserves Money. The corporations' negative fixed balances are offset by variable revenue. CHIP's supply contract covers its wages; further CHIP revenue comes from signed Proposals selling semiconductor units or compensating withholding.

### 8.4 Variable flows

| Flow | Rule |
|---|---|
| Energy sales | Sellers receive the node's uniform price |
| Producer recycling | ENERGY_SUPPLIERS pays 50% of its net Energy-sale revenue to WORLD_HOUSEHOLDS |
| Procurement | To DEF (25% passed to US_WORKERS) or CN_PRIVATE |
| Campaigns and aid | To WORLD_HOUSEHOLDS |
| Investment | To GLOBAL_SMES |
| Shipping and tariffs | Shipping fees to SHIP; tariffs to the imposing government |

### 8.5 Capacity renewal and reconciliation

Investment in round r changes capacity from the Prepare phase of round r + 1, recorded with its originating Intent.

At the end of every round the engine checks [INV-05](02_type_system.md#inv-05): total Money is unchanged; the change in total Energy equals production minus consumption by requirements, procurement, aid, and investment; the change in Military equals deliveries minus attrition; the change in Soft Power equals production minus spending minus decay.

## 9. Actor Catalog

The reference instantiation selects all fifteen seat roles plus China as an NPC.

| ID | Actor | Category | Region | Distinctive capability | Dependency | Objective ([§9.3](#93-objectives)) |
|---|---|---|---|---|---|---|
| US | US Head of State | Government | Atlantic | Tariffs and shipment bans; deployment; largest procurement budget | Energy above its holdings; procurement capacity | Military leadership, solvent |
| DE | German Chancellor | Government | Atlantic | Signs EU procedure Proposals; optional solidarity contributions | Atlantic Energy; tax revenue from the EU residual | Energy security |
| IN | Indian Prime Minister | Government | Asia | Energy production (5); tariffs | Asia Energy prices | Industrial autonomy |
| JP | Japanese Prime Minister | Government | Asia | Tariffs; procurement; large Asia purchasing | Imports and transport; no production | Energy security, credible defense |
| UK | UK Prime Minister | Government | Atlantic | Tariffs; procurement; campaigns | Atlantic Energy | Diplomatic influence |
| BR | Brazilian President | Government | Atlantic | Energy production (13); capacity investment | Semiconductor units for investment | Stronger finances |
| SA | Saudi Energy Minister | Government | Asia | Largest government production (25) | Transport for Atlantic sales | Energy wealth |
| NG | Nigerian President | Government | Atlantic | Energy production (12); capacity investment | Semiconductor units; investment | National capacity |
| ENCO | US Energy Consortium CEO | Corporation | Atlantic | Energy production (25); capacity investment | Semiconductor units; strikes | Profit and supply growth |
| CHIP | Semiconductor Producer CEO | Corporation | Asia | Owns 15 units/round of semiconductor capacity backing procurement; may withhold units or sell them to investors by signed Proposal | 6 E/round operating Energy | Secure Energy inputs |
| SHIP | Shipping Operator CEO | Corporation | Both | Owns the `energy-shipping` facility (60 E/round) and chooses which shipments to sign; arbitrage across both nodes | 4 E/round fuel; shipment bans | Open routes, high throughput |
| DEF | Defense Manufacturer CEO | Corporation | Atlantic | Operates procurement and receives non-Chinese revenue | CHIP's semiconductor capacity; strikes | Procurement revenue |
| LABOR | US Labor Federation Chair | Civil institution | Atlantic | Strike ([§4](#4-transformations)), declared or withheld each round | Dues; Soft Power | Workers' position and influence |
| SME | Small-Business Owner | Small participant | Atlantic | Representation of Global SME demand | Atlantic prices; small cash | Business viability |
| AID | Humanitarian Alliance Director | Civil institution | Atlantic | Aid; Energy relief to Aggregates in shortfall; Soft Power | EU donations; Energy purchases | Limit crisis damage |
| CN | Chinese Head of State | Government | Asia | Tariffs and shipment bans; deployment | Asia Energy prices; CN_PRIVATE tax base | Military parity, solvent |

### 9.1 Semiconductor capacity

CHIP owns 15 semiconductor units per round, which by default back the procurement facility's 30 Mil capacity. CHIP may enact a `Withholding` rule on some of its units, cutting procurement capacity proportionally from the next round; any compensation is a Transfer or signed Proposal. A producer's Energy capacity investment needs 1 unit, bought from CHIP by signed Proposal; each unit so consumed counts against the 15 and reduces that round's procurement capacity by 2 Mil.

### 9.2 Fallbacks

| Actor | Fallback when not explicit |
|---|---|
| US, CN | Required explicit; human or NPC controller |
| DE | Government accounts held in the EU Aggregate as member-government accounts; Germany's 0.20 weight folds into the EU's residual weight (0.60 solidarity, 0.68 trade) |
| IN, JP, UK, BR, SA, NG | `government-default-v0` controls the same accounts: baseline spending, no discretionary actions |
| ENCO, CHIP, SHIP, DEF | `firm-default-v0` controls the same accounts and capacities: keeps semiconductor capacity backing procurement and the DEF supply contract, sells surplus Energy; for SHIP, signs shipping Proposals up to capacity |
| LABOR | Implicit coverage: accounts merge into US_WORKERS; no strike capability |
| SME | Implicit coverage: accounts merge into GLOBAL_SMES; no Representation |
| AID | Implicit coverage: accounts merge into WORLD_HOUSEHOLDS, which also receives EU donations |

Initialization fails if any responsibility is unassigned ([INV-25](02_type_system.md#inv-25)).

### 9.3 Objectives

Each Actor's objective is data: measurable criteria over the recorded game (Gameboard [§10.5](03_gameboard.md#105-evaluation)). The criteria are provisional. They never affect the economy; they are evaluated from the manifest and round records and reported per Actor as met or not met, with values.

| Actor | Criteria |
|---|---|
| US | Final Military ≥ CN final Military; final Money ≥ 200 |
| CN | Final Military ≥ US final Military; final Money ≥ 200 |
| DE | No Energy shortfall in any round; final Money ≥ 110 |
| IN | Final production capacity ≥ 7 E/round; no shortfall in any round |
| JP | No shortfall in any round; final Military ≥ 36 |
| UK | Final Soft Power ≥ 47; final Money ≥ 110 |
| BR | Final Money ≥ 150 |
| SA | Final Money ≥ 250; final Energy ≥ 60 |
| NG | Final production capacity ≥ 14 E/round; final Money ≥ 90 |
| ENCO | Final Money ≥ 220; final production capacity ≥ 27 E/round |
| CHIP | No shortfall in any round; final Money ≥ 150 |
| SHIP | Cumulative Energy shipped ≥ 150 E; final Money ≥ 130 |
| DEF | Cumulative procurement revenue ≥ 300 M |
| LABOR | US_WORKERS final Money ≥ 260; final Soft Power ≥ 30 |
| SME | Final Money ≥ 15; GLOBAL_SMES shortfall in at most 1 round |
| AID | Total Aggregate shortfall across the game ≤ 20 E; final Soft Power ≥ 55 |

Military and Soft Power matter through these objectives (US, CN, JP; UK, LABOR, AID) and through the deployment and strike mechanics.

## 10. Aggregate Catalog

| ID | Aggregate | Residual accounts | Region | Baseline responsibility |
|---|---|---|---|---|
| EU | European Union | Non-explicit member economies and bloc accounts | Atlantic | Energy purchasing; collective decisions |
| CN_PRIVATE | Chinese Private Economy | Chinese activity outside the government | Asia | Industrial demand; Chinese defense production |
| US_WORKERS | US Workforce | Residual worker accounts | Atlantic | Household demand |
| GLOBAL_SMES | Global Small Businesses | Small businesses outside explicit ones | Atlantic | Energy demand; investment supply |
| ENERGY_SUPPLIERS | Other Energy Producers | Production not held by explicit Actors | Both | Energy supply |
| WORLD_HOUSEHOLDS | Other World Households | Residual household activity | Asia | Household demand; media and relief payee |

Each Aggregate is described by its members, its account ownership, and (for the EU only) its collective procedures. Memberships may overlap; accounts may not ([INV-08](02_type_system.md#inv-08)).

### 10.1 Germany and the EU

- **Membership.** Germany is listed in the EU's `members` whether or not it signs anything in a given round.
- **Account ownership.** Making the Chancellor explicit extracts only the German government's accounts. German households and firms stay in the EU residual, which is why German tax revenue flows EU → DE ([§8.3](#83-fixed-income-and-expenditure)). The EU also owns a **solidarity pool**, initially empty.
- **Collective procedures.** The EU's capabilities (solidarity interventions and EU-wide trade restrictions) are exercised by its two collective procedures, which Germany influences by signing the EU's procedure Proposals ([§12.1](#121-germany--eu-collective-procedure)).

Germany controls its own resources and national capabilities and influences EU decisions by declared weight; it never controls EU holdings or authority ([INV-03](02_type_system.md#inv-03)).

### 10.2 Small-business representation

The owner is a member of `GLOBAL_SMES`. Their holdings and direct transactions are excluded from the residual; their proposed decisions serve as bounded evidence through a Representation ([§12.2](#122-small-business-owner--global-smes-representation)).

## 11. Policy Catalog

Policies are declarative decision programs run by the shared interpreter (Type System [§4](02_type_system.md#4-policies)). Memory such as "the US conducted a major deployment last round" is read from World State and the public event record. All Policy orders remain subject to affordability and market limits.

### 11.1 China NPC Policy — `china-strategic-v0`

1. If the US conducted a major military action (≥ 12 Mil deployed) in the previous round, order 4 Mil from the procurement facility.
2. If the beginning-of-round `energy-asia` price exceeds 3.2 M/E, offer up to 12 E on that node.
3. If a US restriction targeting China was enacted in the previous round, enact the reciprocal restriction, effective at the next round boundary.
4. China's budget covers procurement and its discretionary Energy trades in the reference game. If a variant makes them exceed it, the engine scales China's own actions pro rata ([Type System §5.5](02_type_system.md#55-simultaneous-clearing)); a priority order between them would be a scenario-level addition.

The NPC does not improvise outside its declared Policy.

### 11.2 EU Aggregate Policy — `eu-coordination-v0`

- Buy 8 E per round on `energy-atlantic`; 4 E when the beginning-of-round price exceeds 3.0 M/E.
- Publish the solidarity and trade procedure Proposals when their conditions hold ([§12.1](#121-germany--eu-collective-procedure)).
- Pay the fixed flows in [§8.3](#83-fixed-income-and-expenditure).

### 11.3 Global Small-Business Policy — `global-smes-v0`

Baseline demand of 40 E per round on `energy-atlantic`, reduced by 20% when the beginning-of-round price exceeds 3.5 M/E. The target is that baseline multiplied by the factor stored in the previous round ([§12.2](#122-small-business-owner--global-smes-representation)), or the baseline alone when none was stored. The target is both the purchase order and the operating requirement.

### 11.4 Other Aggregate Policies

| Aggregate | Policy |
|---|---|
| CN_PRIVATE | Buys 12 E on `energy-asia`; 9 E above 3.0 M/E |
| US_WORKERS | Buys 8 E on `energy-atlantic`; 6 E above 3.0 M/E |
| WORLD_HOUSEHOLDS | Buys 12 E on `energy-asia`; 9 E above 3.0 M/E |
| ENERGY_SUPPLIERS | Produces 60 E. Offers 40 E (24 Atlantic / 16 Asia), or 50 E (30 / 20) at a node whose beginning-of-round price is at least 2.6 M/E. Retains unsold output |

Each household and industrial Aggregate's purchase target is also its operating requirement. `government-default-v0` and `firm-default-v0` exist only for fallbacks and are inactive in the reference instantiation.

## 12. Representation and Collective Procedures

Explicit Actors influence Aggregates through two distinct mechanisms. In neither does the engine create or change a Proposal.

| Mechanism | Instance | How it works |
|---|---|---|
| Collective procedure | `eu-solidarity`, `eu-trade` (held by the EU) | Weighted signing on the EU's own Proposal, decided and executed in the same round |
| Representation | `sme-energy-sample` | Shrunk, clamped deviation of a sampled representative, recorded as observation → estimate in round r, then proposed Intent → executed result in round r+1 ([INV-09](02_type_system.md#inv-09)) |

### 12.1 Germany → EU (collective procedure)

The EU Aggregate holds two collective procedures. Each is a weighted-signature rule on Proposals the EU's Policy publishes in the EU's name; no Representation or Estimator is involved.

| Procedure | Authorizes | Member weights | Residual weight | Threshold |
|---|---|---|---:|---:|
| `eu-solidarity` | Energy sales on `energy-atlantic` | DE 0.20 | 0.40 | 0.45 |
| `eu-trade` | `Enact Tariff` | DE 0.20 | 0.48 | 0.45 |

**Support = residual weight + the weights of the members who signed.** A procedure Proposal executes in the round it is published if support exceeds the threshold; otherwise it lapses. Germany supports by signing in the Sign step and declines by not signing. Each round's record lists the Proposal, the members who signed, the support, the threshold, and the outcome.

The EU's Policy decides when to publish ([§11.2](#112-eu-aggregate-policy--eu-coordination-v0)):

- **Solidarity Proposal:** sell up to 8 E on `energy-atlantic`, drawing first on the pool and then on its holdings. Published in any round whose beginning-of-round Atlantic price exceeds 3.0 M/E, provided the EU's reserves cover its operating requirement.
- **Trade Proposal:** enact a matching import `Tariff` of 0.5 M/E on cross-region Energy from the restricting jurisdiction, in force from the next round. Published in any round in which a US or Chinese restriction affecting the Atlantic region is in force.

| Procedure | Germany signs | Germany does not sign |
|---|---|---|
| `eu-solidarity` | 0.40 + 0.20 = 0.60: executes | 0.40: lapses |
| `eu-trade` | 0.48 + 0.20 = 0.68: executes | 0.48: executes |

Germany is pivotal for solidarity and has no veto on trade. Separately, Germany may Transfer Energy into the solidarity pool at any time; that is a plain Transfer, independent of signing.

This is a stylized procedure, not a claim about EU voting law.


### 12.2 Small-Business Owner → Global SMEs (Representation)

| Field | Value |
|---|---|
| Weight | 1 |
| Reference | 5 E per round |
| Shrinkage | 0.25 |
| Noise spread | 0.10 (ε uniform in [−0.10, +0.10], drawn from the game seed) |
| Deviation bound | ±50% |

The observation is the owner's validated, escrowed proposed quantity, not the executed fill.

**Factor (round r) = 1 + 0.25 × clamp(deviation + ε, −0.50, +0.50)**

**Target (round r+1) = Baseline (round r+1) × factor**

ε is drawn in the observation's round under the key `round-n / sme-energy-sample / SME / sampling / 0` (Type System [§9.1](02_type_system.md#91-numerics-and-randomness)). It is only drawn when the owner submits an observation.

| Proposed purchase (round r) | Deviation | Next-round target, noise-free (ε = 0) | Range over ε (baseline 40 E) |
|---:|---:|---:|---:|
| 4 E | −20% | 40 × 0.95 = 38 E | 37–39 E |
| 0 E | −100% | 40 × 0.875 = 35 E | 35 E (clamp absorbs the noise) |
| 10 E | +100% | 40 × 1.125 = 45 E | 45 E (clamp absorbs the noise) |

The same decision therefore moves the Aggregate by slightly different amounts in different games, but identically in any replay with the same game seed.

The stored factor replaces the next round's baseline exactly once. Round r itself uses the baseline, or the factor stored in round r−1. The owner's own purchase executes separately against their holdings in round r.

### 12.3 Incentives and bounds

The owner knows their choice moves the Aggregate, so it is a strategic choice, not an independent sample. That is intended; shrinkage and the clamp keep the influence explicit and bounded, and shrinkage is a balancing parameter, not a population estimate. The simulator reports the **Representation influence ratio**: next-round Aggregate change attributable to the owner divided by the owner's executed purchase. In the noise-free 4 E case, 2 E against 4 E gives 0.5; across ε it ranges from 0.25 to 0.75.

## 13. Scenario Rules

### 13.1 Round resolution

Each round is one simulated year and follows the Type System canonical sequence ([§8.1](02_type_system.md#81-canonical-resolution-sequence)). [§20](#20-phase-mapping) maps each phase to this instance. Prices produced by a round become observable in the next round.

### 13.2 Institutional changes

Tariffs, shipment bans, withholding, and strikes take effect at the next round boundary, giving other parties a negotiation window. A procedure Proposal is published, signed, and executed within one round; a tariff it enacts in round r is in force from round r+1.

### 13.3 Military deployment

A deployment reserves the Mil for the round and cannot be redeployed or transferred. A deployment of at least 12 Mil is a **major military action** for NPC rules and reporting. **Declared attrition (provisional):** a major action loses 25% of the deployed units; smaller deployments lose none. Survivors return at the end of the round.

### 13.4 Soft Power

Soft Power is produced by campaigns and aid, spent on strikes, and decays 10% per year. Final holdings count toward several objectives ([§9.3](#93-objectives)).

### 13.5 Proposals and signatures

Deals negotiated out of band enter as Proposals whose required signers are every party (Type System [§3.3](02_type_system.md#33-proposals-and-signatures)). A Proposal settles only when all of them sign in the Sign step; otherwise it lapses ([INV-10](02_type_system.md#inv-10)).

### 13.6 Energy disruption

The only scheduled event: at the Prepare phase of Round 3, a logistics disruption cuts `ENERGY_SUPPLIERS`' maximum offer by 20 E (12 Atlantic, 8 Asia). Production continues; unreleased output accumulates in its holdings. It is predeclared configuration, not an AI intervention.

### 13.7 Evaluation

Actor objectives ([§9.3](#93-objectives)) plus the system metrics in [§30](#30-measurements). No single winner; outcomes are not reduced to Money.

---

# Part III — Instantiated Game Configuration

## 14. Game Settings

| Parameter | Value |
|---|---|
| Game ID | `fractured-world-test-01` |
| Gameboard | `fractured-world-v0` |
| Seats / NPC Actors / Aggregates | 15 / 1 (China) / 6 |
| Horizon and rounds | 5 years; 5 rounds of 1 year |
| Real-world session | About 6 hours |
| Game seed | `26021` (the only in-world seed; its only use in v0 is SME sampling noise) |
| Player seed (simulation only) | `1` by default for reference simulation runs; varies seat-Policy randomness only |
| Reports | Fictional financial-news front page and Actor balance sheets |

### 14.1 Seat assignments

Seats 1–15 drive, in order: US, DE, IN, JP, UK, BR, SA, NG, ENCO, CHIP, SHIP, DEF, LABOR, SME, AID. In live play each seat's Policy is a human; in simulation it may be an AI agent or a preset program. Rebinding a seat requires no Gameboard change.

China is instantiated as Actor `CN` with controller `NPC china-strategic-v0`; it has no seat.

Germany is a seat; the EU is an Aggregate under `eu-coordination-v0` with Germany among its members and both collective procedures active. The `sme-energy-sample` Representation is active. No human controls the EU.

### 14.2 Resolution consistency

With Germany implicit, the German government's accounts sit in the EU Aggregate as member-government accounts ([§9.2](#92-fallbacks)). Before decisions differ, totals must reconcile ([INV-14](02_type_system.md#inv-14)):

| Account | Germany explicit (DE + EU) | Germany implicit (EU) |
|---|---:|---:|
| Money | 125 + 320 = 445 | 445 |
| Energy | 38 + 125 = 163 | 163 |
| Military | 24 + 50 = 74 | 74 |
| Soft Power | 45 + 75 = 120 | 120 |

In the implicit configuration the EU → DE tax and DE → EU spending flows become internal and net to zero, DE's 8 E requirement becomes part of the EU's (16 E total), Germany's 0.20 weight is part of the EU's residual weight (0.60 for `eu-solidarity`, 0.68 for `eu-trade`), and every capability keeps the same owner.

## 15. Initial Holdings

**All values are invented balancing parameters, not empirical estimates.**

| Entity | Money | Energy | Military | Soft Power |
|---|---:|---:|---:|---:|
| US | 260 | 70 | 95 | 60 |
| Germany | 125 | 38 | 24 | 45 |
| India | 180 | 65 | 65 | 35 |
| Japan | 150 | 40 | 36 | 48 |
| UK | 130 | 30 | 38 | 47 |
| Brazil | 105 | 85 | 25 | 32 |
| Saudi Arabia | 155 | 110 | 32 | 28 |
| Nigeria | 85 | 60 | 18 | 24 |
| US Energy Consortium | 140 | 140 | 0 | 20 |
| Semiconductor Producer | 135 | 55 | 0 | 30 |
| Shipping Operator | 110 | 65 | 0 | 25 |
| Defense Manufacturer | 110 | 30 | 5 | 18 |
| US Labor Federation | 30 | 4 | 0 | 42 |
| Small-Business Owner | 15 | 5 | 0 | 12 |
| Humanitarian Alliance | 45 | 12 | 0 | 55 |
| **China (NPC)** | **255** | **95** | **88** | **57** |
| *EU* | *320* | *125* | *50* | *75* |
| *Chinese Private Economy* | *210* | *85* | *0* | *25* |
| *US Workforce* | *260* | *60* | *0* | *45* |
| *Global Small Businesses* | *300* | *80* | *0* | *25* |
| *Other Energy Producers* | *200* | *240* | *0* | *22* |
| *Other World Households* | *360* | *90* | *0* | *35* |

Aggregates are in italics. Government holdings cover only government-controlled resources. The German government's holdings are excluded from the EU and German private accounts are included; the owner's holdings are excluded from Global SMEs; China's government and private economy are separate; ENCO's holdings and capacity are excluded from Other Energy Producers ([INV-08](02_type_system.md#inv-08)). The solidarity pool starts at 0 E within EU holdings.

## 16. Initial Market and Capacity State

| Mechanism | Initial state |
|---|---|
| `energy-atlantic` | x = 0; 700 M and 350 E; 2 M/E |
| `energy-asia` | x = 0; 500 M and 250 E; 2 M/E |
| Defense procurement | 0 orders; 30 Mil capacity |
| Public attention | 0 activity; 50 S capacity; 10 S per Actor |
| `energy-shipping` | 60 E/round capacity; 0.3 M/E |
| Semiconductor capacity | 15 units/round, all backing procurement |
| Production capacity | 140 E/round ([§8.1](#81-energy-production)) |
| Rules | No `Tariff`, `AccessBan`, or `Withholding` in force |

Market reserves are distinct from Entity holdings. Facilities pay their payees directly.

## 17. Additional Parameters

| Parameter | Value |
|---|---|
| EU member weight and thresholds | DE 0.20 in both procedures; support must exceed 0.45 |
| EU residual weights | Solidarity 0.40; trade 0.48 |
| EU matching tariff | 0.5 M/E |
| SME Representation | Weight 1, reference 5 E, shrinkage 0.25, noise spread 0.10, bound ±50% |
| Global SME baseline demand | 40 E/round |
| Major action threshold and attrition | 12 Mil; 25% |
| Procurement delivery delay | 1 round |
| Soft Power decay | 10% per year |
| Energy disruption | Round 3, −20 E (−12 Atlantic / −8 Asia) |
| Producer recycling | 50% of ENERGY_SUPPLIERS net sale revenue |
| DEF variable wage share | 25% |
| Maximum import tariff | 1.0 M/E |
| Shortfall penalty | 2 M per E unmet |

None requires a Type System change.

## 18. Round Schedule

| Round | Year | Central test |
|---|---|---|
| 1 | 1 | Market discovery, diplomacy, allocation |
| 2 | 2 | Policy responses, first deliveries, investment, first signed deals |
| 3 | 3 | Energy disruption and strategic stress |
| 4 | 4 | Adaptation, military competition, recovery or decline |
| 5 | 5 | Final decisions and resolution |

Themes indicate opportunities, not mandatory events.

## 19. Session Structure

A provisional six-hour session: 20 minutes of briefing, five 60-minute rounds (about 40 minutes negotiation; 7 minutes oral announcements with confirmation of ambiguous Intents, ending at the propose deadline; 3 minutes signing, ending at the sign deadline; 10 minutes resolution and reporting), 20 minutes closing analysis, and 20 minutes of breaks.

## 20. Phase Mapping

| Canonical phase | Fractured World content |
|---|---|
| **1. Prepare round** | Deliver production and procurement; apply capacity renewals; activate rule changes and strikes; apply the Round 3 disruption |
| **2. Create observations** | Authorized views, including both regional prices and the SME factor stored last round |
| **3. Generate proposals** (round runner) | 3a Propose: all Policies at once (seats, with human text input interpreted in live play; China's Policy; Aggregate Policies, applying last round's stored factor; the EU publishes its procedure Proposals when their conditions hold). 3b Sign: each other required signer signs the Proposals it endorses, including Germany on EU procedure Proposals |
| **4. Screen and interpret** | Authority, signature rules (an incompletely signed deal lapses; a procedure Proposal lapses unless support exceeds its threshold), and escrow; the owner's observation |
| **5. Estimate** | Compute and store the SME adjustment factor for use next round; no Proposal is created or changed |
| **6. Resolve economy** | One batch: joint feasibility against escrow; uniform clearing of both nodes; shipping, procurement, campaigns, aid, deployment, investment, and circulation evaluated together; operating requirements and decay applied to net positions at commit |
| **7. Commit and record** | New World State, events, diagnostics, balance sheets; AI front page from recorded events |

---

# Part IV — Playtest Fixtures

Unless stated otherwise, fixtures disable circulation, production, and operating requirements to isolate the mechanism under test. All amounts follow the 4-decimal, half-to-even rule; every price below is exact at that precision.

## 21. Fixture A: Direct Market Exchange

`energy-atlantic` starts at x = 0. The US buys 10 E; Germany sells 5 E. Net demand is 5 E.

**C(5) − C(0) = 2 × 5 + 0.01 × 25 = 10.25 M**, uniform price 10.25 / 5 = **2.05 M/E**.

| Entity | Money before | Money after | Energy before | Energy after |
|---|---:|---:|---:|---:|
| US | 260 | 239.50 | 70 | 80 |
| Germany | 125 | 135.25 | 38 | 33 |
| `energy-atlantic` | 700 | 710.25 | 350 | 345 |

Money totals 1,085 before and after; Energy totals 458 before and after.

**A2 (limit price).** The US adds a limit of 2.04. At the trial price 2.05 its order is removed. Net demand becomes −5 E: price = (C(0) − C(−5)) / 5 = (0 − (−9.75)) / 5 = **1.95 M/E**. Germany receives 9.75 M; the US order is recorded as rejected.

Verifies [INV-04](02_type_system.md#inv-04), [INV-06](02_type_system.md#inv-06), [INV-11](02_type_system.md#inv-11); limit handling.

## 22. Fixture B: Germany and the EU (collective procedure)

**B1: contribution, normal prices.** The Atlantic price is at or below 3.0, so the EU publishes no solidarity Proposal. Germany transfers 10 E to the pool. DE Energy 38 → 28; EU 125 → 135, pool 10 E.

**B2: Germany signs, high prices.** Atlantic price above 3.0; the EU publishes the solidarity Proposal and Germany signs it, transferring nothing. Support = 0.40 + 0.20 = 0.60 > 0.45, so the EU's offer of up to 8 E enters this round's clearing.

**B3: Germany does not sign, high prices.** Support = 0.40; the Proposal lapses and the round resolves.

**B4: `eu-trade`.** A US shipment ban on China is in force, so the EU publishes the tariff Proposal. Support is 0.68 with Germany's signature and 0.48 without ([§12.1](#121-germany--eu-collective-procedure)); either way it passes and the tariff is in force next round.

Verifies that signing and transfers are independent, that a procedure Proposal executes only when support exceeds its threshold, and that Germany never controls EU holdings or authority ([INV-03](02_type_system.md#inv-03), [INV-08](02_type_system.md#inv-08), [INV-10](02_type_system.md#inv-10)).

## 23. Fixture C: Small-Business Owner (Representation)

[Fixtures C1](#23-fixture-c-small-business-owner-representation)–C4 set the noise spread to 0 so their values are exact. C5 checks the seeded noise.

In round r the owner submits a purchase of 4 E (reference 5 E). Deviation −20%, within the clamp, so the stored factor is 0.95. Global SMEs uses its 40 E baseline in round r. In round r+1 its target is 40 × 0.95 = **38 E**, replacing the 40 E baseline. The owner's 4 E executes separately in round r.

- **C2 (clamp):** owner proposes 0 E → clamped to −50% → factor 0.875 → 35 E next round.
- **C3 (proposed vs executed):** owner proposes 4 E with a limit violated at clearing. The observation is still 4 E and the next-round target is 38 E; the record shows 4 E proposed, 0 E executed.
- **C4 (high price):** when the round r+1 price is above 3.5 M/E the baseline is 32 E; the 0.95 factor from a 4 E observation gives 32 × 0.95 = 30.4 E.
- **C5 (seeded noise):** noise spread 0.10, game seed `26021`. The next-round target for a 4 E observation lies in [37, 39] E and equals 40 × (1 + 0.25 × (−0.20 + ε)) for the recorded draw. Replaying with the same seed gives the identical target. A different seed generally gives a different target in the same range. A 0 E proposal still gives exactly 35 E.

Verifies bounded extrapolation, separate ownership, and no double-counting ([INV-08](02_type_system.md#inv-08), [INV-09](02_type_system.md#inv-09)).

## 24. Fixture D: China Responds as an NPC

**Round 1.** The US deploys 12 Mil (major action). Attrition 25% × 12 = 3; 9 Mil return at round end. US Military 95 → 92.

**Round 2.** China observes the recorded action and orders 4 Mil; no other orders.

- Cost C(4) = 32 + 4 = **36 M**, uniform price 9 M/Mil; Energy 8 E.
- China: Money 255 → 219; Energy 95 → 87.
- Payee CN_PRIVATE: 210 → 246.

**Round 3 Prepare.** 4 Mil delivered: China 88 → 92.

Verifies conditional Policy execution, cross-round observation, reservation and attrition, delayed delivery, and payment to the payee ([INV-05](02_type_system.md#inv-05), [INV-12](02_type_system.md#inv-12)).

## 25. Fixture E: Scarcity and Trade Restriction

[Fixture E](#25-fixture-e-scarcity-and-trade-restriction) is qualitative: it has no fixed expected values and passes on reconciliation, solvency, and the expected propagation below.

With full circulation enabled, the Round 3 disruption reduces supplier offers by 12 E (Atlantic) and 8 E (Asia). Production continues and accumulates in supplier holdings. With sufficient demand, prices rise at both nodes and their gap widens if transport is constrained.

Expected propagation: operating shortfalls and penalties; capacity investment and procurement (both consume Energy); household and SME demand; SHIP arbitrage; China's Asia offer rule; the EU's solidarity Proposal, with Germany's pivotal signature, executed in the same round.

**E2 (restriction).** In Round 2 the US enacts a shipment ban on China, effective Round 3. China reciprocates in Round 3, effective Round 4. With the ban in force in Round 3, the EU publishes and passes its tariff Proposal in Round 3, in force from Round 4 ([Fixture B4](#22-fixture-b-germany-and-the-eu-collective-procedure)). Compare regional price gaps, shipping volumes, and tariff revenue against the unrestricted run.

Every consequence must be explainable from recorded transitions, and reconciliation must pass every round ([INV-05](02_type_system.md#inv-05), [INV-06](02_type_system.md#inv-06)).

## 26. Fixture F: Simultaneous Conflicting Orders

The US, UK, and Japan order 15, 8, and 9 Mil: 32 against a capacity of 30. Escrow at 15.5 M/Mil: US 232.5, UK 124, JP 139.5, all within their Money.

**Allocation:** 30/32 = 0.9375 gives 14.0625, 7.5, 8.4375. Floors 14 + 7 + 8 = 29; the remaining unit goes to the largest remainder (UK, 0.5). Fills: **US 14, UK 8, JP 8**.

**Cost:** C(30) = 240 + 225 = **465 M**, uniform price 15.5 M/Mil.

| Buyer | Mil | Money | Energy |
|---|---:|---:|---:|
| US | 14 | 217 | 28 |
| UK | 8 | 124 | 16 |
| JP | 8 | 124 | 16 |
| **Total** | **30** | **465** | **60** |

DEF receives 465 M and passes 116.25 M to US_WORKERS. Unfilled escrow (US 1 Mil, JP 1 Mil) is released. Reversing submission order gives identical fills.

Repeat with Energy demand exceeding a node's operational bound, and with one Actor's actions exceeding its Money.

Verifies joint feasibility, deterministic allocation, and order independence ([INV-06](02_type_system.md#inv-06), [INV-11](02_type_system.md#inv-11)).

## 27. Fixture G: Proposals and Signatures

SA and JP agree out of band on 20 E for 48 M. Both are on the Asia node. In Round 3:

- **3a Propose:** SA publishes a Proposal with intents [Transfer 20 E SA → JP, Transfer 48 M JP → SA], signers [SA, JP].
- **3b Sign:** JP signs (SA's signature is implicit in publishing). Each side's giving leg is escrowed at screening.

The Proposal settles atomically in the round batch, assuming no other balance changes:

| Entity | Money before | Money after | Energy before | Energy after |
|---|---:|---:|---:|---:|
| SA | 155 | 203 | 110 | 90 |
| JP | 150 | 102 | 40 | 60 |

- **G2 (unsigned):** JP does not sign. The Proposal lapses, balances are unchanged, and the rest of the round resolves.
- **G3 (cross-region):** the same deal with Germany. SA proposes [Transfer 20 E SA → DE, Transfer 48 M DE → SA, Transform `energy-shipping` 20 by SA], signers [SA, DE, SHIP]. The shipping fee is 0.3 × 20 = 6 M, paid by SA to SHIP: SA 155 → 197 M, 110 → 90 E; DE 125 → 77 M, 38 → 58 E; SHIP 110 → 116 M. Under an `AccessBan` covering the route the Proposal is fully signed but the shipping order is infeasible, so it is rejected as a unit and recorded while the rest of the round resolves. If SHIP signs shipments totalling more than 60 E in a round, all of that round's shipping Proposals are rejected.
- **G4 (out-of-band strike deal):** LABOR, ENCO, and DEF agree off-system that LABOR will not strike. In Round 2 LABOR declares no strike and ENCO and DEF each Transfer 10 M to LABOR. The engine sees ordinary Transfers; nothing binds LABOR in later rounds.
- **G5 (three-party):** SA proposes [Transfer 20 E SA → JP, Transfer 48 M JP → SA, Transfer 10 M JP → SHIP (service fee)], signers [SA, JP, SHIP]. JP and SHIP sign: SA 155 → 203 M, 110 → 90 E; JP 150 → 92 M, 40 → 60 E; SHIP 110 → 120 M. If SHIP does not sign, the whole Proposal lapses and no balance changes.

Verifies signature completeness, lapsing, atomic settlement of bundled Intents, and that the engine enforces no promises ([INV-10](02_type_system.md#inv-10)).

---

# Part V — Balancing and Iteration

## 28. Tunable Parameters

| Category | Parameters |
|---|---|
| Starting power | Holdings and capabilities |
| Energy scarcity | Production, requirements, node reserves, curve slope, supplier offers |
| Circulation | Taxes, spending, wages, recycling |
| Geography | Shipping capacity and fee, tariff ceiling, regional assignments |
| Military escalation | Procurement cost and capacity, attrition, thresholds |
| Industrial dependency | Semiconductor capacity and prices |
| Labor leverage | Strike cost and effect |
| Soft Power | Campaign costs, decay, strike cost |
| NPC behavior | China thresholds and priorities |
| EU coordination | Member weights, residual weights, thresholds |
| Small-business Representation | Reference, shrinkage, noise spread, bound |
| Temporal pressure | Round count, delays, event timing |
| Objectives | Criteria and thresholds ([§9.3](#93-objectives)) |

Vary one parameter at a time from the baseline before combining changes.

**Baseline simulation results.** The simulator's variant experiments ([Simulator §11](06_simulator.md#11-experiments), `packages/harness/test/Test/Harness/Experiments.purs`) leave the parameters above unchanged. They pass every invariant in every variant, and they bear on the gameplay questions of [§29](#29-gameplay-questions) as follows:

- *Seat presets.* `Conservative` only buys below 2.5 M/E while holding under 30 E. Prices pass 2.5 by Round 2, so twelve of the fifteen seats never act under it, and the agency report finds those seats unmeasurable against `Passive`. Against an active baseline (`RandomValid`), every seat has a measurable effect. Agency is therefore analysed against an active baseline.
- *Small players (Q1).* LABOR, SME, and AID move prices and other Actors' holdings measurably. The SME Representation is weak at these values: the shrinkage sweep 0–0.5 moves the final Atlantic price by at most 0.0001 M/E, and game seeds 1–3 give results identical to 4 dp. Reference, shrinkage, and bound are the levers to raise its influence in playtesting.
- *Markets (Q3).* The Round 3 disruption raises the Atlantic price monotonically with the cut (2.75 → 2.86 → 3.08 M/E for 0, 12, 24 E). Supplier offers trade volume against price as expected.
- *Escalation and coordination (Q2, Q4).* No reference preset enacts a major military action or a tariff, so the major-action threshold and the EU matching tariff have no effect under the presets. They are exercised by Fixtures D and E and by Test 11.
- *Circulation (Q5).* Money keeps moving. The largest holder's share of Money rises from 0.098 to at most 0.145 over five rounds in every variant.

## 29. Gameplay Questions

1. **Are small players consequential?** Do SME, LABOR, and AID decisions affect larger Actors, and does the influence ratio stay in range?
2. **Are large Actors too powerful?** Do Energy, semiconductor, transport, and procurement dependencies create leverage for others?
3. **Do Markets produce interesting choices?** Do regional prices and restrictions encourage trade, coordination, and investment without making Markets unusable?
4. **Are implicit Policies credible?** Do Aggregates and the NPC respond coherently without improvisation?
5. **Does the economy stay functional for five years?** Does Money keep moving rather than settling in reserves?
6. **Are results understandable?** Can players trace balance-sheet changes to actions, signed deals, and rules?
7. **Is five rounds enough?** Do early decisions and investments matter later?

## 30. Measurements

Recorded each round; dashboard metrics are versioned projections of snapshots and events ([INV-31](02_type_system.md#inv-31)).

| Measurement | Purpose |
|---|---|
| Node prices and inter-regional gap | Scarcity, volatility, restriction effects |
| Energy traded, shipped, unfilled | Liquidity and shipping |
| Money by Entity and in Market reserves | Concentration and trapped Money |
| Production, consumption, shortfalls | Stock-flow balance |
| Procurement, deployment, attrition | Escalation |
| Soft Power created, spent, decayed | Influence mechanics |
| Semiconductor units withheld or sold, and capacity effect | Industrial dependency |
| EU support and outcome; SME baseline vs adjusted | Procedure and Representation effects |
| Representation influence ratio | Representation balance |
| Rejected and partial actions | Constraints |
| NPC actions and triggers | Policy behavior |
| Reconciliation results | Economic consistency |
| Proposals published, signed, and lapsed | Player interaction |
| Per-Actor agency (counterfactual change) | Consequential choices |
| Objective criteria met per Actor | Evaluation ([§9.3](#93-objectives)) |

## 31. Configuration Package

The instance is a versioned configuration package containing no new foundational types; tuning it changes data, not engine code:

```text
test-instances/fractured-world/v0/
  scenario  verticals/  resources  markets  transformations  circulation
  entities  capabilities  objectives  relationships  policies  representations
  game-rules  instantiation  initial-conditions  test-fixtures/
```

Example version boundaries: a revised cost curve or circulation flow is a Gameboard change; a different initial node reserve within the permitted range is an Instantiation change; new player assignments need no engine change.

---

# Part VI — Acceptance Criteria

## 32. Minimum Viable Test

The first complete test succeeds when the engine can:

1. Initialize fifteen seats, the China NPC, and six Aggregates with consistent ownership, capabilities, and fallbacks. *Proved by:* scenarios: *validates*, *instantiates with the initial holdings of §15*, *instantiates the market and capacity state of §16*.
2. Keep Germany explicit and an EU member without duplication, and reconcile against the Germany-implicit configuration. *Proved by:* scenarios: *reconciles Germany explicit against Germany implicit (§14.2)*; harness: *12 Resolution consistency*.
3. Trade on both nodes with limit prices and partial fills, and ship across regions through the shipping facility under tariffs and bans. *Proved by:* Fixtures A, A2, F (golden); harness: *4 Market exchange*, *9 Conflicting actions*, *11 Trade restriction*.
4. Run procurement as a facility with Energy and semiconductor inputs, delayed delivery, and payment to payees. *Proved by:* Fixture D (golden); handlers: *Fixture D end to end*; harness: *7 NPC response*.
5. Treat deployment as a reservation with declared attrition. *Proved by:* Fixtures D and D-minor (golden).
6. Produce, spend, and decay Soft Power. *Proved by:* harness: *Soft Power is produced, spent, and decays*.
7. Run the circulation cycle and pass reconciliation every round. *Proved by:* Fixture *circulation* (golden); harness: *1 Full game*, *8 Scarcity*; INV-05 asserted every round.
8. Generate China's conditional behavior and baseline Aggregate behavior. *Proved by:* policy: *china-strategic-v0 procures after a major US action*; harness: *7 NPC response*.
9. Execute EU procedure Proposals in the round they are published when weighted signatures exceed the threshold, without transferring EU authority. *Proved by:* Fixtures B1–B4 (golden); harness: *5 Germany and EU*.
10. Store the bounded Representation factor and apply it in the following round, recording proposed and executed actions. *Proved by:* Fixtures C1–C3, C5 and *C4* (golden); harness: *6 SME Representation*.
11. Settle fully signed Proposals atomically and let incompletely signed ones lapse. *Proved by:* Fixtures G1–G5 (golden); harness: *10 Signed Proposals*.
12. Resolve simultaneous actions deterministically and order-independently. *Proved by:* golden: *order independence*; harness: *3 Submission order*.
13. Execute five years and produce a replayable history and structured events for reporting. *Proved by:* harness: *2 Replay*; adapters: *13 Persistence*, *a scripted live session equals replaying…*.
14. Evaluate every Actor's objective criteria from the records. *Proved by:* harness: *14 Agency*.

The test does not need realistic macroeconomic forecasting; its objective is coherent, consequential, explainable interaction.

## 33. Summary

*The Fractured World* is a provisional configuration of the abstract architecture, not an extension of it. It defines four Resources, two regional Energy Markets, three facilities sharing the CostFunction primitive, a balanced circulation schedule, fifteen seats with distinct capabilities, dependencies, and measurable objectives, one NPC China, and six Aggregates.

Germany influences EU decisions by signing the EU's procedure Proposals, which execute in the same round, while German households and firms remain in the EU residual. The small-business owner influences Global SME demand through a bounded Representation, applied in the following round. Scarcity comes from the scheduled disruption and player choices, not from incomplete accounting.

Every parameter here should be changeable while the same Type System, Gameboard machinery, and runtime continue to be used.
