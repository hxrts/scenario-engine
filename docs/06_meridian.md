# Reference PremiseComposition: Meridian

**Status:** Experimental configuration.  
**PremiseComposition ID:** `meridian`, version `v0`

## 1. Purpose

Meridian is the provisional reference PremiseComposition, made of five Premises in order: `meridian-energy`, `meridian-shipping`, `meridian-defence`, `meridian-soft-power` and `meridian-actors`, each at version `v0` ([03 §2](03_premise.md#2-premises-and-compositions)). It describes one set of possible Worlds, and each game played from it is a Run. It gives concrete values to every choice that the [kernel](02_kernel.md), the [Premise format](03_premise.md) and the [Run](04_run.md) leave open. The golden fixtures and test suites run against it, so each value is also a check. The anchors are defined in [01 Terms](01_overview.md#terms), and invariants are cited by ID.

# Part I: Economy

## 2. Resources

Meridian has four circulating Resources, all abstract game units. Energy is one commodity. Geography enters through regional Exchanges and transport capacity. There is no tactical combat.

| Resource | Unit | Transferability | Decay | Reservable | Purpose |
|---|---|---|---|---|---|
| **Money** | M | Free. No Transformation creates it in v0 | None | No | The numeraire. Pays for Energy, Military and services |
| **Energy** | E | Free | None | No | The scarce commodity behind prices and procurement |
| **Military** | Mil | Restricted. Transferred by Actors holding the transfer capability (the governments) | None | Yes, by deployment | Deployable force. Created by procurement, lost in attrition |
| **Soft Power** | S | Non-transferable | 10% per simulated year | No | Influence from campaigns and aid, spent on strikes |

Capacities are non-transferable Resources with no Exchange. They limit what an Actor can produce or do in a round.

## 3. Exchanges and Transformations

### 3.1 Regional Energy Exchanges

<!-- check-docs: exchanges -->

| Exchange ID | Region | Actors with access | Money reserve | Energy reserve | Operational interval |
|---|---|---|---:|---:|---|
| `energy-atlantic` | Atlantic | US, DE, UK, BR, NG, ENCO, DEF, LABOR, SME, AID, EU, EU_POOL, US_WORKERS, GLOBAL_SMES, SHIP, ENERGY_SUPPLIERS | 700 M | 350 E | −80 ≤ x ≤ 300 |
| `energy-asia` | Asia | CN, IN, JP, SA, CHIP, CN_PRIVATE, WORLD_HOUSEHOLDS, SHIP, ENERGY_SUPPLIERS | 500 M | 250 E | −80 ≤ x ≤ 200 |

SHIP trades on both nodes, and `ENERGY_SUPPLIERS` offers into both. Other Actors reach one node. Energy crosses regions only by a shipped Transfer ([§3.3](#33-transformations-in-brief)).

Both nodes use one curve: **p(x) = 2 + 0.02x**, **C(x) = 2x + 0.01x²**, where x is the node's cumulative net Energy sold. The initial price is 2 M/E. Each upper bound leaves 50 E of the node's Energy reserve unsold. At the lower bound a node pays out C(0) − C(−80) = 96 M, which its Money reserve covers ([INV-06](02_kernel.md#inv-06)).

### 3.2 Defence-goods Exchange

The `defence-goods` Exchange trades Military for Money, so a deployment and a sale compete for the same units ([§13.3](#133-military-deployment)). It is an ordinary Exchange ([02 §5.2](02_kernel.md#52-exchanges)). Its access follows the trade capability held by the US and its allies.

<!-- check-docs: defence-exchange -->

| Exchange ID | Base / quote | Money reserve | Military reserve | Access | Operational interval |
|---|---|---:|---:|---|---|
| `defence-goods` | Mil / M | 300 M | 60 Mil | US, DE, UK, JP | −80 ≤ x ≤ 20 |

**p(x) = 3 + 0.01x**, **C(x) = 3x + 0.005x²**, where x is the Exchange's cumulative net Military sold. The initial price is 3 M/Mil and the price runs from 2.2 to 3.2 across the interval. The upper bound sits 40 Mil below the reserve, and the lower bound is −80. A Military sale is a Trade with the Exchange as counterparty ([02 §2.3](02_kernel.md#23-resource)).

### 3.3 Transformations in brief

- **Energy shipping** (`energy-shipping`). Capacity 60 E per round, at a flat cost of 0.3 M per E paid by the sender, with no delay. Every order needs SHIP's signature. A cross-region Energy Transfer is valid only in the same candidate Transaction as a shipping order of the same quantity ([03 §5.5](03_premise.md#55-geography-access-and-rules)). If SHIP signs more than 60 E of shipments in a round, every shipping candidate Transaction of that round is rejected as a unit ([03 §5.3](03_premise.md#53-transformations)). Logistics investment adds 10 E of capacity from the next round. Governments restrict shipping by Rules ([§13.2](#132-trade-restrictions-and-the-tariff)).
- **Defence procurement** (`defense-procurement`; China orders from its own Transformation, `defense-procurement-cn`, with payee CN_PRIVATE). Capacity 30 Mil per round, backed by CHIP's 15 semiconductor units, one unit per 2 Mil. Units CHIP withholds, or that investment draws, cut capacity in proportion. **p(q) = 8 + 0.5q**, **C(q) = 8q + 0.25q²**, where q is the total Military ordered this round. Every order settles at the uniform price C(Q)/Q. Money and Energy are escrowed at screening at the full-capacity price, 15.5 M/Mil. Over-subscription is allocated in whole Military units, as in [Fixture F](#26-fixture-f-simultaneous-conflicting-orders). The payee is DEF, which passes 25% of its procurement revenue to US_WORKERS as variable wages.
- **Public attention** (`public-attention`). **p(q) = 3 + 0.2q**, **C(q) = 3q + 0.1q²**, where q is the total Soft Power generated this round. Capacity 50 units per round, one Soft Power per unit and at most 10 per order. The payee is WORLD_HOUSEHOLDS, standing in for the media sector.

## 4. Transformations

| Transformation | Inputs | Outcome | Payee |
|---|---|---|---|
| **Military Procurement** | Money and 2 E per Mil | Military delivered next round | DEF, or CN_PRIVATE for CN |
| **Public Campaign** | Money, on the attention Transformation's cost curve | Soft Power | WORLD_HOUSEHOLDS |
| **Humanitarian Aid** | 10 M + 5 E. The Energy is consumed in relief | 4 S to the provider. At most 2 units per order | WORLD_HOUSEHOLDS |
| **Military Deployment** (`military-deployment`, uncapped) | Reserves 1 Mil per unit. No input | Reservation for the round, released at commit | - |
| **Major Military Action** (`major-military-action`, minimum 12) | Consumes 1 Mil per unit | Returns 0.75 Mil per unit: 25% attrition, not a reservation | - |
| **Energy Shipping** | 0.3 M per E, paid by the sender. SHIP signs | Makes a bundled cross-region Energy Transfer valid | SHIP |
| **Logistics Investment** (SHIP only) | 10 M + 3 E | +10 E per round of shipping capacity from next round | GLOBAL_SMES |
| **Energy Capacity Investment** (producers only) | 12 M + 3 E and one semiconductor unit from CHIP. CHIP signs | +2 E per round of production capacity from next round | GLOBAL_SMES |
| **Strike** (LABOR only) | 8 S + 5 M strike pay | Next-round capacity loss ([§13.4](#134-strike)) | US_WORKERS |

## 5. Intents and Capabilities

Each Actor's distinctive capability and dependency are in [§9](#9-actor-catalog).

# Part II: PremiseComposition Configuration

## 6. PremiseComposition: Meridian

### 6.1 Story

A hypothetical near-future economy of intensifying competition between the United States and China, divided regional energy exchanges, and unequal access to industrial and military capability. Europe must coordinate while keeping national priorities. The PremiseComposition asks whether participants can preserve cooperation under conditions that reward competition.

### 6.2 Time horizon

Five simulated years, played as five one-year rounds. The PremiseComposition supports four to eight rounds.

## 7. Economic Verticals

Four provisional verticals group the Resources, Exchanges and capacities above. The engine does not record them ([03 §4](03_premise.md#4-verticals)).

| Vertical | Resources and state | Mechanism |
|---|---|---|
| **Energy and Industry** | Energy, Money, production capacity, semiconductor capacity | Production, investment, industrial dependency |
| **Security and Defense** | Military, Energy, Money, procurement capacity | Procurement, deployment, attrition |
| **Finance and Trade** | Money, Energy, transport capacity, tariffs | Circulation, regional exchange, restrictions |
| **Information and Diplomacy** | Soft Power, Money | Campaigns, aid, strikes |

## 8. Economic Circulation

Production, operating requirements and fixed flows recur every round. All amounts are per round and provisional.

### 8.1 Energy production

Supply is concentrated: ENERGY_SUPPLIERS is the largest source at 60 E.

<!-- check-docs: producers -->

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

- **Explicit and bloc requirements (E/round):** US and CN 10 each; DE, IN, JP and EU 8 each; UK and CHIP 6 each; SHIP 4. Subtotal 68.
- **Aggregates:** CN_PRIVATE, US_WORKERS, WORLD_HOUSEHOLDS and GLOBAL_SMES require their adjusted purchase target (baseline 12 + 8 + 12 + 40 = 72).

Each unit of unmet requirement reduces the Actor's fixed incoming transfers by 2 M, which the payers keep.

At baseline, production (140 E) equals the operating requirements (68 explicit plus 72 Aggregate). The Round 3 disruption ([§13.6](#136-energy-disruption)) is the designed scarcity. Aggregate demand is 80 E per round (EU 8, CN_PRIVATE 12, US_WORKERS 8, WORLD_HOUSEHOLDS 12, GLOBAL_SMES 40) against ENERGY_SUPPLIERS' offer of 40 E ([§11.4](#114-other-aggregate-strategies)). Prices therefore rise unless the explicit producers (SA, ENCO, BR, NG, IN, 80 E together) sell about 40 E per round. That is the producers' leverage.

### 8.3 Fixed income and expenditure

Fixed Money transfers, paid every round from opening holdings (M per round):

- **Taxes:** US_WORKERS → US 15; GLOBAL_SMES → US 5; CN_PRIVATE → CN 20; EU → DE 10 (German private-economy revenue, held in the EU residual); WORLD_HOUSEHOLDS → IN 10, JP 10, UK 8, BR 6, NG 4.
- **Public spending:** US → US_WORKERS 12; CN → CN_PRIVATE 18; DE → EU 8; IN 8, JP 8, UK 6, BR 5, NG 3 → WORLD_HOUSEHOLDS.
- **Wages:** ENCO → US_WORKERS 6; DEF → US_WORKERS 6; CHIP → WORLD_HOUSEHOLDS 6; SHIP → WORLD_HOUSEHOLDS 5; GLOBAL_SMES → WORLD_HOUSEHOLDS 10 and → US_WORKERS 6.
- **Sales and dues:** DEF → CHIP 6 (semiconductor supply contract); US_WORKERS → GLOBAL_SMES 12, → SME 3 and → LABOR 3 (dues); WORLD_HOUSEHOLDS → GLOBAL_SMES 12 and → SHIP 4; EU → AID 3 (donations).

Net Money per round, derived from these flows: US +8; CN +2; DE, IN, JP, UK +2 each; BR, NG +1 each; SME, LABOR, AID, GLOBAL_SMES +3 each; CHIP 0; ENCO −6; DEF −12; SHIP −1; US_WORKERS −3; CN_PRIVATE −2; EU −5; WORLD_HOUSEHOLDS −3. Inflow and outflow both total 32 M.

CHIP's supply contract covers its wages. Further CHIP revenue comes from signed candidate Transactions.

### 8.4 Variable flows

ENERGY_SUPPLIERS pays 50% of its net Energy-sale revenue to WORLD_HOUSEHOLDS (producer recycling). DEF's 25% wage share is in [§3.3](#33-transformations-in-brief). Other variable flows go to the payees in [§3](#3-exchanges-and-transformations) and [§4](#4-transformations).

### 8.5 Capacity renewal and reconciliation

Investment in round r changes capacity from round r+1. Fixture `circulation` checks five rounds with no other action: US Money 260 → 300, and DE Money 125 → 131 with Energy 0, since DE's 8 E requirement exceeds its stock in round 5 (2 E unmet withholds 4 M of its incoming transfers).

## 9. Actor Catalog

Meridian has fifteen Playable Actors and one non-playable Actor, China. This table is the one description of each of them.

<!-- check-docs: actors -->

| ID | Actor | Category | Region | Distinctive capability | Dependency | Objective ([§9.3](#93-objectives)) |
|---|---|---|---|---|---|---|
| US | US Head of State | Government | Atlantic | Tariffs, shipment bans, deployment, Military sales. Largest procurement budget | Energy above its holdings; procurement capacity | Military leadership, solvent |
| DE | German Chancellor | Government | Atlantic | Signs EU procedure candidate Transactions. Optional solidarity contributions | Atlantic Energy; tax revenue from the EU residual | Energy security |
| IN | Indian Prime Minister | Government | Asia | Energy production ([§8.1](#81-energy-production)). Tariffs | Asia Energy prices | Industrial autonomy |
| JP | Japanese Prime Minister | Government | Asia | Tariffs. Procurement. Large Asia purchasing | Imports and transport; no production | Energy security, credible defense |
| UK | UK Prime Minister | Government | Atlantic | Tariffs. Procurement. Campaigns | Atlantic Energy | Diplomatic influence |
| BR | Brazilian President | Government | Atlantic | Energy production. Capacity investment | Semiconductor units for investment | Stronger finances |
| SA | Saudi Energy Minister | Government | Asia | Largest government production | Transport for Atlantic sales | Energy wealth |
| NG | Nigerian President | Government | Atlantic | Energy production. Capacity investment | Semiconductor units; investment | National capacity |
| ENCO | US Energy Consortium CEO | Corporation | Atlantic | Energy production. Capacity investment | Semiconductor units; strikes | Profit and supply growth |
| CHIP | Semiconductor Producer CEO | Corporation | Asia | Semiconductor capacity behind procurement ([§2](#2-resources)). May withhold units, and signs each Energy capacity investment | 6 E/round operating Energy | Secure Energy inputs |
| SHIP | Shipping Operator CEO | Corporation | Both | Owns `energy-shipping` ([§3.3](#33-transformations-in-brief)). Chooses which shipments to sign. Arbitrage | 4 E/round fuel; shipment bans | Open routes, high throughput |
| DEF | Defense Manufacturer CEO | Corporation | Atlantic | Operates procurement. Receives non-Chinese revenue | CHIP's semiconductor capacity; strikes | Procurement revenue |
| LABOR | US Labor Federation Chair | Civil institution | Atlantic | Strike ([§13.4](#134-strike)), declared or withheld each round | Dues; Soft Power | Workers' position and influence |
| SME | Small-Business Owner | Small participant | Atlantic | Representation of Global SME demand ([§12.2](#122-small-business-owner--global-smes-representation)) | Atlantic prices; small cash | Business viability |
| AID | Humanitarian Alliance Director | Civil institution | Atlantic | Aid. Energy relief to Aggregates in shortfall. Soft Power | EU donations; Energy purchases | Limit crisis damage |
| CN | Chinese Head of State | Government | Asia | Tariffs, shipment bans, deployment | Asia Energy prices; CN_PRIVATE tax base | Military parity, solvent |

### 9.1 Semiconductor capacity

CHIP may enact a `Withholding` rule on some units ([§2](#2-resources)). An Energy capacity investment draws one unit under a CHIP-signed candidate Transaction. Each unit drawn counts against CHIP's units for that round and cuts its procurement by 2 Mil.

### 9.2 Fallbacks

Where a seat is not explicit, its accounts are controlled by a default Strategy or merged into an Aggregate.

| Actor | Fallback when not explicit |
|---|---|
| US, CN | Required explicit. Human or in-world Strategy controller |
| DE | Government accounts held in the EU Aggregate as member-government accounts. Germany's weight folds into the EU's residual weights ([§14.2](#142-resolution-consistency)) |
| IN, JP, UK, BR, SA, NG | `government-default-v0` controls the same accounts: baseline spending, no discretionary actions |
| ENCO, CHIP, SHIP, DEF | `firm-default-v0` controls the same accounts: sells Energy above 30 E on every Energy Exchange it has access to (its own node, or both nodes for SHIP) and signs `energy-shipping` candidate Transactions |
| LABOR | Implicit coverage: accounts merge into US_WORKERS. No strike capability |
| SME | Implicit coverage: accounts merge into GLOBAL_SMES. No Representation |
| AID | Implicit coverage: accounts merge into WORLD_HOUSEHOLDS, which also receives EU donations |

### 9.3 Objectives

Each explicit Actor's objective is a set of measurable criteria over the recorded Run ([03 §10.5](03_premise.md#105-evaluation)). The criteria are provisional, never affect the economy, and are reported per Actor as met or not met, with values.

| Actor | Criteria |
|---|---|
| US | Final Military ≥ CN final Military. Final Money ≥ 200 |
| CN | Final Military ≥ US final Military. Final Money ≥ 200 |
| DE | No Energy shortfall in any round. Final Money ≥ 110 |
| IN | Final production capacity ≥ 7 E/round. No shortfall in any round |
| JP | No shortfall in any round. Final Military ≥ 36 |
| UK | Final Soft Power ≥ 47. Final Money ≥ 110 |
| BR | Final Money ≥ 150 |
| SA | Final Money ≥ 250. Final Energy ≥ 60 |
| NG | Final production capacity ≥ 14 E/round. Final Money ≥ 90 |
| ENCO | Final Money ≥ 220. Final production capacity ≥ 27 E/round |
| CHIP | No shortfall in any round. Final Money ≥ 150 |
| SHIP | Cumulative Energy shipped ≥ 150 E. Final Money ≥ 130 |
| DEF | Cumulative procurement revenue ≥ 300 M |
| LABOR | US_WORKERS final Money ≥ 260. Final Soft Power ≥ 30 |
| SME | Final Money ≥ 15. GLOBAL_SMES shortfall in at most 1 round |
| AID | Total Aggregate shortfall across the Run ≤ 20 E. Final Soft Power ≥ 55 |

## 10. Aggregate Catalog

Meridian has seven Aggregates. Each has accounts and a baseline Strategy. Only the EU also has collective procedures.

| ID | Aggregate | Residual accounts | Region | Baseline responsibility |
|---|---|---|---|---|
| EU | European Union | Non-explicit member economies and bloc accounts | Atlantic | Energy purchasing. Collective decisions |
| EU_POOL | EU Solidarity Pool | The energy the `eu-solidarity` procedure draws on first | Atlantic | None. Acts only through the EU's procedure |
| CN_PRIVATE | Chinese Private Economy | Chinese activity outside the government | Asia | Industrial demand. Chinese defense production |
| US_WORKERS | US Workforce | Residual worker accounts | Atlantic | Household demand |
| GLOBAL_SMES | Global Small Businesses | Small businesses outside explicit ones | Atlantic | Energy demand. Investment supply |
| ENERGY_SUPPLIERS | Other Energy Producers | Production not held by explicit Actors | Both | Energy supply |
| WORLD_HOUSEHOLDS | Other World Households | Residual household activity | Asia | Household demand. Media and relief payee |

**EU_POOL is an account, not an explicit Actor, and has no capabilities by design.** It is the one Actor with Atlantic exchange access and no trade capability ([03 §5.2](03_premise.md#52-exchanges)), so the pool-to-EU transfer counts as a same-region transfer. Its authority comes from the EU's `eu-solidarity` procedure. Its Strategy (`solidarity-pool-v0`) has no rules and no signatures. [Fixtures B2 and B2-pool](#22-fixture-b-germany-and-the-eu-collective-procedure) exercise the pool-to-EU transfer.

### 10.1 Germany and the EU

- **Account ownership.** Making the Chancellor explicit extracts only the German government's accounts. German households and firms stay in the EU residual, which is why German tax revenue flows EU → DE ([§8.3](#83-fixed-income-and-expenditure)). The EU also owns a solidarity pool, initially empty, held by the Actor `EU_POOL`.

### 10.2 Small-business representation

The owner is a member of `GLOBAL_SMES`. Their holdings and direct transactions are excluded from the residual. Their proposed decisions serve as bounded evidence through a Representation ([§12.2](#122-small-business-owner--global-smes-representation)).

## 11. Strategy Catalog

### 11.1 China non-playable Actor Strategy: `china-strategic-v0`

1. If the US ran `major-military-action` of at least 12 Mil in the previous round, order 4 Mil from the procurement Transformation.
2. If the beginning-of-round `energy-asia` price exceeds 3.2 M/E, offer up to 12 E on that node.
3. If a US restriction targeting China was enacted in the previous round, enact the reciprocal restriction, effective at the next round boundary.

China's budget covers its orders in the reference Run.

### 11.2 EU Aggregate Strategy: `eu-coordination-v0`

- Buy 8 E per round on `energy-atlantic`, or 4 E when the beginning-of-round price exceeds 3.0 M/E.
- Publish the solidarity and trade procedure candidate Transactions when their conditions hold ([§12.1](#121-eu-collective-procedures)).
- Pay the fixed flows in [§8.3](#83-fixed-income-and-expenditure).

### 11.3 Global Small-Business Strategy: `global-smes-v0`

Baseline demand of 40 E per round on `energy-atlantic`, reduced by 20% when the beginning-of-round price exceeds 3.5 M/E. The target is that baseline multiplied by the factor stored in the previous round ([§12.2](#122-small-business-owner--global-smes-representation)), or the baseline alone when none was stored. The target is both the purchase order and the operating requirement.

### 11.4 Other Aggregate Strategies

| Aggregate | Strategy |
|---|---|
| CN_PRIVATE | Buys 12 E on `energy-asia`. 9 E above 3.0 M/E |
| US_WORKERS | Buys 8 E on `energy-atlantic`. 6 E above 3.0 M/E |
| WORLD_HOUSEHOLDS | Buys 12 E on `energy-asia`. 9 E above 3.0 M/E |
| ENERGY_SUPPLIERS | Produces 60 E. Offers 40 E (24 Atlantic / 16 Asia), or 50 E (30 / 20) at a node whose beginning-of-round price is at least 2.6 M/E. Retains unsold output |

Each household and industrial Aggregate's purchase target is also its operating requirement. `government-default-v0` and `firm-default-v0` are fallbacks, inactive in the reference instantiation.

## 12. Representation and Collective Procedures

### 12.1 EU collective procedures

| Procedure | Authorizes | Member weights | Residual weight | Threshold |
|---|---|---:|---:|---:|
| `eu-solidarity` | Energy sales on `energy-atlantic` and Energy Transfers | DE 0.20 | 0.40 | 0.45 |
| `eu-trade` | Enacting a `Tariff` | DE 0.20 | 0.48 | 0.45 |

The EU's Strategy decides when to publish ([§11.2](#112-eu-aggregate-strategy-eu-coordination-v0)):

- **Solidarity candidate Transaction:** sell up to 8 E on `energy-atlantic`, drawing first on the pool and then on its holdings. The pool gives min(8, pool) and the EU sells 8 E. Published in any round whose beginning-of-round Atlantic price exceeds 3.0 M/E, provided the EU's holdings cover its operating requirement of 8 E.
- **Trade candidate Transaction:** enact a matching import `Tariff` of 0.5 M/E on cross-region Energy from the restricting jurisdiction, in force from the next round. Published in any round in which a US or Chinese restriction is in force and no EU tariff is.

Germany is pivotal for solidarity: `eu-solidarity` executes only with its signature (0.40 without it lapses; 0.60 with it executes). `eu-trade` executes with or without Germany (0.48 or 0.68). Germany may also transfer Energy into the solidarity pool at any time, independent of signing ([Fixture B1](#22-fixture-b-germany-and-the-eu-collective-procedure)). The procedure is stylized, not a model of EU voting law.

### 12.2 Small-Business Owner → Global SMEs (Representation)

Weight 1; reference 5 E per round; shrinkage 0.25; noise spread 0.10 (ε uniform in [−0.10, +0.10], drawn from the game seed); deviation bound ±50%. The observation is the owner's validated, escrowed proposed quantity, not the executed fill.

**Factor (round r) = 1 + 0.25 × clamp(deviation + ε, −0.50, +0.50)**

**Target (round r+1) = Baseline (round r+1) × factor**

ε is drawn in the observation's round, and only when the owner submits an observation.

- **Proposed 4 E (−20%):** next-round target 40 × 0.95 = 38 E noise-free; range 37–39 E over ε.
- **Proposed 0 E (−100%):** 40 × 0.875 = 35 E; the clamp absorbs the noise.
- **Proposed 10 E (+100%):** 40 × 1.125 = 45 E; the clamp absorbs the noise.

Round r+1 uses the stored factor in place of its baseline, once ([02 §7.3](02_kernel.md#73-representation-pipeline)). The owner's own purchase executes separately in round r.

## 13. Rules of Play

### 13.1 Institutional changes

Tariffs, shipment bans, withholding and strikes take effect at the round boundary. A tariff that a voted candidate Transaction enacts in round r is in force from round r+1 ([§12.1](#121-eu-collective-procedures)).

### 13.2 Trade restrictions and the tariff

Governments restrict shipping by enacting Rules:

- **Shipment ban** (`AccessBan`). Blocks Energy Transfers that need shipping between the restricting and target jurisdictions. Enacted by the governments holding the capability (US, CN, IN, JP, UK, BR, SA, NG).
- **Import tariff** (`Tariff`). Levies a rate of t M/E on inbound shipped Energy. The EU's matching tariff is 0.5 M/E ([§12.1](#121-eu-collective-procedures)).

Germany holds no tariff authority of its own. EU-wide restrictions belong to the EU's trade procedure. The bound t ≤ 1.0 is this PremiseComposition's own, since the source gives none ([§17](#17-differences-from-the-source)).

### 13.3 Military deployment

`military-deployment` reserves 1 Mil per unit for the round. `major-military-action` (minimum 12 Mil) consumes 1 Mil per unit and returns 0.75 at commit, so a major action loses 25% of its units. A smaller order is rejected ([Fixture D-minor](#24-fixture-d-china-responds-as-a-non-playable-actor)). China's non-playable Actor rule reads only `major-military-action` ([§11.1](#111-china-non-playable-actor-strategy-china-strategic-v0)), so a reservation alone does not trigger it.

Run as the `Deployer` preset ([Simulator §4.2](07_simulator.md#42-presets)) with the other seats Passive, the Atlantic price is 2.97 at the start of round 3 and 3.29 at the start of round 4, so the preset acts in rounds 4 and 5. Round 5 starts with 57 Mil against 100 claimed, so both scale by 0.57. The deployment reserves 34.2 and the sale sells 22.8 Mil at 2.506, which clears the 2.4 limit. US Military ends at 34.2.

**Fixture R** (a deployment reserves Military while a sale runs): US reserves 60 Mil for deployment and sells 40 Mil at a 2.4 limit, in the same round. Scaled to 57 reserved and 38 sold at 2.81, paying 106.78 M. US Military 57, Money 366.78. Round 2: a 50 Mil Transfer to Japan. US Military 7, Japan 86.

**Fixture R-alone** (no deployment): US sells 40 Mil at a 2.4 limit. Price 2.80. Pays 112 M. US Military 55, Money 372. The reservation leaves the sale 2 units short in R.

### 13.4 Strike

LABOR may strike once per round, for 8 S and 5 M of strike pay paid to US_WORKERS. The strike takes effect the next round: ENCO's production capacity falls by 6.25 E and CHIP's semiconductor capacity by 3.75 units, which back procurement.

### 13.5 Joint candidate Transactions

A joint candidate Transaction settles atomically once every required signer signs. A candidate Transaction missing a signature lapses and the rest of the round resolves ([Fixture G](#27-fixture-g-candidate-transactions-and-signatures)).

### 13.6 Energy disruption

The Run set varies one uncertainty, the Energy shock, in two named cases.

- **Calm case.** The reference Run, with its one scheduled event. In Round 3, `ENERGY_SUPPLIERS`' Strategy reads the `disruption-round` parameter and cuts its maximum offer by 20 E (12 Atlantic, 8 Asia). A cut never takes the offer below zero, so a cut at or above the offer removes it and never turns it into a purchase. Production continues, and unreleased output accumulates in its holdings. The disruption is predeclared configuration, not an AI intervention.
- **Major case.** The same disruption moved to Round 1, with both cuts at or above the offers (Atlantic 60, Asia 40), so each region's supply is zero that round. It is not the default Run. It flips the Atlantic price across 3.0 in both directions and changes four presets' Atlantic actions. With every seat on `Conservative`, the end-of-round Atlantic prices are calm 2.89, 3.14, 3.32, 3.12, 2.86 and major 3.07, 3.19, 3.11, 2.92, 2.77. Asia moves from 2.16–2.80 to 2.48–2.88. With every seat on one preset, `Accumulator`, `Trader` and `Deployer` change what they propose in Rounds 2, 3 and 5, and `Opportunistic` in Rounds 2 to 4. The EU's solidarity candidate Transaction is published in Round 2 and not in Round 5. In the `Conservative` run the Atlantic price never reaches 3.5, so `CrisisResponse` does not flip. The flip is pinned by a test ([Simulator §14](07_simulator.md#14-mandatory-tests)).

# Part III: Run Configuration

## 14. Run Configuration

- **Game ID:** `meridian/meridian-test-01`, the PremiseComposition id then its label. **PremiseComposition:** `meridian`, version `v0`.
- **Seats / non-playable Actors / Aggregates:** 15 / 1 (China) / 7. **Rounds:** 5 rounds of 1 year ([§6.2](#62-time-horizon)).
- **Game seed:** `26021` (its only use in v0 is SME sampling noise).

### 14.1 Seat assignments

Seats 1–15 drive, in order: US, DE, IN, JP, UK, BR, SA, NG, ENCO, CHIP, SHIP, DEF, LABOR, SME, AID.

China is Actor `CN`, controlled by the in-world Strategy `china-strategic-v0`. It has no seat. Germany is a seat. The EU is an Aggregate under `eu-coordination-v0`, with Germany among its members and both collective procedures active. No human controls it. The `sme-energy-sample` Representation is active.

### 14.2 Resolution consistency

With Germany implicit, the German government's accounts sit in the EU Aggregate ([§9.2](#92-fallbacks)). Totals must reconcile ([INV-14](02_kernel.md#inv-14)):

| Account | Germany explicit (DE + EU) | Germany implicit (EU) |
|---|---:|---:|
| Money | 125 + 320 = 445 | 445 |
| Energy | 38 + 125 = 163 | 163 |
| Military | 24 + 50 = 74 | 74 |
| Soft Power | 45 + 75 = 120 | 120 |

With Germany implicit, the EU → DE and DE → EU flows become internal. DE's 8 E requirement joins the EU's (16 E total). Germany's 0.20 weight joins the EU's residual weight for each procedure ([§12.1](#121-eu-collective-procedures)).

## 15. Initial Holdings

**All values are invented balancing parameters, not empirical estimates.** Aggregates are in italics. The German government's holdings are excluded from the EU, and German private accounts are included. The owner's holdings are excluded from Global SMEs. China's government and private economy are separate. ENCO's holdings and capacity are excluded from Other Energy Producers. The solidarity pool starts at 0 E, as its own account.

| Actor | Money | Energy | Military | Soft Power |
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
| **China (non-playable Actor)** | **255** | **95** | **88** | **57** |
| *EU* | *320* | *125* | *50* | *75* |
| *Chinese Private Economy* | *210* | *85* | *0* | *25* |
| *US Workforce* | *260* | *60* | *0* | *45* |
| *Global Small Businesses* | *300* | *80* | *0* | *25* |
| *Other Energy Producers* | *200* | *240* | *0* | *22* |
| *Other World Households* | *360* | *90* | *0* | *35* |

## 16. Initial Exchange and Capacity State

- Every Exchange starts at position x = 0, with the reserves and opening prices in [§3](#3-exchanges-and-transformations).
- Capacities and production start at the values in [§2](#2-resources), [§3.3](#33-transformations-in-brief) and [§8.1](#81-energy-production). Procurement, public attention and the semiconductor units start with no orders.
- No `Tariff`, `AccessBan` or `Withholding` is in force at the start.

# Part IV: Playtest Fixtures

Each fixture is a small world filtered from Meridian, with the Actors it needs and no Strategies or seats: its decisions are literal candidate Transactions and signatures. Unless stated otherwise, fixtures disable circulation, production and operating requirements. Amounts are exact at the precision of [02 §9.1](02_kernel.md#91-numerics-and-randomness), and expected values are literals from the worked examples, never computed. The tests that run them are in [Simulator §14](07_simulator.md#14-mandatory-tests). Fixture R is in [§13.3](#133-military-deployment) and fixture `circulation` in [§8.5](#85-capacity-renewal-and-reconciliation).

## 21. Fixture A: Direct Exchange

- **A.** US buys 10 E, Germany sells 5 E, no limit. Price 2.05 M/E. US Money 260 → 239.50, Energy 70 → 80. DE Money 125 → 135.25, Energy 38 → 33. Atlantic reserves 700 → 710.25 M and 350 → 345 E. Money totals 1,085 and Energy totals 458 before and after.
- **A2.** As A, with the US limit at 2.04. Price 1.95 M/E. DE receives 9.75 M (ends at 134.75 M and 33 E). The US order is rejected and US balances are unchanged (260 M, 70 E). Atlantic reserves 690.25 M and 355 E.

## 22. Fixture B: Germany and the EU Collective Procedure

- **B1.** Atlantic price at or below 3.0. DE transfers 10 E to the pool. DE Energy 38 → 28. Pool 0 → 10 E. EU stays at 125 E. EU holdings including the pool are 135 E.
- **B2.** Atlantic node at x = 60 (price 3.2). EU solidarity candidate Transaction, signed by DE. Support 0.60 > 0.45, so the EU's offer of up to 8 E clears. EU Energy 125 → 117, Money 320 → 344.96. DE stays at 125 M and 38 E. Pool 0. Atlantic reserves 675.04 M and 358 E.
- **B2-pool.** As B2, with 10 E in the pool. The pool gives 8 E and the EU sells 8 E: pool 10 → 2 E, EU Energy stays 125. Atlantic reserves 675.04 M and 358 E.
- **B3.** As B2, unsigned. Support 0.40. The candidate Transaction lapses. EU stays at 320 M and 125 E.
- **B4.** US shipment ban on China in force. EU tariff candidate Transaction. Support 0.68 signed and 0.48 unsigned (B4-unsigned). Both pass, and the tariff is in force next round.

## 23. Fixture C: Small-Business Owner Representation

- **C1.** Owner buys 4 E at a 2.1 limit, noise spread 0. Factor 0.95. Owner Money 15 → 6.84, Energy 5 → 9. Global SMEs' next-round target 38 E, replacing the 40 E baseline.
- **C2.** Owner proposes 0 E (clamp at the lower bound). Clamped to −50%. Factor 0.875. Next-round target 35 E.
- **C3.** Owner proposes 4 E with a limit violated at clearing. Observation stays 4 E, factor 0.95, next-round target 38 E. Record shows 4 E proposed and 0 E executed. Owner stays at 15 M and 5 E.
- **C4.** Golden test, not a fixture file. Round r+1 price above 3.5 M/E, factor 0.95 from a 4 E observation. Baseline 32 E. Target 32 × 0.95 = 30.4 E.
- **C5.** Seeded noise, game seed 26021, noise spread 0.10. Factor 0.9358. Next-round target for a 4 E observation lies in [37, 39] E. A proposed 0 E still gives exactly 35 E.

## 24. Fixture D: China Responds as a Non-Playable Actor

- **D.** US orders 12 Mil in Round 1. China observes it and orders 4 Mil in Round 2. Round 1: US Military 95 → 92 (3 attrition, 9 returned). Round 2: cost 36 M at 9 M/Mil. China Money 255 → 219, Energy 95 → 87. CN_PRIVATE 210 → 246. Round 3: China Military 88 → 92.
- **D-minor** (golden). US orders 8 Mil of `major-military-action`. Rejected. US Military stays at 95.

## 25. Fixture E: Scarcity and Trade Restriction

- **E** (qualitative, no fixture file). Full circulation with the Round 3 disruption ([§13.6](#136-energy-disruption)). Passes on reconciliation, solvency and propagation: operating shortfalls, capacity investment and household demand stepping from 8 E to 6 E above 3.0 M/E, and the EU solidarity candidate Transaction executed in the same round. Observed by preset ([test 8](07_simulator.md#14-mandatory-tests)): shortfalls (`Aggressive`), investment (`Investor`), procurement (`Aggressive`), SME demand with its Representation factor (`Passive`), and the solidarity candidate Transaction with Germany's signature (`Investor`). SHIP places no Exchange order and China's Asia offer does not fire. Compare price gaps, shipping volumes and tariff revenue with the unrestricted run.
- **E2** (restriction). US enacts a shipment ban on China in Round 2, effective Round 3. China reciprocates, effective Round 4. The EU publishes and passes its tariff candidate Transaction in Round 3, in force from Round 4 (as in B4).

## 26. Fixture F: Simultaneous Conflicting Orders

- **F.** US, UK and JP order 15, 8 and 9 Mil against a capacity of 30, with escrow at 15.5 M/Mil: US 232.5, UK 124, JP 139.5 M. Allocation 30/32 gives 14.0625, 7.5 and 8.4375. Fills US 14, UK 8, JP 8 (the spare unit goes to UK). Cost C(30) = 465 M. US pays 217 M and uses 28 E. UK and JP pay 124 M each and use 16 E each. DEF receives 465 M and passes 116.25 M to US_WORKERS. Reversing submission order gives identical fills.

## 27. Fixture G: Candidate Transactions and Signatures

- **G1.** SA transfers 20 E to JP and JP transfers 48 M to SA, and JP signs. SA 155 → 203 M, 110 → 90 E. JP 150 → 102 M, 40 → 60 E.
- **G2.** As G1, JP does not sign. The candidate Transaction lapses. SA 155 M, 110 E. JP 150 M, 40 E.
- **G3.** SA sends 20 E to DE, DE sends 48 M, SA orders `energy-shipping` for 20. Signers SA, DE and SHIP. Fee 6 M. SA 155 → 197 M, 110 → 90 E. DE 125 → 77 M, 38 → 58 E. SHIP 110 → 116 M.
- **G3-ban.** As G3, with an `AccessBan` on the route. Rejected as a unit while the rest of the round resolves. SA 155 M, 110 E. DE 125 M, 38 E. SHIP 110 M, 65 E.
- **G4.** Out-of-band strike agreement, which is only ordinary Transfers. No strike in Round 2. ENCO and DEF each transfer 10 M to LABOR. LABOR 50 M, 4 E. ENCO 130 M, 140 E. DEF 100 M, 30 E. Nothing binds LABOR in later rounds.
- **G5.** SA sends 20 E to JP and JP sends 48 M to SA. JP sends 10 M to SHIP. Signers JP and SHIP. SA 203 M, 90 E. JP 92 M, 60 E. SHIP 120 M, 65 E.
- **G5-unsigned.** As G5, SHIP does not sign. Candidate Transaction lapses. No balance changes: SA 155 M, 110 E. JP 150 M, 40 E. SHIP 110 M, 65 E.

- **F-bound.** US and DE sell 70 and 38 E on the Atlantic Exchange, wanting x = −108 against its bound of −80. 80 fills, split 70 : 38 into 51.8519 and 28.1481, at a price of 1.2. US ends at 322.2223 M and 18.1481 E, DE at 158.7777 M and 9.8519 E.
- **F-short.** US orders 200 E at a limit of 3, escrowing 600 M against the 260 it holds. Funding scales the order by 0.4333 to 86.66 E, at 2.8666 M/E. US ends at 11.5804 M and 156.66 E.

- **H.** An agreement entered during play. SA proposes the template `loan-advance`, principal 50 M, lender SA, borrower JP, and JP signs. Round 1: nothing moves, SA 155 M, JP 150 M. Round 2: 50 M is advanced, SA 105 M, JP 200 M, and `loan-repay` falls due next round at 1.1 times what moved. Round 3: 55 M is repaid, SA 160 M, JP 145 M.

## 17. Differences from the source

This document leads. Where it differs from the source, or where a value is not pinned by a test, its value stands, as recorded here.

- **Solidarity pool.** The solidarity draw, min(8, pool), is pinned only at pools of 0 and 10 E (B2 and B2-pool). A partial pool (0 < pool < 8 E) has no fixture. The `solidarity-pool-v0` Strategy is empty, so the draw rule lives in the procedure.
- **JP signing threshold.** The 2.4 M/E threshold for JP's signature, as recorded in the scope pass, has no source value. The only 2.6 M/E threshold in the source is `ENERGY_SUPPLIERS`' high-price switch ([§11.4](#114-other-aggregate-strategies)).
- **Tariff bound.** The bound t ≤ 1.0 on the tariff rate ([§13.2](#132-trade-restrictions-and-the-tariff)) is not in the source. The source sets the EU rate to 0.5 M/E and gives no bound.
- **Per-preset flip rounds.** The round lists for each preset under the major case ([§13.6](#136-energy-disruption)) are not pinned by a test. The calm and major prices for `Conservative` are pinned.
