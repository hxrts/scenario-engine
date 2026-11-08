# Reference Instance: Meridian

**Version:** 0.2  
**Status:** Experimental configuration. All setting choices and numerical parameters are provisional  
**Setting ID:** `meridian`, version `v0` (Gameboard `meridian@v0`)  
**Implementation language:** PureScript

---

## 1. Purpose

This document defines a concrete reference instance of the Scenario Planning Game, provisionally titled **Meridian**.

It makes experimental choices about resources, participants, markets, facilities, circulation, policies, and rules on top of the Type System, Gameboard, and Instantiated Game specifications. Terms follow the Type System Glossary ([§2.5](02_type_system.md#25-glossary)). Invariants are cited by ID ([§9](02_type_system.md#9-architectural-invariants-and-numerics)). Changing this instance should normally mean changing data, not engine code. A mechanic that does not fit the existing types is a finding, not a special case.

### 1.1 Test objectives

The instance should demonstrate that:

1. Fifteen seats and one NPC share one economic world through the same interfaces ([INV-01](02_type_system.md#inv-01)).
2. An explicit government can sit inside an implicit Aggregate ([INV-08](02_type_system.md#inv-08)).
3. Representation and collective procedures give small Actors and member states bounded influence ([INV-09](02_type_system.md#inv-09), [INV-03](02_type_system.md#inv-03)).
4. Four Resources with differing configurable semantics generate meaningful strategic interaction.
5. Production, income, expenditure, and capacity renewal sustain five years of activity ([INV-05](02_type_system.md#inv-05)).
6. Prices respond to players and the implicit economy, and trade restrictions have identifiable effects.
7. Every Actor has a distinct capability, a dependency, and a measurable objective ([INV-19](02_type_system.md#inv-19)).
8. Policies respond to conditions without being rewritten during play ([INV-12](02_type_system.md#inv-12)).
9. Out-of-band joint Proposals settle atomically once every required signer signs ([INV-10](02_type_system.md#inv-10)).
10. One Gameboard supports different levels of explicitness with consistent resolution ([INV-14](02_type_system.md#inv-14)).

---

# Part I: Type System Configuration

Part I chooses values for the Type System's abstract types. Their rules are in [Type System](02_type_system.md) and are not restated here.

## 2. Resources

The instance uses exactly four Resources, all abstract game units. Their differences are declared Resource properties.

| Resource | Unit | Created / destroyed by | Transferability | Decay | Reservable |
|---|---|---|---|---|---|
| **Money** | M | Nothing (issuance disabled in v0), so it is conserved | Transferable and tradable | None | No |
| **Energy** | E | Production / consumption | Transferable and tradable | None | No |
| **Military** | Mil | Procurement / declared attrition | Government-to-government Transfer, under procurement authority. A Trade on the `defence-goods` Market for the US and its allies ([§3.5](#35-defence-goods-market)) | None | Yes, by deployment |
| **Soft Power** | S | Campaigns and aid / spending and decay | Non-transferable | 10% per simulated year | No |

Energy is one commodity. Geography is expressed through regional Markets and transport capacity. There is no tactical combat.

### 2.1 Economic state that is not a Resource

| Economic state | Owner | Purpose |
|---|---|---|
| Energy production capacity | Energy producers | Energy created each round ([§8.1](#81-energy-production)) |
| Transport capacity | SHIP | Capacity of `energy-shipping` ([§3.2](#32-transport-capacity-and-trade-restrictions)) |
| Semiconductor capacity | CHIP | 15 units/round. Backs procurement (1 unit per 2 Mil) and Energy capacity investment |
| Procurement capacity | DEF | Annual Military production ([§3.3](#33-defense-procurement-facility)) |
| Rules in force (`Tariff`, `AccessBan`, `Withholding`) | Governments. The EU collective procedure. CHIP | Who may ship across regions and at what surcharge. Withheld capacity |

## 3. Markets and Facilities

Markets and facilities price through the shared CostFunction primitive. Facilities create output only through their recipe ([INV-07](02_type_system.md#inv-07)).

### 3.1 Regional Energy Markets

| Market ID | Region | Entities with access | Money reserve | Energy reserve | Operational interval |
|---|---|---|---:|---:|---|
| `energy-atlantic` | Atlantic | US, DE, UK, BR, NG, ENCO, DEF, LABOR, SME, AID, EU, EU_POOL, US_WORKERS, GLOBAL_SMES | 700 M | 350 E | −80 ≤ x ≤ 300 |
| `energy-asia` | Asia | CN, IN, JP, SA, CHIP, CN_PRIVATE, WORLD_HOUSEHOLDS | 500 M | 250 E | −80 ≤ x ≤ 200 |

SHIP trades on both nodes and `ENERGY_SUPPLIERS` offers into both per its Policy. Other Entities have access to one node only. Energy crosses regions only by a shipped Transfer ([§3.2](#32-transport-capacity-and-trade-restrictions)) or by SHIP's own two-node trading.

Both nodes use the same curve:

**p(x) = 2 + 0.02x**, **C(x) = 2x + 0.01x²**

where x is the node's cumulative net Energy sold. The initial price is 2 M/E. Each upper bound sits 50 E below the node's Energy reserve. At the lower bound a node pays out C(0) − C(−80) = 96 M, within both Money reserves ([INV-06](02_type_system.md#inv-06)). Orders and escrow follow [Type System §5.5](02_type_system.md#55-simultaneous-clearing) and [§8.2](02_type_system.md#82-single-round-update-and-snapshot-discipline).

### 3.2 Transport capacity and trade restrictions

**Transformation ID:** `energy-shipping`

- **Capacity:** 60 E per round (SHIP's transport capacity). Logistics investment adds capacity from the next round.
- **Cost:** flat 0.3 M per E (CostFunction base 0.3, slope 0), with no delay.
- **Payee:** SHIP. **Authority:** every shipping order requires SHIP's signature.

A cross-region Energy Transfer is valid only when bundled in the same Proposal with a `Transform energy-shipping` order of the same quantity, executed by the sender, with SHIP signing. If the shipments SHIP signs in a round exceed the capacity, every shipping Proposal of that round is rejected as a unit and recorded.

Governments restrict shipping by enacting Rules through `ChangeRule`, effective at the next round boundary:

| Restriction | Rule | Effect |
|---|---|---|
| **Shipment ban** | `AccessBan` | Shipping orders between the restricting and target jurisdictions' Entities are infeasible |
| **Import tariff** | `Tariff` | Adds t M/E (t ≤ 1.0) to shipping orders delivering into the restricting jurisdiction from the origin, paid to the restricting government |

Germany holds no tariff authority of its own. EU-wide restrictions belong to the EU collective procedure ([§12.1](#121-germany--eu-collective-procedure)).

### 3.3 Defense procurement (facility)

**Transformation ID:** `defense-procurement` (China orders from its own `defense-procurement-cn`, with payee CN_PRIVATE)

- **Capacity:** 30 Mil per round, backed by CHIP's 15 semiconductor units (1 unit per 2 Mil). Units CHIP withholds, or that capacity investment consumes, reduce capacity proportionally.
- **Cost:** **p(q) = 8 + 0.5q**, **C(q) = 8q + 0.25q²**, where q is total Military ordered this round. Every order settles at the uniform price C(Q)/Q.
- **Recipe:** each Mil also consumes 2 E.
- **Payee:** DEF for every buyer except CN, whose payments go to CN_PRIVATE. DEF passes 25% of its procurement revenue to US_WORKERS as variable wages.
- **Constraints:** buyers need procurement authority. Money and Energy are escrowed at screening at the full-capacity price (15.5 M/Mil). Delivery is at the Prepare phase of the next round. Over-subscription is allocated pro rata in whole units, remaining units by largest remainder, ties by canonical EntityId.

### 3.4 Public attention (facility)

**Transformation ID:** `public-attention`

- **Cost:** **p(q) = 3 + 0.2q**, **C(q) = 3q + 0.1q²**, where q is total Soft Power generated this round. Uniform price C(Q)/Q.
- **Capacity:** 50 S per round globally, 10 S per Actor. Over-subscription pro rata.
- **Payee:** WORLD_HOUSEHOLDS, standing in for the media sector.

### 3.5 Defence-goods market

**Market ID:** `defence-goods`, an exchange Market ([Type System §5.2](02_type_system.md#52-exchange-markets)) trading Military for Money.

| Market ID | Base / quote | Money reserve | Military reserve | Access | Operational interval |
|---|---|---:|---:|---|---|
| `defence-goods` | Mil / M | 300 M | 60 Mil | US, DE, UK, JP | −80 ≤ x ≤ 20 |

**Cost:** p(x) = 3 + 0.01x, **C(x) = 3x + 0.005x²**, where x is the Market's cumulative net Military sold. The price runs from 2.2 to 3.2 across the interval. The initial price is 3 M/Mil. The upper bound sits 40 Mil below the Military reserve (the Energy Markets sit 50 E below theirs), and the lower bound is −80. The Money reserve pays sellers.

**Access:** the US and its allies (Germany, the UK, Japan), each holding the `trade defence-goods` capability, so the access rule and the capabilities agree. No other Entity may trade on it.

**Orders:** as on the Energy Markets. A sale is a negative quantity whose limit is the minimum average price, and it escrows its Military at screening. A Military sale is a Trade, not a Transfer: the Market is the counterparty ([§2.3](02_type_system.md#23-resource)).

## 4. Transformations

| Transformation | Inputs | Outcome | Payee |
|---|---|---|---|
| **Military Procurement** | Money + 2 E per Mil | Military delivered next round | DEF, or CN_PRIVATE for CN ([§3.3](#33-defense-procurement-facility)) |
| **Public Campaign** | Money | Soft Power | WORLD_HOUSEHOLDS |
| **Humanitarian Aid** | 10 M + 5 E | 4 S to the provider. The 5 E is consumed in relief. At most twice per Actor per round | WORLD_HOUSEHOLDS |
| **Military Deployment** (`military-deployment`, uncapped) | Reserves 1 Mil per unit. No input | Reservation for the round, released at commit. Nothing consumed or produced ([§13.3](#133-military-deployment)) | - |
| **Major Military Action** (`major-military-action`, uncapped, minimum 12) | 1 Mil per unit, 0.75 returned at commit | Reservation for the round. 25% attrition ([§13.3](#133-military-deployment)) | - |
| **Energy Shipping** | 0.3 M per E, paid by the sender. SHIP must sign | Makes a bundled cross-region Energy Transfer valid ([§3.2](#32-transport-capacity-and-trade-restrictions)) | SHIP |
| **Logistics Investment** | 10 M + 3 E (SHIP only) | +10 E/round `energy-shipping` capacity from next round | GLOBAL_SMES |
| **Energy Capacity Investment** | 12 M + 3 E + 1 semiconductor unit bought from CHIP by signed Proposal (producers only) | +2 E/round production capacity from next round | GLOBAL_SMES |
| **Strike** | 8 S + 5 M strike pay (LABOR only) | Next round: ENCO production capacity −6.25 E/round and CHIP semiconductor capacity −3.75 units (which back procurement) | US_WORKERS |

Investments change future capacity. They never create Energy or Military directly.

## 5. Intents and Capabilities

Meridian uses the four Intent categories ([Type System §3](02_type_system.md#3-intents-and-economic-actions)). Joint Proposals between players are Proposals bundling each side's Transfers ([§13.5](#135-proposals-and-signatures)). Governments act only within their jurisdictions. Corporations trade and transform the capacities they own but cannot regulate or deploy Military. Civil organizations and the small-business owner act through their own holdings, Soft Power, strikes, aid, signed joint Proposals, and Representation, and never acquire the authority of the populations they represent ([INV-03](02_type_system.md#inv-03)).

---

# Part II: Gameboard Configuration

Part II lists Meridian's Verticals, Actors, Aggregates, Policies, and rules. Gameboard configuration is in [Gameboard](03_gameboard.md). Instance initialization is in [Instantiated Game](04_instantiated_game.md).

## 6. Setting: Meridian

### 6.1 Setting

A hypothetical near-future economy of intensifying competition between the United States and China, divided regional energy markets, and unequal access to industrial and military capability. Europe must coordinate while keeping national priorities. The setting asks whether participants can preserve cooperation under conditions that reward competition.

### 6.2 Time horizon

Five simulated years, played as five one-year rounds. Other instantiations may use four to eight rounds. Scaling annual flows to round length is planned ([INV-28](02_type_system.md#inv-28)) and not yet implemented.

### 6.3 Central tensions

| Tension | Mechanism |
|---|---|
| **Geopolitical competition** | Procurement, deployment, China's conditional Policy, shipment bans |
| **Energy security** | Regional nodes, production and transport capacity, tariffs, the Round 3 disruption |
| **European coordination** | Germany's signatures on the EU's procedure Proposals |
| **Distributional conflict** | Wage, tax, and spending flows. DEF's variable wages. LABOR's strike. Household and small-business exposure to Energy prices |

## 7. Economic Verticals

Four provisional Verticals, placeholders for research-backed vertical reports, share Resources, Markets, and capacities. The engine does not record them.

| Vertical | Resources and state | Mechanism |
|---|---|---|
| **Energy and Industry** | Energy, Money, production capacity, semiconductor capacity | Production, investment, industrial dependency |
| **Security and Defense** | Military, Energy, Money, procurement capacity | Procurement, deployment, attrition |
| **Finance and Trade** | Money, Energy, transport capacity, tariffs | Circulation, regional exchange, restrictions |
| **Information and Diplomacy** | Soft Power, Money | Campaigns, aid, strikes |

Procurement consumes Energy and depends on semiconductor capacity, so Energy scarcity constrains military capacity. These effects arise from mechanisms, not scripted events.

## 8. Economic Circulation

Each round's single update evaluates the economic cycle together with every other action ([Type System §8.2](02_type_system.md#82-single-round-update-and-snapshot-discipline)). All amounts are annual and provisional.

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

Consumed at commit from each Entity's net Energy position, evaluated on the batch result (each a recurring flow of [02 §6](02_type_system.md#6-economic-circulation), its shortfall `moved < owed` on its leg):

| Entity | E/round |
|---|---:|
| US, CN | 10 each |
| DE, IN, JP, EU | 8 each |
| UK, CHIP | 6 each |
| SHIP | 4 |
| **Explicit and bloc subtotal** | **68** |
| CN_PRIVATE, US_WORKERS, WORLD_HOUSEHOLDS, GLOBAL_SMES | Their adjusted purchase target (baseline 12 + 8 + 12 + 40 = 72) |

At baseline, production equals operating requirements. The Round 3 disruption ([§13.6](#136-energy-disruption)) is the designed scarcity.

Aggregate market demand is 80 E per round (EU 8, CN_PRIVATE 12, US_WORKERS 8, WORLD_HOUSEHOLDS 12, GLOBAL_SMES 40) against `ENERGY_SUPPLIERS`' offer of 40 E ([§11.4](#114-other-aggregate-policies)). Prices therefore rise unless the explicit producers (SA, ENCO, BR, NG, IN, 80 E) sell roughly 40 E per round. This is the producers' deliberate leverage.

**Shortfall rule:** each unit of unmet requirement reduces the Entity's fixed incoming transfers ([§8.3](#83-fixed-income-and-expenditure)) by 2 M, which the payers keep.

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

The schedule conserves Money. CHIP's supply contract covers its wages. Further CHIP revenue comes from signed Proposals.

### 8.4 Variable flows

Energy sales pay sellers the node's uniform price. ENERGY_SUPPLIERS pays 50% of its net Energy-sale revenue to WORLD_HOUSEHOLDS (producer recycling). Other variable flows go to the payees in [§3](#3-markets-and-facilities) and [§4](#4-transformations).

### 8.5 Capacity renewal and reconciliation

Investment in round r changes capacity from the Prepare phase of round r + 1, recorded with its originating Intent.

At the end of every round the engine checks [INV-05](02_type_system.md#inv-05): total Money is unchanged. The change in total Energy equals production minus consumption, procurement, aid, and investment. The change in Military equals deliveries minus attrition. The change in Soft Power equals production minus spending minus decay. Fixture `circulation` checks five rounds with no other action: US Money 260 → 300, and DE Money 125 → 131 with Energy 0, since DE's 8 E requirement exceeds its stock in round 5 (2 E unmet withholds 4 M of its incoming transfers).

## 9. Actor Catalog

The reference instantiation selects all fifteen seat roles plus China as an NPC. This table is the one description of each Actor. Quantities appear once, in the section named.

| ID | Actor | Category | Region | Distinctive capability | Dependency | Objective ([§9.3](#93-objectives)) |
|---|---|---|---|---|---|---|
| US | US Head of State | Government | Atlantic | Tariffs, shipment bans, deployment, Military sales. Largest procurement budget | Energy. Procurement capacity | Military leadership, solvent |
| DE | German Chancellor | Government | Atlantic | Signs EU procedure Proposals. Optional solidarity contributions | Atlantic Energy. EU residual tax revenue | Energy security |
| IN | Indian Prime Minister | Government | Asia | Energy production ([§8.1](#81-energy-production)). Tariffs | Asia Energy prices | Industrial autonomy |
| JP | Japanese Prime Minister | Government | Asia | Tariffs. Procurement. Large Asia purchasing | Imports and transport. No production | Energy security, credible defense |
| UK | UK Prime Minister | Government | Atlantic | Tariffs. Procurement. Campaigns | Atlantic Energy | Diplomatic influence |
| BR | Brazilian President | Government | Atlantic | Energy production. Capacity investment | Semiconductor units | Stronger finances |
| SA | Saudi Energy Minister | Government | Asia | Largest government production | Transport for Atlantic sales | Energy wealth |
| NG | Nigerian President | Government | Atlantic | Energy production. Capacity investment | Semiconductor units. Investment | National capacity |
| ENCO | US Energy Consortium CEO | Corporation | Atlantic | Energy production. Capacity investment | Semiconductor units. Strikes | Profit and supply growth |
| CHIP | Semiconductor Producer CEO | Corporation | Asia | Semiconductor capacity behind procurement ([§2.1](#21-economic-state-that-is-not-a-resource)). May withhold or sell units | Operating Energy | Secure Energy inputs |
| SHIP | Shipping Operator CEO | Corporation | Both | Owns `energy-shipping` ([§3.2](#32-transport-capacity-and-trade-restrictions)). Chooses which shipments to sign. Arbitrage | Fuel. Shipment bans | Open routes, high throughput |
| DEF | Defense Manufacturer CEO | Corporation | Atlantic | Operates procurement. Receives non-Chinese revenue | CHIP's semiconductor capacity. Strikes | Procurement revenue |
| LABOR | US Labor Federation Chair | Civil institution | Atlantic | Strike ([§4](#4-transformations)), declared or withheld each round | Dues. Soft Power | Workers' position and influence |
| SME | Small-Business Owner | Small participant | Atlantic | Representation of Global SME demand ([§12.2](#122-small-business-owner--global-smes-representation)) | Atlantic prices. Small cash | Business viability |
| AID | Humanitarian Alliance Director | Civil institution | Atlantic | Aid. Energy relief to Aggregates in shortfall. Soft Power | EU donations. Energy purchases | Limit crisis damage |
| CN | Chinese Head of State | Government | Asia | Tariffs, shipment bans, deployment | Asia Energy prices. CN_PRIVATE tax base | Military parity, solvent |

### 9.1 Semiconductor capacity

CHIP may enact a `Withholding` rule on some units ([§2.1](#21-economic-state-that-is-not-a-resource)). Compensation is a Transfer or signed Proposal. An Energy capacity investment needs 1 unit bought from CHIP by signed Proposal. Each unit so consumed counts against CHIP's units and cuts that round's procurement capacity by 2 Mil.

### 9.2 Fallbacks

| Actor | Fallback when not explicit |
|---|---|
| US, CN | Required explicit. Human or NPC controller |
| DE | Government accounts held in the EU Aggregate as member-government accounts. Germany's weight folds into the EU's residual weights ([§14.2](#142-resolution-consistency)) |
| IN, JP, UK, BR, SA, NG | `government-default-v0` controls the same accounts: baseline spending, no discretionary actions |
| ENCO, CHIP, SHIP, DEF | `firm-default-v0` controls the same accounts and capacities: sells Energy above 30 E on every Energy Market it has access to (its own node, for each of these firms) and signs `energy-shipping` Proposals |
| LABOR | Implicit coverage: accounts merge into US_WORKERS. No strike capability |
| SME | Implicit coverage: accounts merge into GLOBAL_SMES. No Representation |
| AID | Implicit coverage: accounts merge into WORLD_HOUSEHOLDS, which also receives EU donations |

### 9.3 Objectives

Each Actor's objective is data: measurable criteria over the recorded game ([Gameboard §10.5](03_gameboard.md#105-evaluation)). The criteria are provisional, never affect the economy, and are reported per Actor as met or not met, with values.

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
| AID | Total Aggregate shortfall across the game ≤ 20 E. Final Soft Power ≥ 55 |

Military and Soft Power matter through these objectives (US, CN, JP, UK, LABOR, AID) and through the deployment and strike mechanics.

## 10. Aggregate Catalog

| ID | Aggregate | Residual accounts | Region | Baseline responsibility |
|---|---|---|---|---|
| EU | European Union | Non-explicit member economies and bloc accounts | Atlantic | Energy purchasing. Collective decisions |
| EU_POOL | EU Solidarity Pool | The energy the `eu-solidarity` procedure draws on first | Atlantic | None. Acts only through the EU's procedure |
| CN_PRIVATE | Chinese Private Economy | Chinese activity outside the government | Asia | Industrial demand. Chinese defense production |
| US_WORKERS | US Workforce | Residual worker accounts | Atlantic | Household demand |
| GLOBAL_SMES | Global Small Businesses | Small businesses outside explicit ones | Atlantic | Energy demand. Investment supply |
| ENERGY_SUPPLIERS | Other Energy Producers | Production not held by explicit Actors | Both | Energy supply |
| WORLD_HOUSEHOLDS | Other World Households | Residual household activity | Asia | Household demand. Media and relief payee |

Each Aggregate's members, accounts, and collective procedures are defined separately ([Type System §2.2](02_type_system.md#22-three-projections-of-an-entity)). No account has two owners ([INV-08](02_type_system.md#inv-08)). The catalog has seven Aggregates: the six above and EU_POOL.

**EU_POOL is an account, not an Actor, and it has no capabilities by design.** It is the one Entity with Atlantic market access and no `trade` capability. Its access lets the pool-to-EU transfer count as a same-region transfer (both ends in the Market's access list). Its authority comes from the EU's `eu-solidarity` procedure, which `authorizes` the transfer and the trade; the pool's own policy (`solidarity-pool-v0`) has no rules and no signatures. Fixture B2-pool exercises the pool-to-EU transfer, so the access-without-capability case is tested there.

### 10.1 Germany and the EU

- **Membership.** Germany is listed in the EU's `members` whether or not it signs anything in a given round.
- **Account ownership.** Making the Chancellor explicit extracts only the German government's accounts. German households and firms stay in the EU residual, which is why German tax revenue flows EU → DE ([§8.3](#83-fixed-income-and-expenditure)). The EU also owns a **solidarity pool**, initially empty: a separate account with no policy, held by the Entity `EU_POOL`, which acts for the EU only inside its solidarity procedure ([§12.1](#121-germany--eu-collective-procedure)).
- **Collective procedures.** The EU's capabilities are exercised by its two procedures, which Germany influences by signing ([§12.1](#121-germany--eu-collective-procedure)).

### 10.2 Small-business representation

The owner is a member of `GLOBAL_SMES`. Their holdings and direct transactions are excluded from the residual. Their proposed decisions serve as bounded evidence through a Representation ([§12.2](#122-small-business-owner--global-smes-representation)).

## 11. Policy Catalog

Each declarative Policy runs a Strategy: a rule set over World State ([Type System §4](02_type_system.md#4-policies)). Memory such as "the US conducted a major deployment last round" is read from World State and the public event record.

### 11.1 China NPC Policy: `china-strategic-v0`

1. If the US ran `major-military-action` of at least `major-action-threshold` (12 Mil in the reference game) in the previous round, order 4 Mil from the procurement facility.
2. If the beginning-of-round `energy-asia` price exceeds 3.2 M/E, offer up to 12 E on that node.
3. If a US restriction targeting China was enacted in the previous round, enact the reciprocal restriction, effective at the next round boundary.
4. China's budget covers procurement and its discretionary Energy trades in the reference game. If a variant exceeds it, the engine scales China's actions pro rata ([Type System §5.5](02_type_system.md#55-simultaneous-clearing)). A priority order would be a setting-level addition.

The NPC does not improvise outside its declared Policy.

### 11.2 EU Aggregate Policy: `eu-coordination-v0`

- Buy 8 E per round on `energy-atlantic`. 4 E when the beginning-of-round price exceeds 3.0 M/E.
- Publish the solidarity and trade procedure Proposals when their conditions hold ([§12.1](#121-germany--eu-collective-procedure)).
- Pay the fixed flows in [§8.3](#83-fixed-income-and-expenditure).

### 11.3 Global Small-Business Policy: `global-smes-v0`

Baseline demand of 40 E per round on `energy-atlantic`, reduced by 20% when the beginning-of-round price exceeds 3.5 M/E. The target is that baseline multiplied by the factor stored in the previous round ([§12.2](#122-small-business-owner--global-smes-representation)), or the baseline alone when none was stored. The target is both the purchase order and the operating requirement.

### 11.4 Other Aggregate Policies

| Aggregate | Policy |
|---|---|
| CN_PRIVATE | Buys 12 E on `energy-asia`. 9 E above 3.0 M/E |
| US_WORKERS | Buys 8 E on `energy-atlantic`. 6 E above 3.0 M/E |
| WORLD_HOUSEHOLDS | Buys 12 E on `energy-asia`. 9 E above 3.0 M/E |
| ENERGY_SUPPLIERS | Produces 60 E. Offers 40 E (24 Atlantic / 16 Asia), or 50 E (30 / 20) at a node whose beginning-of-round price is at least 2.6 M/E. Retains unsold output |

Each household and industrial Aggregate's purchase target is also its operating requirement. `government-default-v0` and `firm-default-v0` are fallbacks, inactive in the reference instantiation.

## 12. Representation and Collective Procedures

Explicit Actors influence Aggregates through two mechanisms. In neither does the engine create or change a Proposal.

| Mechanism | Instance | How it works |
|---|---|---|
| Collective procedure | `eu-solidarity`, `eu-trade` (held by the EU) | Weighted signing on the EU's own Proposal, decided and executed in the same round |
| Representation | `sme-energy-sample` | Shrunk, clamped deviation of a sampled representative: observation → estimate in round r, then proposed Intent → executed result in round r+1 ([INV-09](02_type_system.md#inv-09)) |

### 12.1 Germany → EU (collective procedure)

Each procedure is a weighted-signature rule on Proposals the EU's Policy publishes in the EU's name. No Representation or Estimator is involved.

| Procedure | Authorizes | Member weights | Residual weight | Threshold |
|---|---|---|---:|---:|
| `eu-solidarity` | Energy sales on `energy-atlantic` | DE 0.20 | 0.40 | 0.45 |
| `eu-trade` | `Enact Tariff` | DE 0.20 | 0.48 | 0.45 |

**Support = residual weight + the weights of the members who signed.** A procedure Proposal executes in the round it is published if support exceeds the threshold. Otherwise it lapses. Germany supports by signing in the Sign step and declines by not signing. Each round's record lists the Proposal, signers, support, threshold, and outcome.

The EU's Policy decides when to publish ([§11.2](#112-eu-aggregate-policy-eu-coordination-v0)):

- **Solidarity Proposal:** sell up to 8 E on `energy-atlantic`, drawing first on the pool and then on its holdings. The pool's transfer is scaled to what it holds, so the pool gives min(8, pool) and the EU sells 8 E. Published in any round whose beginning-of-round Atlantic price exceeds 3.0 M/E, provided the EU's holdings cover its operating requirement of 8 E.
- **Trade Proposal:** enact a matching import `Tariff` of 0.5 M/E on cross-region Energy from the restricting jurisdiction, in force from the next round. Published in any round in which a US or Chinese restriction is in force and no EU tariff is. The trigger is any US or Chinese restriction, in any region.

| Procedure | Germany signs | Germany does not sign |
|---|---|---|
| `eu-solidarity` | 0.40 + 0.20 = 0.60: executes | 0.40: lapses |
| `eu-trade` | 0.48 + 0.20 = 0.68: executes | 0.48: executes |

Germany is pivotal for solidarity and has no veto on trade. Germany may also Transfer Energy into the solidarity pool at any time, independent of signing. That Transfer is an ordinary Proposal by Germany, made in Fixture B1 rather than by its default Policy. This is a stylized procedure, not a claim about EU voting law.

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

ε is drawn in the observation's round under the key `round-n / sme-energy-sample / SME / sampling / 0` ([Type System §9.1](02_type_system.md#91-numerics-and-randomness)), and only when the owner submits an observation.

| Proposed purchase (round r) | Deviation | Next-round target, noise-free (ε = 0) | Range over ε (baseline 40 E) |
|---:|---:|---:|---:|
| 4 E | −20% | 40 × 0.95 = 38 E | 37–39 E |
| 0 E | −100% | 40 × 0.875 = 35 E | 35 E (clamp absorbs the noise) |
| 10 E | +100% | 40 × 1.125 = 45 E | 45 E (clamp absorbs the noise) |

The stored factor replaces the next round's baseline exactly once. Round r uses the baseline, or the factor stored in round r−1. The owner's own purchase executes separately in round r. The same decision moves the Aggregate by slightly different amounts in different games, but identically on replay with the same game seed.

### 12.3 Incentives and bounds

The owner knows their choice moves the Aggregate, so it is a strategic choice, not an independent sample. That is intended: shrinkage and the clamp keep influence explicit and bounded, and shrinkage is a balancing parameter, not a population estimate. The simulator reports the [Representation influence ratio](02_type_system.md#25-glossary). In the noise-free 4 E case, 2 E against 4 E gives 0.5. Across ε it ranges from 0.25 to 0.75.

## 13. Setting Rules

### 13.1 Round resolution

Each round is one simulated year and follows the Type System canonical sequence ([§8.1](02_type_system.md#81-canonical-resolution-sequence)). [§20](#20-phase-mapping) gives the Meridian content of each phase. Prices produced by a round become observable in the next round.

### 13.2 Institutional changes

Tariffs, shipment bans, withholding, and strikes take effect at the next round boundary. A procedure Proposal is published, signed, and executed within one round. A tariff it enacts in round r is in force from round r+1.

### 13.3 Military deployment

`military-deployment` reserves 1 Mil per unit for the round: the reserved Mil is unavailable to the same Entity's other actions, is not consumed, and is released at commit ([Type System §2.3](02_type_system.md#23-resource)). `major-military-action` (minimum 12 Mil) consumes 1 Mil per unit and returns 0.75 at commit, so a major action loses 25% of its units. A smaller order is rejected (Fixture D-minor). China's NPC rule reads only `major-military-action` ([§11.1](#111-china-npc-policy-china-strategic-v0)), so a reservation alone does not trigger it. All of these values are provisional.

**Fixture R (golden).** Round 1: the US orders a 60 Mil deployment and sells 40 Mil on the `defence-goods` Market at a limit of 2.4 M/Mil. Their claims on the US's 95 Mil total 100, so both scale by 95/100: the deployment reserves 57 and the sale sells 38 Mil. The Market is fresh, so the uniform price is (C(−38) − C(0)) / −38 = 2.81, and the sale pays 38 × 2.81 = **106.78 M**. US Military is 95 − 38 = **57** (the reserved 57 are not consumed) and US Money is 260 + 106.78 = **366.78**. Round 2: the reservation is released, so a 50 Mil Transfer to Japan runs in full, leaving US Military at **7** and Japan at **86**.

**Fixture R-alone (golden).** The same sale with no deployment runs in full: 40 Mil at (C(−40) − C(0)) / −40 = 2.80, paying 112 M. US Military is **55** and US Money **372**. The reservation leaves the sale 2 units short.

**Deployer preset (simulator test 7c).** The shipped `Deployer` preset ([06 §4.2](06_simulator.md#42-presets)) orders the same pair while the Atlantic price is above 3. In a five-round game with the other seats Passive, the Atlantic price is 2.97 at the start of round 3 and 3.29 at the start of round 4, so the preset acts in rounds 4 and 5. Round 4 matches Fixture R. Round 5 starts with 57 Mil against 100 claimed, so both scale by 0.57: the deployment reserves 34.2 and the sale sells 22.8 Mil at (C(−60.8) − C(−38)) / −22.8 = 2.506, which clears the 2.4 limit. US Military ends at **34.2**.

### 13.4 Soft Power

Soft Power is produced by campaigns and aid, spent on strikes, and decays at the rate in [§2](#2-resources). Final holdings count toward several objectives ([§9.3](#93-objectives)).

### 13.5 Proposals and signatures

Out-of-band joint Proposals enter as Proposals whose signers are every party. Fixture G shows the settle and lapse cases ([INV-10](02_type_system.md#inv-10)).

### 13.6 Energy disruption

The only scheduled event: in Round 3, `ENERGY_SUPPLIERS`' Policy reads the `disruption-round` parameter, and a logistics disruption cuts `ENERGY_SUPPLIERS`' maximum offer by 20 E (12 Atlantic, 8 Asia). A cut never takes the offer below zero, so a cut at or above the offer removes it and never turns it into a purchase. Production continues. Unreleased output accumulates in its holdings. It is predeclared configuration, not an AI intervention.

**Major shock (designed test case).** `Settings.Meridian.majorShock` is the reference disruption moved to Round 1 with both cuts at or above the offers: Atlantic 60, Asia 40. Each region's supply is zero that round. It is not the default game. It flips the Atlantic price across 3.0 in both directions, and four presets' Atlantic actions with it. With every seat on `Conservative`, the end-of-round Atlantic prices are calm 2.89, 3.14, 3.32, 3.12, 2.86 and shocked 3.07, 3.19, 3.11, 2.92, 2.77. Asia moves from 2.16–2.80 to 2.48–2.88. With every seat on one preset, Accumulator, Trader, and Deployer change their proposals in Rounds 2, 3, and 5, and Opportunistic in Rounds 2 to 4. The EU's solidarity Proposal is published in Round 2 and not in Round 5. In the `Conservative` run the Atlantic price never reaches 3.5, so CrisisResponse does not flip. [Test 8b](06_simulator.md#14-mandatory-tests) pins the flip.

### 13.7 Evaluation

Actor objectives ([§9.3](#93-objectives)) plus the system metrics in [§30](#30-measurements). No single winner. Outcomes are not reduced to Money.

---

# Part III: Instantiated Game Configuration

## 14. Game Settings

| Parameter | Value |
|---|---|
| Game ID | `meridian/meridian-test-01`: the Gameboard id, then the test-instance label. Every run records it in its spec. |
| Gameboard | `meridian`, version `v0` |
| Seats / NPC Actors / Aggregates | 15 / 1 (China) / 7 |
| Horizon and rounds | 5 years. 5 rounds of 1 year ([§6.2](#62-time-horizon)) |
| Real-world session | About 6 hours ([§19](#19-session-structure)) |
| Game seed | `26021` (the only in-world seed, its only use in v0 is SME sampling noise) |
| Player seed (simulation only) | `1` by default for reference simulation runs. Varies seat-Policy randomness only |
| Reports | Fictional financial-news front page and Actor balance sheets |

### 14.1 Seat assignments

Seats 1–15 drive, in order: US, DE, IN, JP, UK, BR, SA, NG, ENCO, CHIP, SHIP, DEF, LABOR, SME, AID. In live play each seat's Policy is a human. In simulation it may be an AI agent or a preset Strategy. Rebinding a seat requires no Gameboard change.

China is Actor `CN` with controller `NPC china-strategic-v0`. It has no seat. Germany is a seat. The EU is an Aggregate under `eu-coordination-v0` with Germany among its members and both collective procedures active. No human controls it. The `sme-energy-sample` Representation is active.

### 14.2 Resolution consistency

With Germany implicit, the German government's accounts sit in the EU Aggregate ([§9.2](#92-fallbacks)). Before decisions differ, totals must reconcile ([INV-14](02_type_system.md#inv-14)):

| Account | Germany explicit (DE + EU) | Germany implicit (EU) |
|---|---:|---:|
| Money | 125 + 320 = 445 | 445 |
| Energy | 38 + 125 = 163 | 163 |
| Military | 24 + 50 = 74 | 74 |
| Soft Power | 45 + 75 = 120 | 120 |

Implicitly, the EU → DE and DE → EU flows become internal and net to zero. DE's 8 E requirement joins the EU's (16 E total). Germany's 0.20 weight joins the EU's residual weight for each procedure ([§12.1](#121-germany--eu-collective-procedure)). And every capability keeps the same owner.

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

Aggregates are in italics. Government holdings cover only government-controlled resources. The German government's holdings are excluded from the EU and German private accounts are included. The owner's holdings are excluded from Global SMEs. China's government and private economy are separate. ENCO's holdings and capacity are excluded from Other Energy Producers ([INV-08](02_type_system.md#inv-08)). The solidarity pool (`EU_POOL`) starts at 0 E, as its own account.

## 16. Initial Market and Capacity State

| Mechanism | Initial state |
|---|---|
| Regional Energy Markets and `defence-goods` | Position x = 0. Reserves and opening prices as in [§3](#3-markets-and-facilities) |
| Defense procurement, public attention, semiconductor capacity | 0 orders. Capacities as in [§2.1](#21-economic-state-that-is-not-a-resource), [§3.3](#33-defense-procurement-facility) and [§3.4](#34-public-attention-facility) |
| `energy-shipping` and production | Capacities as in [§3.2](#32-transport-capacity-and-trade-restrictions) and [§8.1](#81-energy-production) |
| Rules | No `Tariff`, `AccessBan`, or `Withholding` in force |

Market reserves are distinct from Entity holdings. Facilities pay their payees directly.

## 17. Additional Parameters

Every numerical parameter is stated where it takes effect (§§3, 8, 11–13). None requires a Type System change.

## 18. Round Schedule

| Round | Year | Central test |
|---|---|---|
| 1 | 1 | Market discovery, diplomacy, allocation |
| 2 | 2 | Policy responses, first deliveries, investment, first signed joint Proposals |
| 3 | 3 | Energy disruption and strategic stress |
| 4 | 4 | Adaptation, military competition, recovery or decline |
| 5 | 5 | Final decisions and resolution |

Themes indicate opportunities, not mandatory events.

## 19. Session Structure

A provisional six-hour session: 20 minutes of briefing, five 60-minute rounds (about 40 minutes negotiation, 7 minutes of audio submissions, after which humans give no further input and transcription and interpretation run before the propose deadline, 3 minutes signing, ending at the sign deadline, 10 minutes resolution and reporting), 20 minutes closing analysis, and 20 minutes of breaks.

## 20. Phase Mapping

The canonical phases are defined in [Type System §8.1](02_type_system.md#81-canonical-resolution-sequence). This table gives only the Meridian content of each.

| Canonical phase | Meridian content |
|---|---|
| **1. Prepare round** | Deliver production and procurement. Apply capacity renewals. Activate rule changes and strikes. Apply the Round 3 disruption |
| **2. Create observations** | Authorized views, including both regional prices and the SME factor stored last round |
| **3. Generate Proposals** (round runner) | 3a Propose: all Policies at once (seats, with human text interpreted in live play, China, Aggregate Policies, applying last round's stored factor, the EU's procedure Proposals when conditions hold). 3b Sign: each required signer signs the Proposals it endorses, including Germany on EU procedure Proposals |
| **4. Screen and interpret** | Authority, signature rules, and escrow ([§12.1](#121-germany--eu-collective-procedure) for thresholds). The owner's observation |
| **5. Estimate** | Compute and store the SME adjustment factor for use next round. No Proposal is created or changed |
| **6. Resolve economy** | One batch: joint feasibility against escrow. Uniform clearing of both nodes. All actions and circulation evaluated together. Operating requirements and decay applied at commit |
| **7. Commit and record** | New World State, events, diagnostics, balance sheets. AI front page from recorded events |

---

# Part IV: Playtest Fixtures

Every fixture is filtered from the reference setting with `restrict` ([03 §12a](03_gameboard.md#12a-how-settings-are-made)) plus named patches: an Entity's capabilities, a stated account or Market position, the fixture's Rules and Representations. Its decisions are its Proposals, so a fixture keeps no Actors, Policies or seats. Expected values stay literal.

Each fixture is a `Setting`: a literal setting (`LiteralSetting`) that writes its World State out in full. Each seat that proposes or signs in its rounds is bound to a `Script` of those answers, and an `Expect` gives the values each round is checked against. The expected values are literals taken from the worked examples below, never computed. The families are in `packages/premises/src/Settings/Fixtures/`, and each compiles to `packages/premises/fixtures/<name>.json`. The file has three parts: `setting` (the seed and the World), `bindings` (one per round, in order, each with its Proposals), and `expectations` (each naming its round and giving its `expect`).

Unless stated otherwise, fixtures disable circulation, production, and operating requirements to isolate the mechanism under test. Amounts are exact at the precision of Type System [§9.1](02_type_system.md#91-numerics-and-randomness).

| Fixture | Isolates | Section |
|---|---|---|
| A, A2 | Direct Market exchange. Limit prices | [§21](#21-fixture-a-direct-market-exchange) |
| B1–B4, B2-pool, B4-unsigned | Germany's signature against EU procedure support. The solidarity pool | [§22](#22-fixture-b-germany-and-the-eu-collective-procedure) |
| C1–C5 | Bounded Representation and its one-round lag | [§23](#23-fixture-c-small-business-owner-representation) |
| D, D-minor | China's NPC response. Minimum major action | [§24](#24-fixture-d-china-responds-as-an-npc) |
| R, R-alone | A deployment reserving Mil against a Military sale | [§13.3](#133-military-deployment) |
| E | Scarcity and restriction propagation (qualitative) | [§25](#25-fixture-e-scarcity-and-trade-restriction) |
| F | Conflicting orders. Deterministic allocation | [§26](#26-fixture-f-simultaneous-conflicting-orders) |
| G1–G5, G3-ban, G5-unsigned | Atomic settlement, lapse, and access bans | [§27](#27-fixture-g-proposals-and-signatures) |
| circulation | Five rounds of circulation and reconciliation | [§8.5](#85-capacity-renewal-and-reconciliation) |

## 21. Fixture A: Direct Market Exchange

`energy-atlantic` starts at x = 0. The US buys 10 E. Germany sells 5 E. Net demand is 5 E.

**C(5) − C(0) = 2 × 5 + 0.01 × 25 = 10.25 M**, uniform price 10.25 / 5 = **2.05 M/E**.

| Entity | Money before | Money after | Energy before | Energy after |
|---|---:|---:|---:|---:|
| US | 260 | 239.50 | 70 | 80 |
| Germany | 125 | 135.25 | 38 | 33 |
| `energy-atlantic` | 700 | 710.25 | 350 | 345 |

Money totals 1,085 before and after. Energy totals 458 before and after.

**A2 (limit price).** The US adds a limit of 2.04. At the trial price 2.05 its order is removed. Net demand becomes −5 E: price = (C(0) − C(−5)) / 5 = (0 − (−9.75)) / 5 = **1.95 M/E**. Germany receives 9.75 M (ending at 134.75 M and 33 E). The US order is recorded as rejected and US balances are unchanged.

Verifies [INV-04](02_type_system.md#inv-04), [INV-06](02_type_system.md#inv-06), [INV-11](02_type_system.md#inv-11). Limit handling.

## 22. Fixture B: Germany and the EU (collective procedure)

**B1: contribution, normal prices.** The Atlantic price is at or below 3.0, so the EU publishes no solidarity Proposal. Germany transfers 10 E to the pool. DE Energy 38 → 28. Pool 0 → 10 E. The EU's own account stays at 125 E. EU holdings including the pool are 135 E.

**B2: Germany signs, high prices.** Atlantic price above 3.0 (B2, B2-pool and B3 start the node at x = 60, price 3.2). The EU publishes the solidarity Proposal and Germany signs it, transferring nothing. Support = 0.40 + 0.20 = 0.60 > 0.45, so the EU's offer of up to 8 E enters this round's clearing. The pool is empty, so its leg gives nothing and the EU sells from its own holdings: EU Energy 125 → 117, Money 320 → 344.96.

**B2-pool: the pool drawn first.** As B2, with 10 E in the pool. The pool gives 8 E to the EU, which sells 8 E: pool 10 → 2, EU account 125 unchanged.

In both B2 and B2-pool the Atlantic reserves end at Money 675.04 and Energy 358.

**B3: Germany does not sign, high prices.** Support = 0.40. The Proposal lapses and the round resolves.

**B4: `eu-trade`.** A US shipment ban on China is in force, so the EU publishes the tariff Proposal. Support is 0.68 with Germany's signature and 0.48 without ([§12.1](#121-germany--eu-collective-procedure)). Either way it passes and the tariff is in force next round.

Verifies that signing and transfers are independent, that a procedure Proposal executes only when support exceeds its threshold, and that Germany never controls EU holdings or authority ([INV-03](02_type_system.md#inv-03), [INV-08](02_type_system.md#inv-08), [INV-10](02_type_system.md#inv-10)).

## 23. Fixture C: Small-Business Owner (Representation)

[Fixtures C1](#23-fixture-c-small-business-owner-representation)–C3 set the noise spread to 0 so their values are exact. C5 checks the seeded noise.

In round r the owner submits a purchase of 4 E (reference 5 E). Deviation −20%, within the clamp, so the stored factor is 0.95. Global SMEs uses its 40 E baseline in round r. In round r+1 its target is 40 × 0.95 = **38 E**, replacing the 40 E baseline. The owner's 4 E executes separately in round r, at 2.04 M/E for 8.16 M: Money 15 → 6.84, Energy 5 → 9.

- **C2 (clamp):** owner proposes 0 E → clamped to −50% → factor 0.875 → 35 E next round.
- **C3 (proposed vs executed):** owner proposes 4 E with a limit violated at clearing. The observation is still 4 E and the next-round target is 38 E. The record shows 4 E proposed, 0 E executed.
- **C4 (high price):** when the round r+1 price is above 3.5 M/E the baseline is 32 E. The 0.95 factor from a 4 E observation gives 32 × 0.95 = 30.4 E. This is a golden test case on the adjustment arithmetic, not a fixture file.
- **C5 (seeded noise):** noise spread 0.10, game seed `26021`. The recorded draw is u = 0.2158, so ε = 0.10 × (2u − 1) = −0.0568 and the factor is 1 + 0.25 × (−0.20 − 0.0568) = 0.9358 (quantities carry four decimals). The next-round target for a 4 E observation lies in [37, 39] E and equals 40 × (1 + 0.25 × (−0.20 + ε)) for the recorded draw. Replaying with the same seed gives the identical target. A different seed generally gives a different target in the same range. A 0 E proposal still gives exactly 35 E.

Verifies bounded extrapolation, separate ownership, and no double-counting ([INV-08](02_type_system.md#inv-08), [INV-09](02_type_system.md#inv-09)).

## 24. Fixture D: China Responds as an NPC

**Round 1.** The US deploys 12 Mil (major action). Attrition 25% × 12 = 3. 9 Mil return at round end. US Military 95 → 92.

**Round 2.** China observes the recorded action and orders 4 Mil. No other orders.

- Cost C(4) = 32 + 4 = **36 M**, uniform price 9 M/Mil. Energy 8 E.
- China: Money 255 → 219. Energy 95 → 87.
- Payee CN_PRIVATE: 210 → 246.

**Round 3 Prepare.** 4 Mil delivered: China 88 → 92.

Verifies conditional Policy execution, cross-round observation, reservation and attrition, delayed delivery, and payment to the payee ([INV-05](02_type_system.md#inv-05), [INV-12](02_type_system.md#inv-12)).

**D-minor (golden).** The US orders 8 Mil of `major-military-action`, below its minimum of 12: the order is rejected, and US Military stays at 95.

## 25. Fixture E: Scarcity and Trade Restriction

[Fixture E](#25-fixture-e-scarcity-and-trade-restriction) is qualitative: it has no fixed expected values and no compiled fixture file. It passes on reconciliation, solvency, and the propagation below.

With full circulation enabled, the Round 3 disruption ([§13.6](#136-energy-disruption)) reduces supplier offers. Observed propagation, with every seat on one preset ([test 8](06_simulator.md#14-mandatory-tests)): operating shortfalls (`Aggressive`). Capacity investment (`Investor`) and procurement (`Aggressive`). Household demand stepping from 8 E to 6 E above 3.0 M/E, and SME demand falling with its Representation factor (`Passive`). And the EU's solidarity Proposal, with Germany's pivotal signature, executed in the same round (`Investor`). Two channels stay dormant in those runs: SHIP places no Market order, so it does no arbitrage, and China's Asia offer does not fire, because its rule needs an Asian price above 3.2 and the Asian price peaks at 2.88.

**E2 (restriction).** In Round 2 the US enacts a shipment ban on China, effective Round 3. China reciprocates in Round 3, effective Round 4. With the ban in force in Round 3, the EU publishes and passes its tariff Proposal in Round 3, in force from Round 4 ([Fixture B4](#22-fixture-b-germany-and-the-eu-collective-procedure)). Compare regional price gaps, shipping volumes, and tariff revenue against the unrestricted run. Every consequence must be explainable from recorded transitions ([INV-05](02_type_system.md#inv-05), [INV-06](02_type_system.md#inv-06)).

## 26. Fixture F: Simultaneous Conflicting Orders

The US, UK, and Japan order 15, 8, and 9 Mil: 32 against a capacity of 30. Escrow at 15.5 M/Mil: US 232.5, UK 124, JP 139.5, all within their Money.

**Allocation:** 30/32 = 0.9375 gives 14.0625, 7.5, 8.4375. Floors 14 + 7 + 8 = 29. The remaining unit goes to the largest remainder (UK, 0.5). Fills: **US 14, UK 8, JP 8**.

**Cost:** C(30) = 240 + 225 = **465 M**, uniform price 15.5 M/Mil.

| Buyer | Mil | Money paid | Energy used |
|---|---:|---:|---:|
| US | 14 | 217 | 28 |
| UK | 8 | 124 | 16 |
| JP | 8 | 124 | 16 |
| **Total** | **30** | **465** | **60** |

DEF receives 465 M and passes 116.25 M to US_WORKERS. Unfilled escrow (US 1 Mil, JP 1 Mil) is released. Reversing submission order gives identical fills. Repeat with Energy demand exceeding a node's operational bound, and with one Actor's actions exceeding its Money.

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
- **G3 (cross-region):** the same joint Proposal with Germany. SA proposes [Transfer 20 E SA → DE, Transfer 48 M DE → SA, Transform `energy-shipping` 20 by SA], signers [SA, DE, SHIP]. The shipping fee is 0.3 × 20 = 6 M, paid by SA to SHIP: SA 155 → 197 M, 110 → 90 E. DE 125 → 77 M, 38 → 58 E. SHIP 110 → 116 M. Under an `AccessBan` covering the route the Proposal is fully signed but the shipping order is infeasible, so it is rejected as a unit while the rest of the round resolves. If SHIP signs shipments totalling more than 60 E in a round, all of that round's shipping Proposals are rejected ([§3.2](#32-transport-capacity-and-trade-restrictions)).
- **G4 (out-of-band strike joint Proposal):** LABOR, ENCO, and DEF agree off-system that LABOR will not strike. In Round 2 LABOR declares no strike and ENCO and DEF each Transfer 10 M to LABOR. The engine sees ordinary Transfers. Nothing binds LABOR in later rounds.
- **G5 (three-party):** SA proposes [Transfer 20 E SA → JP, Transfer 48 M JP → SA, Transfer 10 M JP → SHIP (service fee)], signers [SA, JP, SHIP]. JP and SHIP sign: SA 155 → 203 M, 110 → 90 E. JP 150 → 92 M, 40 → 60 E. SHIP 110 → 120 M. If SHIP does not sign, the whole Proposal lapses and no balance changes.

Verifies signature completeness, lapsing, atomic settlement of bundled Intents, and that the engine enforces no promises ([INV-10](02_type_system.md#inv-10)).

---

# Part V: Balancing and Iteration

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

**Baseline simulation results.** The variant experiments ([Simulator §11](06_simulator.md#11-experiments), `packages/experiments/test/Test/Experiments/Experiments.purs`) leave these parameters unchanged. They pass every invariant in every variant, and bear on [§29](#29-gameplay-questions) as follows:

- *Seat presets.* `Conservative` only buys below 2.5 M/E while holding under 30 E. Prices pass 2.5 by Round 2, so twelve of the fifteen seats never act under it and are unmeasurable against `Passive`. Against an active baseline (`RandomValid`), every seat has a measurable effect, so agency is analysed against that baseline.
- *Small players (Q1).* LABOR, SME, and AID move prices and other Actors' holdings measurably. The SME Representation is weak at these values: the shrinkage sweep 0–0.5 moves the final Atlantic price by at most 0.0001 M/E, and game seeds 1–3 give results identical to 4 dp. Reference, shrinkage, and bound are the levers to raise its influence.
- *Markets (Q3).* The Round 3 disruption raises the Atlantic price monotonically with the cut (2.75 → 2.86 → 3.08 M/E for 0, 12, 24 E).
- *Escalation and coordination (Q2, Q4).* No reference preset enacts a major military action or a tariff, so the major-action threshold and the EU matching tariff have no effect under the presets. They are exercised by Fixtures D and E and by Test 11.
- *Circulation (Q5).* The largest holder's share of Money rises from 0.098 to at most 0.145 over five rounds in every variant.

## 29. Gameplay Questions

1. **Are small players consequential?** Do SME, LABOR, and AID decisions affect larger Actors, and does the influence ratio stay in range?
2. **Are large Actors too powerful?** Do Energy, semiconductor, transport, and procurement dependencies create leverage for others?
3. **Do Markets produce interesting choices?** Do regional prices and restrictions encourage trade, coordination, and investment without making Markets unusable?
4. **Are implicit Policies credible?** Do Aggregates and the NPC respond coherently without improvisation?
5. **Does the economy stay functional for five years?** Does Money keep moving rather than settling in reserves?
6. **Are results understandable?** Can players trace balance-sheet changes to actions, signed joint Proposals, and rules?
7. **Is five rounds enough?** Do early decisions and investments matter later?

## 30. Measurements

Recorded each round. Dashboard metrics are versioned projections of snapshots and events ([INV-31](02_type_system.md#inv-31)).

| Measurement | Purpose |
|---|---|
| Node prices and inter-regional gap | Scarcity, volatility, restriction effects |
| Energy traded, shipped, unfilled | Liquidity and shipping |
| Money by Entity and in Market reserves | Concentration and trapped Money |
| Production, consumption, shortfalls | Stock-flow balance |
| Procurement, deployment, attrition | Escalation |
| Soft Power created, spent, decayed | Influence mechanics |
| Semiconductor units withheld or sold, and capacity effect | Industrial dependency |
| EU support and outcome. SME baseline vs adjusted | Procedure and Representation effects |
| Representation influence ratio | Representation balance |
| Rejected and partial actions | Constraints |
| NPC actions and triggers | Policy behavior |
| Reconciliation results | Economic consistency |
| Proposals published, signed, and lapsed | Player interaction |
| Per-Actor agency (counterfactual change) | Consequential choices |
| Objective criteria met per Actor | Evaluation ([§9.3](#93-objectives)) |

## 31. Configuration Package

The instance is a versioned configuration package with no new foundational types. Its files are compiled from the DSL by `just _premises`:

```text
packages/premises/
  meridian/v0/   setting  resources  markets  transformations  circulation
                        entities  representations  game-rules  policies  instantiation
  presets.json          the seat presets, shared by every setting (docs/06 §4.2)
  fixtures/             the golden fixtures of Part IV (generated, not committed)
```

Each section file is one JSON object, merged into the Gameboard. `instantiation.json` is the Instantiation Configuration. Capabilities, objectives, and relationships live in `entities.json` and the DSL. Verticals and initial conditions have no file yet. Which edits are Gameboard changes and which are Instantiation changes is set by [Gameboard](03_gameboard.md) and [Instantiated Game](04_instantiated_game.md).

---

# Part VI: Acceptance Criteria

## 32. Minimum Viable Test

The first complete test succeeds when the engine can:

1. Initialize fifteen seats, the China NPC, and seven Aggregates with consistent ownership, capabilities, and fallbacks. *Proved by:* settings: *validates*, *instantiates with the initial holdings of §15*, *instantiates the market and capacity state of §16*.
2. Keep Germany explicit and an EU member without duplication, and reconcile against the Germany-implicit configuration. *Proved by:* settings: *reconciles Germany explicit against Germany implicit (§14.2)*. Simulator: *12 Resolution consistency*.
3. Trade on both nodes with limit prices and partial fills, and ship across regions under tariffs and bans. *Proved by:* Fixtures A, A2, F (golden). Simulator: *4 Market exchange*, *9 Conflicting actions*, *11 Trade restriction*.
4. Run procurement as a facility with Energy and semiconductor inputs, delayed delivery, and payment to payees. *Proved by:* Fixture D (golden). Handlers: *Fixture D end to end*. Experiments: *7 NPC response*.
5. Treat deployment as a reservation that competes with a Military Market sale, with declared attrition on major actions. *Proved by:* Fixtures R, R-alone, D and D-minor (golden). Simulator: *7b Reservation*, *7c Deployer*.
6. Produce, spend, and decay Soft Power. *Proved by:* simulator: *Soft Power is produced, spent, and decays*.
7. Run the circulation cycle and pass reconciliation every round. *Proved by:* Fixture *circulation* (golden). Simulator: *1 Full game*, *8 Scarcity*. INV-05 asserted every round.
8. Generate China's conditional behavior and baseline Aggregate behavior. *Proved by:* settings: *china-strategic-v0 procures after a major US action*. Experiments: *7 NPC response*.
9. Execute EU procedure Proposals in the round they are published when weighted signatures exceed the threshold, without transferring EU authority. *Proved by:* Fixtures B1–B4 (golden). Simulator: *5 Germany and EU*.
10. Store the bounded Representation factor and apply it in the following round, recording proposed and executed actions. *Proved by:* Fixtures C1–C3 (golden), the C4 arithmetic (golden test), and C5 for seeded noise only (its invariants and noise test). Simulator: *6 SME Representation*.
11. Settle fully signed Proposals atomically and let incompletely signed ones lapse. *Proved by:* Fixtures G1–G5 (golden). Simulator: *10 Signed Proposals*.
12. Resolve simultaneous actions deterministically and order-independently. *Proved by:* golden: *order independence*. Simulator: *3 Submission order*.
13. Execute five years and produce a replayable history and structured events for reporting. *Proved by:* simulator: *2 Replay*. Adapters: *13 Persistence*. Handlers: *a scripted live session equals replaying its recorded Proposals and signatures*.
14. Evaluate every Actor's objective criteria from the records. *Proved by:* experiments: *14 Agency*.

The test does not need realistic macroeconomic forecasting. Its objective is coherent, consequential, explainable interaction.

## 33. Summary

*Meridian* is a provisional configuration of the abstract architecture, not an extension of it. It defines four Resources, two regional Energy Markets, three facilities, a balanced circulation schedule, fifteen seats, one NPC (China), and seven Aggregates. Germany influences EU decisions by signing procedure Proposals that execute in the same round, while German households and firms remain in the EU residual. The small-business owner influences Global SME demand through a bounded Representation applied in the following round. Scarcity comes from the scheduled disruption and player choices, not from incomplete accounting. Every parameter here should be changeable while the same Type System, Gameboard machinery, and runtime continue to be used.
