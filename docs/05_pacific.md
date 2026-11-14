# Pacific Rivalry

**PremiseComposition ID:** `pacific`, version `v0`

## 1. Purpose

Pacific is a game about strategic interdependence between the United States and China. Each country makes something the other needs, each runs its economy under different institutions, and each country's arrangement helps sustain the other's while making both more dependent on arrangements that are costly to change. The game lets players pull the levers each side really holds (trade barriers, industrial policy, credit, currency, investment and distribution) and see what follows, round by round, from a few rules on the World's own Stocks and Flows.

Every outcome is a change in holdings, flows, production or the Rules in force, in normalized game units. Nothing is scripted: political viability, debt, industrial competence and the exchange rate are all derived from the record (§6).

## 2. Two complementary economies

The US makes technology (`T`) and China makes industrial inputs (`I`); each needs some of both, and each prices its good in its own currency, the dollar (`USD`) or the renminbi (`RMB`). Trade leaves both better off financially, but gives each a hold over the other.

Pacific is three economic objects: two national reproduction regimes, one per country, joined by one international clearing system ([01 design principles](01_overview.md)).

```text
NationalReproductionRegime                 InternationalClearing
  actors: a state, a firm, households        goods markets: the world Exchanges, each good in its producer's currency
  productive capability: capacity,           currency market: the usd-rmb dealer, whose price is the exchange rate
    competence, inputs, production           capital: world savings, China's reserves, foreign bonds
  effective demand: wages, transfers,        settlement Rules: export controls, import bans, tariffs, the export tax,
    dividends, household spending              the asset fee
  financial capacity: deposits, loans,       external balances: each country's accounts (INV-38)
    bonds, interest, arrears
```

One builder makes both regimes; they differ only in configuration, which is where the two countries' institutions live:

| | United States: market-led | China: state-led |
|---|---|---|
| Productive capability | The firm holds most technology capacity (8 of 10); a quarter of a unit of industrial competence. | The state holds most industrial capacity (8 of 10); strong industrial competence, none in technology. |
| Effective demand | Households spend 80% of what they hold, supported by foreign financing. | Households spend 20%: weak relative to production. |
| Financial capacity | Foreign demand for dollar assets: the US bond, held by households, China and the world. | Directed savings and credit: deposits and loans at the state's rates. |
| State income | The export tax, any tariff, its households' lending, foreign lending and the asset fee. | Its export sales, any tariff, its households' deposits, and interest on its loans. |
| Central tension | A strong dollar supports purchasing power and borrowing, but makes domestic industry less competitive and harder to rebuild. | Directing resources to industry strengthens manufacturing but suppresses household demand, so industry depends on exports and debt-financed investment. |
| Viability | Its households' purchases every round. | Its industrial output every round, and its firm current on its loans. |

The complementarity runs through every circuit (§5): China's surplus and the world's savings buy dollar assets and hold the dollar up; the strong dollar keeps America importing; America's imports keep China's industry selling and its credit rolling. These are starting configurations, not permanent traits of either country.

## 3. Seats

Four seats, a state and a firm in each country, each control a different part of their country's reproduction and use the clearing system to pursue their interests. The central dilemma: each state tries to change its country's dependencies abroad, but succeeding depends partly on its own firm, whose incentives may run the other way.

Each seat holds four tools, each off, low or high every round, so every seat chooses among 3⁴ = 81 settings a round. The countries differ in where they start and in their institutions, not in how many choices their seats have.

| Seat | Its dilemma | Tool | Off / low / high |
|---|---|---|---|
| US state (`US`) | **Purchasing power or industrial renewal**, and whether to trade financial advantage for competitiveness | Trade barrier | none / import ban on Chinese inputs (demand pressure) / export control on US technology (supply pressure) |
| | | Industrial policy: build and staff industrial capacity | none / 1 / 2 units a round |
| | | Transfer to households | none / 5% / 15% of its USD a round |
| | | Asset fee on foreign lending | none / 0.02 / 0.05 USD per USD lent |
| China state (`CN`) | **Producer support or household prosperity**, and whether to buy competitiveness with reserves | Trade barrier | none / import ban on US technology / export control on Chinese inputs |
| | | Industrial policy: build and staff technology capacity | none / 1 / 2 units a round |
| | | Financial repression | off: households paid 5%, firm lent 5% of the state's RMB at 5% / low: 2%, 10% at 1% / high: nothing, 20% free |
| | | Dollar reserves | none / 3 / 6 USD bought a round, lent to the US state |
| US firm (`US-firm`) | **Accumulation or distribution**: reinvest, or pay households, whose spending is its home market | World-market share | none / half / all of its technology offered abroad |
| | | Input inventory | none / 1 / 2 units of inputs a round |
| | | Capital investment | none / 1 / 2 units of technology capacity a round |
| | | Dividends | none / 5% / 15% of its USD a round |
| China firm (`CN-firm`) | **Expansion or profitability**: borrow and invest, or distribute and cut its debt | World-market share | none / half / all of its inputs offered abroad |
| | | Input inventory | none / 1 / 2 units of technology a round |
| | | Capital investment | none / 1 / 2 units of industrial capacity a round |
| | | Dividends | none / 5% / 15% of its RMB a round |

The barrier is a ladder of pressure. An import ban is demand pressure: it cuts the other country's sales, and with them, slowly, its income and debt service. An export control is supply pressure: it cuts the other country's input at once, until it substitutes. The tariff is not a tool level but a Rule kind with its rate a dial (§4.4), which a human or agent seat may enact; the catalog holds each state's tariff as a Strategy of its own (`us-tariff-v0`, `cn-tariff-v0`) so a test or cast can give it to a state, and a live run's joint pass has its form to copy. No investment is ordered beyond the goods and wages its investor holds.

Each seat may be played by a human, an AI agent or a preset (§8.2), and is briefed with its description and objective (§6). What no seat decides runs by itself: each state's and firm's routine trade, currency and production, the households' and the rest of the world's spending and lending, wages, upkeep and the export tax (§7).

## 4. The model

### 4.1 Capability

| Resource | Moves | Role |
|---|---|---|
| `USD`, `RMB` | Freely | Each country's currency. Totals are fixed (317 and 334): trades, Transfers, levies, wages, upkeep and bonds move money; lending creates a claim, never money. |
| `T`, `I` | Freely | The goods, consumed by requirements, by households and as the other firm's input. |
| `K_T`, `K_I` | Not at all | Capacity: each unit of production holds one for the round. |
| `S_T`, `S_I` | Not at all | Competence: what a workforce can run, held for the round like capacity. |

- **Production** is a Transformation delivered the next round, at most `min(capacity, competence)` units. Each unit pays its producer's households a wage and teaches 0.15 of a unit of competence; every holder's competence wears a tenth a round, so a producer running fully settles at 1.5 times its capacity and one that stops loses its skill.
- **The firms** run half their capacity with no input and the other half on the foreign good: 1/4 `I` per unit of US technology, 1 `T` per unit of Chinese inputs.
- **The states** run their own good's capacity, and their substitute (the other's good) only when it is cheaper than importing at the round's prices and tariff, or when an export control or their own ban stops imports.
- **Investment** turns goods and construction wages into capacity and competence: industrial policy 2 goods and 2 wages for 2 capacity, a firm 2 goods and 4 wages for 1; China's technology capacity arrives two rounds later, the rest the next round.
- **Upkeep** costs 0.05 per unit of capacity a round, idle or not, paid to the owner's households. **Requirements:** each round the US consumes 6 `T` and 5 `I`, China 3 `T` and 4 `I`; what cannot be met is a shortfall.

### 4.2 Demand

Wages, transfers and dividends are household income. Households buy their own good at home with half their spend share of what they hold, convert the other half on the currency market (under their own tariff, only what leaves room for it), and buy the foreign good abroad. America spends 0.8 of what it holds and China 0.2: China's low consumption is a low propensity to spend, not low income. Each lends its state half of what it does not spend, and America's lend their firm the other half (§4.3), so a household never commits more than it holds. What a firm offers at home, its households buy as far as their budget reaches at the highest price the home Exchange could clear at (§4.4).

### 4.3 Finance

Every loan is one bond template, set by currency and rate. The lender Transfers the principal, the borrower holds it for a round and repays it with interest the next; what it cannot pay carries with interest, and a borrower holding less than one unit lapses the bond ([03 §15](03_premise.md#15-authoring-laws-and-agreements)). The principal moves as a Transfer, so the Rules in force screen it.

| Template | Rate | Lender → borrower |
|---|---:|---|
| `us-bond` | 5% | US households (10% of their USD a round), China's state and the rest of the world → US state |
| `us-firm-bond` | 5% | US households (another 10%) → US firm |
| `cn-deposit-off`, `-low`, `-high` | 5%, 2%, 0% | China's households (40% of their RMB) → China's state, at its repression setting |
| `cn-loan-off`, `-low`, `-high` | 5%, 1%, 0% | China's state → China's firm |

**Financial repression** moves income only through those rates and volumes: households give up their deposits times 5% less their rate, and the firm saves its loans times 5% less its rate. With the firms investing on credit, it grows China's industry (F3). **Dollar reserves** buy USD on the currency market and lend China's surplus dollars to the US state.

### 4.4 Exchanges and Rules

| Exchange | Base / quote | Open to | Price |
|---|---|---|---|
| `technology-world`, `industry-world` | `T`/`USD`, `I`/`RMB` | every Actor | 1 and 2 to start, rising 0.05 and 0.03 per unit drawn from their stock of 4 and 6 |
| `technology-us`, `industry-cn` | the same | the country's state, firm and households | 1.25 and 2.5 to start, rising 0.0625 and 0.0375 per unit drawn from their stock of 4 and 6 |
| `usd-rmb` | `USD`/`RMB` | every Actor and the rest of the world | the exchange rate, moving with the dealer's position |

Every goods Exchange, world and home, holds a stock of its good (dials), counted as the producing country's for Rules ([02 §5.2](02_kernel.md#52-exchanges)). Orders pair with each other first; what buyers want beyond the sellers comes from the stock, and each unit drawn raises the price along the Exchange's curve. Its domain starts at its opening position, so it buys back only what it has sold, its price falling back as it restocks, and never takes output nobody ordered: a seller is otherwise paid only for what a buyer takes. Orders on the goods Exchanges take the batch price. An order with no limit escrows the highest price its Exchange's curve reaches ([03 §10.1](03_premise.md#101-candidate-transaction-rules)), and one short Resource scales everything its Actor does ([02 §8.2](02_kernel.md#82-funding-and-settlement)),, and every participant budgets its orders the same way (§7). A home Exchange starts at a premium of a quarter over the world price and rises a quarter faster. Each firm and household sector counts as its country for Rules.

| Rule kind | Effect |
|---|---|
| `us-import-ban`, `cn-import-ban` | Blocks the other's good coming in. |
| `us-export-control`, `cn-export-control` | Blocks its own good leaving for the other. |
| `us-import-tariff`, `cn-import-tariff` | 1 (a dial) per unit of the other's good coming in, paid by the importer to its state. |
| `us-export-tax` | 0.5 `USD` per unit of `T` sold to China, paid by the seller; in force from the start. |
| `us-asset-fee` | 0.02 or 0.05 `USD` per `USD` that China or the rest of the world lends the US, paid by the lender. |

Each regime gives its state the authority to enact its own kinds. A Rule takes effect at the next round boundary and stays until repealed. A block refuses each pair, gift Transfer or joint candidate Transaction it covers, so it holds however a trade is arranged ([03 §5.5](03_premise.md#55-geography-access-and-rules)).

### 4.5 Currency and capital

`usd-rmb` is a dealer holding 150 of each currency. It trades each round's net from its reserves at a price that starts at 1 `RMB` per `USD` and moves half a hundredth per USD of net buying ([02 §5.1](02_kernel.md#51-cost-function)), so the exchange rate is the running balance of what has been bought and sold. Importers buy the other currency to cover their next imports, China's reserves buy USD, and world savings buy USD with a fifth of their RMB and lend half their USD to the US state while the asset fee is off; with the fee on they lend a fifth and sell 30% of their USD. America's deficit weakens the dollar; China's reserves and the world's demand for dollar assets hold it up.

### 4.6 Accounting

Three identities hold every round, each checked separately:

- **Currency conservation.** Each currency's total never changes (INV-05, [02 §9](02_kernel.md#9-architectural-invariants-and-numerics)).
- **Asset and liability matching.** Every open bond is its lender's claim and its borrower's liability, in one currency.
- **External consistency (INV-38).** For each country and currency, the round's change in its position (holdings, plus claims on the rest of the world, less what it owes) equals the flows across its border: goods, currency trades, levies and interest (`Laws.External`). Summed in the numeraire these are the current account, the financial account and a valuation line for the exchange rate's move.

### 4.7 A round

Each round follows the canonical sequence ([02 §8.1](02_kernel.md#81-canonical-resolution-sequence)): Rule changes take effect, capacity arrives, production is delivered, competence wears and bonds fall due; each seat sets its tools; routines draft production, trade, currency and loans; orders are funded, Exchanges clear, Rules block or levy, loans and investments move, wages and upkeep are paid and requirements consumed; the round-end World is stored, and money received is spendable from the next round.

## 5. Circulation and feedback

### 5.1 Circuits

Four circuits close, each with a source and a sink:

1. **Domestic, within a regime.** Production pays wages; households spend at home, which is the firm's home revenue; saving, imports and taxes leak out, transfers, interest and the state's purchases flow in.
2. **Savings, within a regime.** Households lend to their state (and, in America, their firm); China's state lends to its firm; the firm invests; output, wages, interest and principal flow back.
3. **External, through clearing.** Importers buy the other currency and pay the other country's sellers; China's reserves buy USD and lend it to the US state, which spends it.
4. **World savings, through clearing.** The rest of the world buys USD, lends it to the US state, and is repaid with interest: the only saving from outside both regimes.

### 5.2 Feedback loops

Thirteen loops arise from the rules; none is scripted, and each holds only under its conditions.

| # | Loop | Sign | Mechanism | Holds when |
|---|---|---|---|---|
| F1 | Learning by doing | Reinforcing | Each unit produced teaches competence | Capacity runs |
| F2 | Industrial decay | Reinforcing | Competence wears a tenth a round | Capacity stands idle |
| F3 | Repression growth model | Reinforcing | Repression lends the firm more, cheaply, and it invests on credit | At the default dials; the more, the less money the firm has of its own |
| F4 | Debt burden | Reinforcing | Loans fall due with interest and are rolled over | The loan rate is above zero |
| F5 | Currency adjustment | Balancing | America's imports sell USD on the dealer | Reserves and world savings do not buy it back |
| F6 | Recycling blocks adjustment | Counters F5 | China's reserves and world savings buy USD | They are on, and the asset fee is not |
| F7 | Strong-dollar trap | Reinforcing | A substitute runs only when cheaper than importing at the rate, and an idle one decays (F2) | Its cost is within the dollar's swing of the import price, as at the default dials |
| F8 | Home market | Reinforcing | Wages and dividends become household purchases at home | The firm offers at home |
| F9 | Tariff erosion | Balancing | Under its tariff a private sector imports less, shrinking the tariff's base | A tariff is in force |
| F10 | Supply leverage | Reinforcing | Under the other's export control a substitute runs only as far as its competence | Competence was lost before the control |
| F11 | Political constraint | Balancing, through behaviour | Viability is an objective, not a limit; a Strategy that weighs it delays adjustment | A Strategy responds to it, such as `Cautious` |
| F12 | Overcapacity cost | Balancing | Output nobody buys still pays its wages and upkeep | A producer cannot sell what it makes |
| F13 | Shortage pricing | Balancing | When buyers want more than sellers offer, a goods Exchange sells from its stock at a price rising along its curve, which rations demand and makes the importer's substitute pay; as it restocks the price falls back | Demand exceeds what sellers offer, and the stock lasts |

### 5.3 Two traps

**The strong-dollar industrial trap.** Foreign demand for dollar assets holds the dollar up; a strong dollar makes Chinese inputs cheap in USD, so America's substitute stands idle and its competence wears away (F2); capacity built later runs only as fast as lost competence is relearned (F10), so America depends more on Chinese supply and is more exposed to an export control; and China's dollar earnings, lent back through reserves, finance the next round of imports. A shortage that draws down the inputs' stock raises their price (F13) and weakens the trap, but at the default dials it holds: US industry costs a little more than imports at par, so a strong dollar leaves America's substitute idle and a weak one runs it (§9).

**The industrial-surplus trap.** Repression favours producers: the firm borrows cheaply and invests on credit (F3), production outgrows households that spend a fifth of what they hold, so the surplus depends on foreign demand; export income services the firm's loans and the state refinances them, so debt grows by refinancing rather than default (F4); and a state that weighs its viability is slow to rebalance (F11). Rebalancing costs China no output (§9), so what holds this trap in place is behaviour and debt, not a loss of production.

## 6. Objectives and indicators

Each seat has four criteria, evaluated from the recorded run ([03 §10.5](03_premise.md#105-evaluation)); its score is the number it meets. Each bar sits near the 40th percentile of its measure over the balance sample (§8.3), so a seat meets each about three times in five; the bars are dials.

| Seat | Criteria |
|---|---|
| Each state | **Security:** its shortfall of the good it imports at most 15.52 (US) or 11.56 (China) over the run. **Industry:** its firm ends with at least 9.87 `K_T` (US) or 3.79 `K_I` (China). **Viability, every round:** US households buy at least 2.52 units; China's state and firm produce at least 8 `I` with its firm owing nothing in arrears. **Prosperity:** its households buy at least 20.57 (US) or 3.64 (China) units over the run. |
| Each firm | **Wealth:** it ends with at least 11.61 `USD` (US) or 11.56 `RMB` (China). **Growth:** its state's industry bar. **Output:** its input-dependent production at least 10.82 (US) or 2 (China) units. **Home demand:** its state's prosperity bar. |

Viability is political support derived from the economy, not a Resource: what America's households can buy; China's industrial employment, which output stands for, and its financial stability. It is an objective, not a limit, so a state may break its floor and pay for it in its score (F11).

No indicator is stored: each is read from the record by a generic view (`Views.Indicators`, `Views.Regimes`, `Views.Dependence`), over any composition, and the run page's headline shows them at the round chosen ([07 §9.3](07_simulator.md#93-run-details)).

| Indicator | Formula | Reads |
|---|---|---|
| Substitute gap | The wage of making an imported good at home, in the numeraire, less the cheapest import cost, at the round's opening prices | Competitiveness: above 0, the substitute stands idle |
| Net external position | `Σ_currency V(c) × (foreign currency held + owed by the rest of the world − owed to it)` | The external balance |
| Domestic absorption | What a country's households bought of its own good over what it made | Dependence on foreign demand |
| Debt service | What a seat owed on its agreements in the round over its revenue from goods | Financial stability |
| Viability margin | An every-round criterion's measure less its bound, in the worst round so far | Political viability |
| Leverage and resilience | The other's exposure, `max(0, requirement − capacity)`, and sales abroad; each state's `coverage = min(1, capacity / requirement)` | What a barrier could cost, and how far each could carry on |

## 7. Implementation

Pacific is three Premises at version `v0`, one per economic object ([03 §2](03_premise.md#2-premises-and-compositions)), built from the existing vocabulary of Resources, Transformations, laws, agreements, Exchanges and Rules; it adds no new kind of engine object, and the World stays authoritative.

| Premise | Adds |
|---|---|
| `us-regime` | America's state, firm and households; their production and investment Transformations; the wage and upkeep laws; the US bonds; its routines, its households' demand, its requirements and its seats' criteria. |
| `china-regime` | The same for China, with deposits and loans at each repression setting in place of the US bonds. |
| `international-clearing` | Every Resource, the world, home and currency Exchanges; every Rule kind, with the export tax in force; the groups that make each country one party; the rest of the world's savings; and the parameters the Strategies read. |

One builder makes both regimes (`Premises.Pacific.Regime`) from a configuration that is the only place the countries' numbers and institutions differ (`configOf`); another makes the clearing system (`Premises.Pacific.Clearing`). Both read a country through its record (`Premises.Pacific.Country`), and the composition is the union of the three objects' parts (`Premises.Pacific.Parts`). A regime does not load alone: its Exchanges, Rules and Resources are the clearing system's. `Premises.Pacific.Dials` holds every number.

Each regime has two routines ([04 §6](04_run.md#6-strategy-initialization)) that run beside whoever plays its seats. The state's commerce routine (`us-commerce-v0`, `cn-commerce-v0`) sells what is left after the requirement and buys what the requirement and its investments take beyond what it holds, orders production and its substitute (§4.1), buys the other currency for two rounds of imports and signs its state's bonds. The firm's supply routine (`us-supply-v0`, `cn-supply-v0`) orders production as far as capacity, input and wages allow while it holds less than two rounds' output, running its input first, keeps two rounds of input and three of the other currency, and signs its firm's loans.

Every participant budgets the same way. Funding sets the round's wages and upkeep aside first ([02 §8.2](02_kernel.md#82-funding-and-settlement)); what a seat holds beyond them is shared out in fixed shares that sum to no more than all of it: six tenths for trade, its payouts' share (transfers or dividends), China's loan to its firm and up to a tenth for its reserves, and investment with the rest. The trade budget meets each cost in turn, levies among them: the export tax on what a seat sells abroad, its purchase of its own good, the tariff on its imports, its currency and a firm's stockpile. A trade whose levy the budget cannot meet shrinks, and a firm offers at home what it cannot pay the export tax on. Households and the rest of the world spend fixed shares of what they hold, and households budget their tariff likewise. So no participant claims more than it holds, and the engine's compliance check ([03 §5.5](03_premise.md#55-geography-access-and-rules)) is a backstop for seats a person or an agent plays.

## 8. Initial configuration, balance and strategies

### 8.1 Opening holdings

| | US | US firm | US households | CN | CN firm | CN households | Rest of world | Dealer |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| `USD` | 112 | 15 | 10 | 6 | 4 | | 20 | 150 |
| `RMB` | 12 | 4 | | 88 | 10 | 10 | 60 | 150 |
| `T` | 2 | 8 | | 0 | 1 | | | |
| `I` | 0 | 1 | | 8 | 2 | | | |
| `K_T` / `K_I` | 2 / 0 | 8 / — | | 0 / 8 | — / 2 | | | |
| `S_T` / `S_I` | 3 / 0.25 | 12 / — | | 0 / 12 | — / 3 | | | |

The goods are one production cycle's output. Each firm holds one round's input; China's state and the firms open with two rounds or more of the other currency their imports cost, America's state with a little over one; the firms open with little money of their own, so they invest on credit. Each country opens at full competence in its own good, and America with a quarter of a unit of industrial competence.

### 8.2 Presets

A preset is the policy every seat plays in a simulated run, as each tool's level (`Premises.Pacific.Strategies`). A state preset sets the states' tools with each firm offering all it holds abroad; a firm preset sets the firms' tools with the states' off. The default is `FreeTrade`, for six rounds of the 1 to 12 allowed; each seat's alternative for the suite's agency runs ([04 §2](04_run.md#2-instantiation-configuration)) is `Hawk` for a state and `Builder` for a firm.

| State preset | US: barrier · industry · transfer · fee | China: barrier · industry · repression · reserves |
|---|---|---|
| `FreeTrade` | none · none · none · none | none · none · off · none |
| `Ban` | ban · none · none · none | ban · none · off · none |
| `Restrict` | control · none · none · none | control · none · off · none |
| `Substitute` | none · 1 · none · none | none · 1 · off · none |
| `StatusQuo` | none · none · none · none | none · none · high · high |
| `Rebalance` | none · none · none · high | none · none · off · none |
| `Hawk` | control · none · none · high | control · none · high · high |
| `Populist` | ban · none · 15% · none | ban · none · off · none |
| `Planner` | none · 2 · none · high | none · 2 · high · none |
| `Reformer` | none · 1 · 5% · none | none · 1 · off · none |

| Firm preset | World share · inventory · investment · dividends |
|---|---|
| `Exporter` | all · none · 1 · none |
| `Merchant` | all · none · none · 15% |
| `Builder` | none · none · 2 · none |
| `Patron` | none · none · none · 15% |
| `Hedger` | all · 2 · none · none |

Three state presets react: `Adaptive` builds its substitute while the other keeps a barrier against it; `Retaliate` controls its exports the round after the other raised a barrier; `Cautious`, the Strategy F11 is tested with, keeps China's reserves on and stays rebalanced while its firm's production on US technology held up last round, and represses when it did not. No preset repeals, since a repeal names a Rule's id; a human or agent seat can.

### 8.3 Balance

Over 400 settings of every seat's four tools, drawn deterministically from the 81 each has, the seats meet 959, 1,080, 962 and 970 of their 1,600 criteria (the US, China, the US firm and China's firm): each bar is set near the 40th percentile of its measure over this sample. A change to any dial is checked against it.

## 9. What the simulations show

The golden scenarios and experiments play whole runs and check their records against literals held in the suites; this section states what each shows. Firms offer all abroad unless named.

| Suite | Holds |
|---|---|
| `packages/handlers/test/Test/Handlers/Pacific.purs` | Trade, barriers, firms, money and balance, and sensitivity |
| `packages/handlers/test/Test/Handlers/Loops.purs` | Circuits, accounting, the loops and the experiments |
| `packages/handlers/test/Test/Handlers/Properties.purs` | Generated runs, sector balances and each dial's direction |
| `packages/premises/test/Test/Premises/Golden.purs` | Engine fixtures: literal worlds cut from Pacific, through `resolveRound` alone (`Premises.Pacific.Fixtures`) |
| `packages/premises/test/Test/Premises/Compose.purs` | The three Premises and the dials |
| `packages/views/test/Test/Views/` | The Dependence view, Compare and the indicators |
| `apps/server/test/Test/Server/Compositions.purs`, `zz-pacific.spec.js` | The composition served, launched and shown |

**Trade and barriers.** Under free trade each country exports its good, the US collects its export tax, and both currencies' totals hold every round. An export control costs the target its import at once (China short 3 `T`, the US 5 `I` a round) and halves the other firm's output on its input, while the controller keeps goods it cannot sell; an import ban leaves the target's exports piling up. No gift, joint candidate Transaction or routed order gets round a block. A tariff is paid by the importer and cuts America's imports, the more the higher its rate; a higher spend share raises what households buy. Substitution is held back by competence, and an input inventory carries a firm a round further into a control.

**Firms, money and households.** Repression lends China's firm a fifth of what the state's wages leave of its RMB, free, against a twentieth at 5% rebalanced; dividends become home demand (F8); China's households, spending a fifth of what they hold, end holding more and buying far fewer goods than America's. Under free trade America's imports sell USD and the dollar falls; China's reserves hold it up; the asset fee cuts foreign lending to the US and the dollar falls further.

**Loops.** Competence grows while capacity runs and wears while it stands idle (F1, F2). Repression grows China's industry, the more so the less money the firm has (F3), at the cost of interest rolled over (F4). China's firm repays every loan from the next and its debt grows by refinancing, never by default; a firm with no money of its own borrowing at 30% falls into arrears, which fail China's viability though its output clears the floor. A US that kept its industrial competence is far less exposed to China's export control than one that lost it (F10); output nobody buys still costs its wages (F12). A strong dollar leaves the capacity America built idle where a weak one runs it, and its competence lags (F7); a shortage draws down an Exchange's stock and raises its price (F13).

**Experiments.**

- **Political trap.** Rebalancing costs China no industrial output, and a US export control cuts it to 9 a round, above its floor of 8; its households gain the interest repression took. What keeps China repressing is a Strategy that weighs its viability, and its firm's debt.
- **Dollar trap.** The asset fee weakens the dollar, and US industry, costing a little more than imports at par, goes from about 2 units to about 9 over the run; US households, paid its wages, buy a little more. Where US industry is cheap, the dollar does not matter.
- **Lost knowledge.** With the same capacity under China's export control, a US that kept its workforce runs it all and one that lost it runs a quarter of a unit: dependence is a shortage of capital only where the workforce is still there.
- **International leverage.** An export control costs America 5 `I` a round until it substitutes, and substitution erodes it to 1.06 by round 8. An import ban nearly halves the US firm's sales abroad and cuts its money by over two fifths, but not its solvency.
- **Mutually reinforcing dependence.** Under the status quo China lends America more dollars over the run, rising to about 7 a round, while America keeps buying about 7 units of China's inputs and China's industry runs at about 10: each country's arrangement finances the other's.
- **Status quo against rebalancing.** Over eight rounds, each country building its substitute and each firm selling half at home, with each country's arrangement kept or undone and then the other's export control from round 5. China's control costs America 12 `I` under the status quo's strong dollar against 4.4 when America weakens it, and America's control costs China most when both rebalance (5.5 `T` against 2.6 when both keep their arrangements). America owes abroad about 69 `USD` when both keep them against about 2 when both rebalance.

**Sensitivity.** Across the spend shares, each country's capacity split, the tax rate, China's lending and the home premium, each structural difference keeps its effect: China's own control leaves it with inputs it cannot export, an inventory keeps a firm producing under a control, China's households lend their state more than America's, and the US state collects its export tax.
