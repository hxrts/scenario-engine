# PremiseComposition: Pacific

**Status:** Experimental configuration.  
**PremiseComposition ID:** `pacific`, version `v0`

## 1. The game

Two countries, the United States and China, each make something the other needs. The US makes technology and China makes industrial inputs, and each needs some of both. Each has its own currency, the dollar (`USD`) and the renminbi (`RMB`). Trading leaves both better off, and gives each a hold over the other.

Pacific is three economic objects: two national reproduction regimes, one per country, connected by one international clearing system ([01 design principles](01_overview.md)).

1. **The US regime** reproduces American production, household demand and financing. Its central tension: a strong dollar supports purchasing power and borrowing, while making domestic industrial production less competitive and harder to rebuild.
2. **The China regime** reproduces the same three things under different arrangements. Its central tension: directing resources to industrial production strengthens manufacturing but suppresses household demand, so its industry depends on exports and debt-financed investment.
3. **The clearing system** moves goods, currencies and financial claims between them. It is how one country's surpluses and financial decisions sustain the other's arrangement.

The central dynamic is that each country's domestic arrangement helps sustain the other's, while making both increasingly dependent on arrangements that are costly to change. China's surplus and the world's savings buy dollar assets and hold the dollar up; the strong dollar keeps America importing and leaves its substitute industry idle; America's imports keep China's industry selling and its credit rolling. Political viability, debt, industrial competence and the exchange rate all arise from a few rules on the World's own Stocks and Flows (§6), and each state is scored on its viability every round (§8), so a reform that pays later must survive the rounds it costs.

```text
NationalReproductionRegime                 InternationalClearing
  actors: a state, a firm, households        goods markets: the world Exchanges, each good in its producer's currency
  productive capability: K, S, inputs,       currency market: the usd-rmb dealer, whose price is the exchange rate
    production, learning, decay, upkeep      capital: world savings, China's reserves, foreign bonds
  effective demand: wages, transfers,        settlement Rules: export controls, import bans, tariffs, the export tax,
    dividends, household spending, the         the asset fee
    home market                              external balances: each country's accounts (INV-38)
  financial capacity: deposits, loans,
    bonds, interest, arrears

Pacific = us-regime (a NationalReproductionRegime) + china-regime (the same builder, another configuration)
        + international-clearing, the three composed into one World
```

This describes how Pacific is put together; it is not a new kind of engine object. Each object is a Premise built from the existing Resources, Transformations, laws, agreements, Exchanges and Rules (§9), and the World stays authoritative.

Every outcome is a change in holdings, flows, production or the Rules in force. All values are normalized game units.

## 2. Seats

Each seat controls a different part of its country's reproduction, and uses the clearing system to pursue its interests.

| Seat | Its national regime | The clearing system |
|---|---|---|
| US state | **Purchasing power or industrial renewal.** Invest in domestic industry and competence, or keep household prosperity and access to financing. | Restrict trade, or discourage foreign purchases of dollar assets, trading financial advantage for industrial competitiveness. |
| US firm | **Accumulation or distribution.** Reinvest in capacity, or pay households, whose spending is its home market. | Choose foreign or domestic customers, and hold inputs against dependence on imports. |
| China state | **Producer support or household prosperity.** Direct cheap credit to industry, or relax repression so households earn more. | Restrict trade, or buy dollar reserves, keeping exports competitive at the cost of adjustment. |
| China firm | **Expansion or profitability.** Borrow and invest in capacity, or distribute earnings to strengthen home demand and reduce its debt. | Choose export or domestic markets, and stockpile foreign technology against restrictions. |

The four seats press four ways at once. The US state wants industrial independence without giving up the purchasing power and financing its economy depends on. The US firm wants profitable production, and its investment and distribution decide whether American capability and household demand grow. The China state wants industrial strength without unsustainable debt or weaker households. The China firm wants profitable growth, balancing export dependence, borrowing costs and home demand. The central strategic dilemma: each state tries to change its country's dependencies abroad, but succeeding depends partly on its own firm, whose incentives may run the other way.

Each seat holds four tools, each set off, low or high every round, so every seat chooses among 3⁴ = 81 settings a round. The countries differ in where they start and in their institutions, not in how many choices their seats have.

| Seat | Actor | # | Tool | Off / low / high | What it trades off |
|---|---|---|---|---|---|
| US state | `US` | 1 | Trade barrier | none / an import ban on Chinese industrial inputs (demand pressure) / an export control on US technology to China (supply pressure) | Cutting China's sales abroad, or China's supply of technology, against its own supply of inputs, its firm's sales and retaliation |
| | | 2 | Industrial policy: build industrial capacity, and train the workforce to run it | none / 1 unit / 2 units a round | Technology and construction wages now, for less dependence on China later |
| | | 3 | Transfer to US households | none / 5% / 15% of its USD a round | Its own USD, for household purchases and home demand |
| | | 4 | Asset fee on foreign lending to the US | none / 0.02 / 0.05 USD per USD lent | Fee income and a weaker dollar, which makes US industry competitive, against foreign financing and dearer imports |
| China state | `CN` | 1 | Trade barrier | none / an import ban on US technology (demand pressure) / an export control on Chinese industrial inputs to the US (supply pressure) | Cutting America's sales abroad, or America's supply of inputs, against its own supply of technology, its exporters' sales and retaliation |
| | | 2 | Industrial policy: build technology capacity, and train the workforce to run it | none / 1 unit / 2 units a round | Industrial inputs and construction wages now, for less dependence on the US later |
| | | 3 | Financial repression | off: households paid 5%, the firm lent 5% of the state's RMB at 5% / low: 2%, 10% at 1% / high: nothing, 20% free | Cheap credit for industry, against household income and home demand |
| | | 4 | Dollar reserves | none / 3 / 6 USD bought a round, and lent to the US state | A weaker renminbi and dearer US technology, against RMB spent on dollar claims |
| US firm | `US-firm` | 1 | World-market share: technology offered on the world Exchange, the rest at home | none / half / all | A larger market, against exposure to China's barrier and the export tax |
| | | 2 | Input inventory: stockpile industrial inputs | none / 1 / 2 units a round, within half its RMB | Production that continues into a Chinese export control, against RMB tied up |
| | | 3 | Capital investment in technology capacity | none / 1 unit / 2 units a round | Capacity, competence and future sales, against goods and construction wages now, which its households' loans help pay |
| | | 4 | Dividends to US households | none / 5% / 15% of its USD a round | Its own USD, for a larger home market |
| China firm | `CN-firm` | 1 | World-market share: industrial inputs offered on the world Exchange, the rest at home | none / half / all | A larger market, against exposure to the US barrier |
| | | 2 | Input inventory: stockpile technology | none / 1 / 2 units a round, within half its USD | Production that continues into a US export control, against USD tied up |
| | | 3 | Capital investment in industrial capacity | none / 1 unit / 2 units a round | Capacity and future sales, against goods and construction wages, which its state's loans pay |
| | | 4 | Dividends to Chinese households | none / 5% / 15% of its RMB a round | Its own RMB, for a home market that grows slowly, since households save |

The barrier is a ladder of pressure. An import ban is demand pressure: it cuts the other country's sales, and with them, slowly, its income, employment and debt service. An export control is supply pressure: it cuts the other country's input at once, until it substitutes. The tariff is not a tool level: it is a Rule kind with its rate a dial (§4.1), which a human or agent seat may enact and no preset does. The catalog holds each state's tariff as a Strategy of its own (`us-tariff-v0`, `cn-tariff-v0`: the state enacts its tariff in round 1), so a test or a cast can give it to a state and a live run's joint pass has its form to copy. A seat's investment is never ordered beyond the goods and the money for its construction wages it holds. The US firm keeps dividends as its fourth tool: with them every seat's choices balance (§10.4), and they are its share of the same choice between investment and household income that repression is for China.

What no seat decides runs by itself (§7.1): a state's routine trade, currency and production, a firm's production and the purchase of its input and currency, the households' and the rest of the world's spending and lending, wages, upkeep and the export tax. Each seat may be played by a human, an AI agent or a preset (§7.3), and is briefed with its description, which names its four tools, and its objective (§8).

## 3. The national reproduction regime

Each regime's three dimensions are made of the ordinary vocabulary:

| Dimension | Resources | Transformations | Laws and agreements | Exchanges |
|---|---|---|---|---|
| Productive capability | `K_T`, `K_I`, `S_T`, `S_I`, and the goods as inputs | each production and investment (§3.5) | upkeep (a stock law), requirements; competence wears by the decay the clearing Premise declares for every competence | none of its own |
| Effective demand | the country's currency held by its households | none | wages (an action law on each Transformation's units), transfers and dividends (Transfers) | the home Exchange (`technology-us`, `industry-cn`), on which its households buy |
| Financial capacity | the country's currency | none | the bond templates (§3.4), with their interest and arrears | none of its own |


### 3.1 Resources and holdings

| Resource | ID | Moves | Use |
|---|---|---|---|
| Dollar | `USD` | Freely | The numeraire, and America's currency. Its total is fixed at 317: only trades, Transfers, levies, wages, upkeep and bonds move it, and lending moves money and creates a claim, never money. |
| Renminbi | `RMB` | Freely | China's currency, its total fixed at 334 likewise. |
| Technology | `T` | Freely | A good, priced in USD; consumed by its holders' requirements, by households when bought, and by China's firm's production. |
| Industrial inputs | `I` | Freely | A good, priced in RMB; consumed likewise, and by the US firm's production. |
| Technology capacity | `K_T` | Not at all | Produces `T`: held for the round by each unit of production. |
| Industrial capacity | `K_I` | Not at all | Produces `I`, likewise. |
| Technology competence | `S_T` | Not at all | What a workforce knows how to run of `K_T`: held for the round by each unit of production, learned and lost (§3.2). |
| Industrial competence | `S_I` | Not at all | The same for `K_I`. |

| Opening holding | US | US firm | US households | CN | CN firm | CN households | Rest of world | Currency dealer |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| `USD` | 112 | 15 | 10 | 6 | 4 | | 20 | 150 |
| `RMB` | 12 | 4 | | 88 | 10 | 10 | 60 | 150 |
| `T` | 2 | 8 | | 0 | 1 | | | |
| `I` | 0 | 1 | | 8 | 2 | | | |
| `K_T` | 2 | 8 | | 0 | | | | |
| `K_I` | 0 | | | 8 | 2 | | | |
| `S_T` | 3 | 12 | | 0 | | | | |
| `S_I` | 0.25 | | | 12 | 3 | | | |

The goods are one production cycle's output, available in round 1. Each firm holds one round's input, and each importer two rounds' worth of the other currency. The firms open with little money of their own, so they invest on credit (§3.4). A producer running its capacity fully holds competence at 1.5 times it (§3.2), so each country opens there in its own good; America has a quarter of a unit of industrial competence. The rest of the world's RMB stands for all its non-dollar savings.

### 3.2 Productive capability

- **Production.** All production is a Transformation (§3.5), delivered the next round. Each unit holds one unit of capacity and one of the competence to run it for the round: `units ≤ min(K, S)`.
- **Competence.** Each unit produced teaches 0.15 of a unit of its competence, and every holder's competence wears a tenth away each round (dials). A producer running fully settles at 1.5 times its capacity; one that stops loses competence. Each investment trains a unit of competence per unit.
- **The states' production.** Each state runs its own good's capacity as far as its competence and the wages it can pay allow. It runs its substitute, the other country's good, only when that is cheaper than importing at the round's prices: the US makes industrial inputs when its wage, 1.95 `USD` a unit, is at most their RMB price over the exchange rate (and its tariff, if in force); China makes technology when its wage, 0.97 `RMB`, is at most the technology's USD price times the rate. When it cannot import at all, under the other's export control or its own import ban, it produces regardless.
- **The firms' production** runs in two parts. Half the runnable capacity (a dial) needs no input; the other half needs the foreign good, 1/4 `I` per unit of the US firm's output and 1 `T` per unit of the Chinese firm's.
- **Upkeep.** Every unit of capacity a state or firm holds costs 0.05 a round in its currency (a dial), paid to its own households, idle or not, so capacity nobody uses costs money.
- **Requirements.** Each round the US consumes 6 `T` and 5 `I`, and China 3 `T` and 4 `I`, after the round's trades; what cannot be met is a shortfall. Households consume what they buy.

### 3.3 Effective demand

- **Wages.** Each unit produced pays its producer's households a wage in its currency: 0.25 for a country's own good, and the substitute wages above (dials). Each unit of investment pays construction wages: 2 for industrial policy, 4 for a firm's investment.
- **Transfers and dividends.** The US state's transfer to its households, and a firm's dividends to its own, are Transfers of a share of its own currency.
- **Household demand.** Households buy their own good at home with half their spend share of what they hold, turn the other half into the foreign currency on the currency Exchange (under their own country's tariff, only what leaves room for it), and buy the foreign good abroad with all the foreign currency they hold. America's spend share is 0.8 and China's 0.2 (dials). Low household consumption in China is a low propensity to spend, not low income: wages are paid per unit in both countries.
- **The home market.** What a firm offers at home, its households buy; what it sends abroad meets the other country's demand.

### 3.4 Financial capacity

Every loan is one bond template, set by its currency and rate: the lender Transfers the principal to the borrower, and they enter the bond. The bond runs two rounds: the borrower holds the principal for a round, then repays it with the bond's interest. Money received in a round is spendable only from the next, so a loan due the round after it was made would be repaid as soon as it could be spent; with a term of two, a borrower has the use of one loan's worth at a time. What the borrower cannot pay carries another round with interest; if it holds less than one unit of the currency, the bond lapses and the lender loses what is owed ([03 §15](03_premise.md#15-authoring-laws-and-agreements)). The principal moves as a Transfer, so the Rules in force screen it.

| Template | Currency | Rate | Lender → borrower |
|---|---|---:|---|
| `us-bond` | `USD` | 5% | US households, China's state and the rest of the world → the US state |
| `us-firm-bond` | `USD` | 5% | US households → the US firm |
| `cn-deposit-off`, `-low`, `-high` | `RMB` | 5%, 2%, 0% | China's households → China's state |
| `cn-loan-off`, `-low`, `-high` | `RMB` | 5%, 1%, 0% | China's state → China's firm |

Each round America's households lend their state 10% of their USD and their firm another 10%, and China's households lend their state 40% of their RMB (dials). The US state and firm sign what they are offered. China's households offer a deposit at each repression setting and China's state signs only the one at its setting; its firm signs every loan its state makes.

**Financial repression** (China's tool 3) lends the firm a share of the state's RMB, 5%, 10% or 20%, at the setting's loan rate, and signs households' deposits at the setting's deposit rate. It moves income only through those rates and volumes: households give up their deposits times 5% less their rate, and the firm saves its loans times 5% less its rate. Under `StatusQuo` (§7.3) over six rounds China's households give up 2.2598 `RMB` and its firm saves 8.5761. With the firms financing investment on credit, repression grows China's industry at the default dials (F3); it costs no output when undone (§11).

**Dollar reserves** (China's tool 4) buy 3 or 6 USD a round on the currency Exchange and lend the US state whatever China holds beyond two rounds of its technology imports.

### 3.5 Transformations

| Transformation | Executor | Uses | Creates | Arrives |
|---|---|---|---|---|
| `us-technology-production`, `us-industry-production` | US | 1 `K_T` and 1 `S_T`, or 1 `K_I` and 1 `S_I`, held for the round | +1 `T` or +1 `I`, and 0.15 of its competence | the next round |
| `cn-technology-production`, `cn-industry-production` | CN | the same | the same | the next round |
| `us-firm-base-production`, `us-firm-production` | US firm | 1 `K_T` and 1 `S_T` held; the second also 1/4 `I` | +1 `T`, +0.15 `S_T` | the next round |
| `cn-firm-base-production`, `cn-firm-production` | CN firm | 1 `K_I` and 1 `S_I` held; the second also 1 `T` | +1 `I`, +0.15 `S_I` | the next round |
| `us-industrial-investment` | US | 2 `T`; 2 `USD` of construction wages | +2 `K_I`, +1 `S_I` | the next round |
| `cn-technology-investment` | CN | 4 `I`; 2 `RMB` of construction wages | +2 `K_T`, +1 `S_T` | two rounds later |
| `us-firm-investment` | US firm | 2 `T`; 4 `USD` of construction wages | +1 `K_T`, +1 `S_T` | the next round |
| `cn-firm-investment` | CN firm | 2 `I`; 4 `RMB` of construction wages | +1 `K_I`, +1 `S_I` | the next round |

Each investment is at most two units per Actor per round. Capacity and competence produce from the round after they arrive.

### 3.6 The two configurations

The regimes are one builder's, configured twice (§9). Their configurations differ in ownership, holdings, wages, institutions, instruments and objectives:

| | China: state-led | United States: market-led |
|---|---|---|
| Productive capability | The state holds most industrial capacity (8 of 10); strong industrial competence, none in technology. | The firm holds most technology capacity (8 of 10); a quarter of a unit of industrial competence. |
| Effective demand | Households spend 20% of what they hold: weak relative to production. | Households spend 80%: strong, and supported by foreign financing. |
| Financial capacity | Directed savings and credit: deposits and loans at the state's rates. | Foreign demand for dollar assets: the US bond, held by households, China and the world. |
| State income | Its export sales, any tariff, its households' deposits, and interest on its loans. | The export tax, any tariff, its households' lending, foreign lending and the asset fee. |
| Central vulnerability | Dependence on foreign demand. | Dependence on foreign industrial supply. |
| Adjustment difficulty | Rebalancing income while keeping production and employment. | Rebuilding industry while keeping purchasing power. |
| Viability measure | Its industrial output every round, and its firm current on its loans. | Its households' purchases every round. |

These are stylized starting configurations, not permanent traits of either country.

## 4. The international clearing system

The clearing system's three categories, likewise:

| Category | Resources | Rules | Exchanges | Agreements and Actors |
|---|---|---|---|---|
| Trade | `T`, `I` | import bans, export controls, tariffs, the export tax (§4.1) | `technology-world`, `industry-world` | the groups that make each country one party |
| Currency | `USD`, `RMB` | none | the `usd-rmb` dealer (§4.2) | China's reserves, world savings |
| Capital | `USD` | the asset fee | none | the US bond lent from abroad (§4.3); the rest of the world |


### 4.1 Trade and the Rules at the border

| Exchange | Base | Quote | Open to | Price |
|---|---|---|---|---:|
| `technology-world` | `T` | `USD` | every Actor | 1 |
| `industry-world` | `I` | `RMB` | every Actor | 2 |
| `technology-us` | `T` | `USD` | the US, its firm and its households | 1.25 |
| `industry-cn` | `I` | `RMB` | China, its firm and its households | 2.5 |
| `usd-rmb` | `USD` | `RMB` | every Actor and the rest of the world | 1, moving |

Each good is priced in its producer's currency, so an import needs the other currency first and the exchange rate is what an import costs. A home Exchange pays its sellers a premium of a quarter (a dial). The goods Exchanges hold no reserves, so every trade is a pair between a buyer and a seller ([02 §5.2](02_kernel.md#52-exchanges)). Each firm and each household sector counts as its country for Rules.

| Rule kind | Governor | Effect |
|---|---|---|
| `us-import-ban` | US | Blocks `I` coming into the US from China. |
| `cn-import-ban` | China | Blocks `T` coming into China from the US. |
| `us-export-control` | US | Blocks `T` leaving the US for China. |
| `cn-export-control` | China | Blocks `I` leaving China for the US. |
| `us-import-tariff` | US | Charges 1 `USD` (a dial) for each unit of `I` coming into the US from China, paid by the importer to the US. |
| `cn-import-tariff` | China | Charges 1 `RMB` for each unit of `T` coming into China from the US, paid by the importer to China. |
| `us-export-tax` | US | Charges 0.5 `USD` (a dial) for each unit of `T` sold from the US to China, paid by the seller. In force from the start. |
| `us-asset-fee` | US | Charges 0.02 or 0.05 `USD` (dials) for each `USD` that China or the rest of the world Transfers to the US, paid by the lender. |

A Rule takes effect at the next round boundary and stays in force until repealed; enacting one already in force adds nothing. A block refuses each pair, gift Transfer or joint candidate Transaction it covers, and a routed order pairs like any other, so a block holds however a trade is arranged ([03 §5.5](03_premise.md#55-geography-access-and-rules)). Every kind is declared by the clearing system; each regime gives its state the authority to enact its own. `wage` and `upkeep` are declared as rule kinds too, so their payments are recorded as levies.

### 4.2 The currency market

`usd-rmb` is a dealer: it holds 150 of each currency and trades each round's net from its reserves, at a price, RMB per USD, that starts at 1 and moves half a hundredth for each USD of net buying, within 100 USD either way (dials; [02 §5.1](02_kernel.md#51-cost-function)). Its position carries from round to round, so the exchange rate is the running balance of what has been bought and sold. Every importer buys the other currency to cover its next imports; China's reserves buy USD; world savings buy USD with a fifth of their RMB each round while the asset fee is not in force, and sell 30% of their USD while it is. America's deficit weakens the dollar; China's reserves and the world's demand for dollar assets hold it up.

### 4.3 Capital

The rest of the world's savings lend the US state half their USD each round while the asset fee is not in force, and a fifth while it is. China's reserves lend the US state too. Both pay the fee on what they lend.

### 4.4 Accounting

Three identities hold every round, each checked separately:

- **Currency conservation.** Each currency's total, across every Actor and the dealer's reserves, never changes: 317 `USD` and 334 `RMB`. INV-05 checks it ([02 §9](02_kernel.md#9-architectural-invariants-and-numerics)), since no rule creates or destroys a currency.
- **Asset and liability matching.** Every open bond is its lender's claim and its borrower's liability, in one currency, at its principal, in its term and when due.
- **External consistency (INV-38).** For each country (an Actor group) and each currency, the round's change in its position (what it holds of the currency, plus what the rest of the world owes it on bonds, less what it owes) equals the flows across its border: goods, currency trades (the dealer counting as abroad), levies, and interest (lending across the border plus the change in its bonds). The engine's invariant checks it every round for every composition (`Laws.External`). Summed in the numeraire, goods, levies and interest are the current account, currency trades the financial account, and the change from the exchange rate moving is a separate valuation line.

## 5. A round

Each round follows the canonical sequence ([02 §8.1](02_kernel.md#81-canonical-resolution-sequence)):

1. **Prepare.** Rule changes enacted last round take effect, new capacity and competence arrive, last round's production is delivered, competence wears, and bonds fall due.
2. **Decide.** Each seat sets its tools from its View (§2).
3. **Routines.** Each state's commerce routine orders its production and drafts its trades and currency; each firm's supply routine orders its production and buys its input and currency; households draft their purchases and loans; the rest of the world drafts its currency and loan.
4. **Resolve.** Orders are funded from opening holdings. The Exchanges clear; blocks refuse their pairs and levies are charged on theirs; loans' principal moves and bonds are entered; production and investments use their inputs and pay their wages; upkeep is charged; requirements are consumed.
5. **Commit.** The round-end World is stored. Money received this round is spendable from the next.

## 6. How the objects interact

### 6.1 Circular flows

Four circuits close, each with a source and a sink:

1. **Domestic, within a regime.** Production pays wages; households spend at home, which is the firm's home revenue; the firm pays wages and dividends. Saving, imports and taxes leak out; transfers, interest and the state's purchases flow in.
2. **Savings, within a regime.** Households lend to their state and, in America, to their firm; China's state lends to its firm; the firm invests in capacity and competence; output and wages follow; interest and principal flow back.
3. **External, through clearing.** US importers buy RMB and pay Chinese sellers; Chinese importers buy USD and pay American sellers; China's reserves buy USD and lend it to the US state, which spends it, and US households spend part of it on imports.
4. **World savings, through clearing.** The rest of the world buys USD, lends it to the US state, and is repaid with interest: the only saving from outside both regimes.

### 6.2 Feedback loops

Twelve loops arise from the rules; none is scripted. Each holds under conditions, and some are countered by others (§10.5).

| # | Loop | Object | Sign | From the rules | Holds when |
|---|---|---|---|---|---|
| F1 | Learning by doing | Regime: capability | Reinforcing | Each unit produced teaches competence | Capacity runs |
| F2 | Industrial decay | Regime: capability | Reinforcing | Competence wears a tenth a round | Capacity stands idle |
| F3 | Repression growth model | Regime: finance and capability | Reinforcing | Repression lends the firm more, cheaply, and the firm invests on credit | Always at the default dials; the more, the less money the firm has of its own |
| F4 | Debt burden | Regime: finance | Reinforcing | Loans fall due with interest and are rolled over | The loan rate is above zero |
| F5 | Currency adjustment | Clearing | Balancing | America's imports sell USD on the dealer | Reserves and world savings do not buy it back |
| F6 | Recycling blocks adjustment | Clearing, from China's finance | Counters F5 | China's reserves and world savings buy USD | They are on, and the asset fee is not |
| F7 | Strong-dollar trap | Clearing into America's capability | Reinforcing | A substitute runs only when cheaper than importing at the rate, and an idle one decays (F2) | Its cost is within the dollar's swing of the import price, as at the default dials |
| F8 | Home market | Regime: demand | Reinforcing | Wages and dividends are household purchases at home | The firm offers at home |
| F9 | Tariff erosion | Clearing into demand | Balancing | Under its own tariff a private sector sends less abroad | A tariff is in force |
| F10 | Supply leverage | Clearing into the other's capability | Reinforcing | Under the other's export control a substitute runs only as far as its competence | Competence was lost before the control |
| F11 | Political constraint | Regime: objectives | Balancing, through behaviour | Viability is an objective, not a limit; a Strategy that weighs it delays adjustment | A Strategy responds to it, such as `Cautious` |
| F12 | Overcapacity cost | Regime: capability and demand | Balancing | Output nobody buys still pays its wages and its capacity its upkeep | A producer cannot sell what it makes |

### 6.3 The two traps

**The strong-dollar industrial trap**, America's regime and clearing together:

1. Foreign demand for dollar assets (world savings, China's reserves) buys USD and holds the dollar up.
2. A strong dollar makes Chinese industrial inputs cheap in USD: America's substitute gap turns positive (§8.2).
3. America's substitute industry stands idle, and its competence wears away through disuse (F2).
4. Substituting becomes harder: capacity built later runs only as fast as the lost competence is relearned (F10).
5. America depends more on Chinese supply, and is more exposed to a Chinese export control.
6. Chinese exporters' dollar earnings, through reserves, are lent back to the US state, financing the next round of imports.

It holds when US industry costs nearly what imports do, as at the default dials; where US industry is cheap, the dollar does not decide (§11, dollar trap). It supports US consumption and financing in the short run while making industrial independence harder.

**The industrial-surplus trap**, China's regime and clearing together:

1. Repression and directed credit favour producers: the firm borrows cheaply, its households earn little on their savings.
2. The firm invests in industrial capacity on credit (F3).
3. Production grows faster than domestic demand: China's households, spending a fifth of what they hold, absorb little of its output (§8.2).
4. The surplus depends on foreign demand, and is paid for in RMB by America's importers.
5. Export income services the firm's loans; the state refinances them, so debt grows by refinancing, not default (§10.5).
6. A state that weighs its viability is reluctant to rebalance when its industry falters (F11).

It holds while the state lends a growing share of its growing RMB. Rebalancing does not cost China output (§11, political trap), so what keeps the trap in place in Pacific is behaviour and debt, not a loss of production.

## 7. Strategies

### 7.1 Routines

Each regime has two routines ([04 §6](04_run.md#6-strategy-initialization)), written once and read through its country. Its state's commerce routine (`us-commerce-v0`, `cn-commerce-v0`) runs after the state's player: for each good it sells what is left after the requirement and planned inputs, and buys what the requirement needs, on the good's world Exchange; it orders the state's production (§3.2); it buys the other currency to cover two rounds of the foreign requirement; and it signs the bonds its state borrows on. Its firm's supply routine (`us-supply-v0`, `cn-supply-v0`) orders the input-free half of the runnable capacity as far as wages allow and the other half as far as its input and the remaining wages allow, buys `max(0, 2 × one round's input − input held)` on the world Exchange and three rounds' worth of the other currency, and signs the loans its firm borrows on.

### 7.2 Households and world savings

The households' demand and lending are in §3.3 and §3.4; the rest of the world's in §4.2 and §4.3.

### 7.3 Presets

A preset is the policy every seat plays in a simulated run, written as each tool's level (`Premises.Pacific.Strategies`: `statePolicy`, `firmPolicy`). A state preset sets each state's tools, with each firm offering all it holds on the world Exchange and its other tools off; a firm preset sets both firms' tools, with each state's tools off. The default is `FreeTrade`, played for six rounds of the 1 to 12 the composition allows; each seat's alternative, which the suite's agency runs cast it to ([04 §2](04_run.md#2-instantiation-configuration)), is `Hawk` for a state and `Builder` for a firm.

| Preset | Seat | US: barrier · industrial policy · transfer · asset fee | China: barrier · industrial policy · repression · reserves |
|---|---|---|---|
| `FreeTrade` | states | none · none · none · none | none · none · off · none |
| `Ban` | states | import ban · none · none · none | import ban · none · off · none |
| `Restrict` | states | control · none · none · none | control · none · off · none |
| `Substitute` | states | none · 1 · none · none | none · 1 · off · none |
| `StatusQuo` | states | none · none · none · none | none · none · high · high |
| `Rebalance` | states | none · none · none · high | none · none · off · none |
| `Hawk` | states | control · none · none · high | control · none · high · high |
| `Populist` | states | import ban · none · 15% · none | import ban · none · off · none |
| `Planner` | states | none · 2 · none · high | none · 2 · high · none |
| `Reformer` | states | none · 1 · 5% · none | none · 1 · off · none |

| Preset | Seat | World-market share · input inventory · capital investment · dividends |
|---|---|---|
| `Exporter` | firms | all · none · 1 unit · none |
| `Merchant` | firms | all · none · none · 15% |
| `Builder` | firms | none · none · 2 units · none |
| `Patron` | firms | none · none · none · 15% |
| `Hedger` | firms | all · 2 units · none · none |

Three state presets react. `Adaptive` builds one unit of its substitute while the other country keeps a barrier against it. `Retaliate` controls its exports to the other the round after the other raised a barrier. `Cautious` is the adaptive political Strategy F11 is tested with: China keeps its reserves on and stays rebalanced while its firm's production on US technology kept up last round (at least 0.5, a dial), and represses at the high setting when it did not. No preset repeals, since a repeal names the Rule's id; a human or agent seat can.

## 8. Objectives and reporting

### 8.1 Criteria

Each seat has four criteria, evaluated from the recorded run ([03 §10.5](03_premise.md#105-evaluation)). A seat's score is the number it meets, from 0 to 4. Each bar is set near the 40th percentile of its measure over the balance sample, so a seat meets each about three times in five (§10.4); the bars are dials.

| Seat | Criteria |
|---|---|
| Each state | **Security:** its shortfall of the good it imports is at most 19.52 (US) or 13.6 (China) over the run. **Industry:** its firm ends with at least 10.9 `K_T` (US) or 4 `K_I` (China). **Viability, every round:** US households buy at least 0.6 units of goods; or China's state and firm produce at least 8 `I` and China's firm owes nothing in arrears, in each round. **Prosperity:** its households buy at least 12.5 (US) or 0.63 (China) units of goods over the run. |
| Each firm | **Wealth:** it ends with at least 3.35 `USD` (US) or 8.85 `RMB` (China). **Growth:** the same capacity bar as its state's industry. **Output:** its input-dependent production is at least 5.1 units (US) or 1.5 (China) over the run. **Home demand:** the same bar as its state's prosperity. |

Viability is political support. It is derived from the economy, not held as a Resource: for America, what its households can buy; for China, its industry's employment, which output stands for, and its financial stability. It is an objective, not a limit: a state may break its floor and pays for it in its score (F11). China's viability is a criterion with a further condition, which must hold in the same rounds ([03 §10.5](03_premise.md#105-evaluation)).

### 8.2 What is derived, not stored

No indicator is a Resource or a stored score: each is read from the record by a generic view (`Views.Indicators`, `Views.Regimes`, `Views.Dependence`), over any composition. Competitiveness is the substitute gap, prosperity what households bought, leverage and resilience the Dependence view's exposure and coverage, viability the margin.

| Indicator | Formula | Reads |
|---|---|---|
| Competitiveness: substitute gap | For each good a country imported (bought on an Exchange quoted in a currency not its own) and can make: the wage a unit of making it pays, valued in the numeraire, less the cheapest import cost of a unit, `price × V(quote)`, at the round's opening prices | Competitiveness: above 0, the substitute stands idle |
| Net external position | `Σ_currency V(c) × (foreign currency held + owed by the rest of the world − owed to it)`; a country's own currency is what its home Exchanges quote | The external balance |
| Domestic absorption | For each good a country made: what its households bought of it over what it made | Dependence on foreign demand |
| Debt service | What a seat owed on its agreements in the round over its revenue from goods | Financial stability |
| Viability margin | For each every-round criterion: its measure less its bound, in the worst round so far | Political viability |
| Prosperity | Units of goods a country's households bought, over the run (the `bought` measure) | Household welfare, and a criterion (§8.1) |
| Leverage | For each good a country imports: the other country's exposure, `max(0, requirement − capacity)`, which its export control would turn into a shortfall; and the other's sales abroad of the good it exports, which its import ban would cut | What a barrier can cost the other (§11, international leverage) |
| Resilience | For each state and good: `coverage = min(1, capacity / requirement)`, capacity being the least it holds of what each unit of its production draws, capacity or competence | How far it could carry on under the other's control |

The run page's headline shows them at the round chosen, beside the Regimes and Clearing views ([07 §9.3](07_simulator.md#93-run-details)).

## 9. How it is built

Pacific is three Premises at version `v0`, one per economic object ([03 §2](03_premise.md#2-premises-and-compositions)). One builder makes both regimes (`Premises.Pacific.Regime`), from a configuration that is the only place the two countries' numbers and institutions differ (`configOf`); a second makes the clearing system (`Premises.Pacific.Clearing`). Both read a country only through its record (`Premises.Pacific.Country`), and the composition is the union of the three objects' parts (`Premises.Pacific.Parts`, `Premises.Pacific`).

| Premise | Adds |
|---|---|
| `us-regime` | America's state, firm and households; their production and investment Transformations; the wage and upkeep laws that pay its households; the US bond and the US firm's bond; its routines and its households' demand; its requirements; its seats' criteria. |
| `china-regime` | The same for China, with China's deposits and loans at each repression setting in place of the US bonds. |
| `international-clearing` | Every Resource, both world Exchanges, each home Exchange and the currency dealer; every kind of Rule, and the export tax in force; the groups that make each country one party to the Rules; the rest of the world's savings; and the parameters the Strategies read. |

A regime does not load alone: its Exchanges, Rules and Resources are the clearing system's.

**Dials.** `Premises.Pacific.Dials` holds every number: prices and the home premium, the currency Exchange, opening money in each currency, each country's capacity split, requirements, each investment's goods, construction wages, yield and delay, competence, the input share and input per unit, each production's wage, upkeep, the tariff and tax rates, `Cautious`'s threshold, the households' shares (`PrivateDials`) and the money between the countries (`WorldDials`: bond rates, repression's three settings, the fee, the reserves, world savings), each tool's low and high level, and the objectives' bars.

**Engine features it uses.** Trades settling as pairs, and a dealer Exchange whose price moves with its position ([02 §5](02_kernel.md#5-cost-functions-exchanges-and-solvers)); Rules acting on pairs and Transfers, and a levy paid by the receiver ([03 §5.5](03_premise.md#55-geography-access-and-rules)); Actor groups; an executor's draws on two reservable Resources, and depreciation ([03 §5.3](03_premise.md#53-transformations)); action laws charged on a Transformation's units, and stock laws charged on what each holder holds ([03 §15](03_premise.md#15-authoring-laws-and-agreements)); multi-round agreements entered by a joint candidate Transaction that Transfers and enters; a routine beside a player ([04 §6](04_run.md#6-strategy-initialization)); Aggregates ([02 §7](02_kernel.md#7-aggregates-and-representation)); the `acting` condition, kind-specific "Rules in force", last round's Transformations and the quotient expression ([03 §8](03_premise.md#8-strategy-catalog)); every-round criteria, criteria with further conditions, and the `bought`, `produced` and `arrears` measures ([03 §10.5](03_premise.md#105-evaluation)); INV-38; and a composition's default preset ([04 §2](04_run.md#2-instantiation-configuration)).

# Part II: Fixtures

Each scenario plays the composition with the presets named, the states' then the firms', and checks the round-end World and the round's records. Expected values are literals.

| Suite | Holds |
|---|---|
| `packages/handlers/test/Test/Handlers/Pacific.purs` | §10.1–§10.4 and §12 |
| `packages/handlers/test/Test/Handlers/Loops.purs` | §10.5 and §11 |
| `packages/premises/test/Test/Premises/Golden.purs` | §10.6, from `Premises.Pacific.Fixtures` |
| `packages/premises/test/Test/Premises/Compose.purs` | The three Premises and the dials |
| `packages/views/test/Test/Views/` | The Dependence view, Compare and the indicators |
| `apps/server/test/Test/Server/Compositions.purs`, `zz-pacific.spec.js` | The composition served, launched and shown |

## 10. Golden scenarios

### 10.1 Trade, barriers and substitution

Firms on the default (all offered on the world Exchange) unless named.

- **A. Free trade.** Every seat `FreeTrade`, six rounds. In round 1 the US firm exports 8 `T`; China buys 3 and its firm 1; the US buys 5 `I`, China's state sells 4 and its firm 2; the export tax is 2 `USD`. After round 1 the US holds 112.5 `USD`, the US firm 18, China 93.6 `RMB` and China's firm 17.9. Every round, 317 `USD` and 334 `RMB`.
- **B. US export control.** US `Restrict`. From round 2 China is short 3 `T`; 4 orders are `Blocked` in round 2. China's firm produces 2, 2, 1, 1, 1, 1.
- **C. China's export control.** China `Restrict`. From round 2 the US is short 5 `I`; the US firm produces 8, 8, 4, 4, 4, 4, and China, whose technology came from it, goes short of 0, 0.331, 0.327, 1.6903, 1.8295, 1.8641 `T`. By round 6 China's state holds 20 unsold `I`.
- **S. US import ban, demand pressure.** US `Ban`. China's state sells 4 `I` in round 1 and none after; its unsold inputs pile up, 4, 8, 12, 16, 20; the US, with no industrial capacity, is short 5 a round from round 2.
- **D. US substitution.** US `Substitute`. Its industrial capacity is 0, 2, 3.996, 5.992, 5.992, 5.992 by round, but its competence, 0.225, 1.1025, 2.0555, 3.0565, 3.2093, 3.3698, lets it run only 0, 1.2226, 2.2839, 3.3961, 3.5659, 3.7442.
- **E. The importer pays the tariff.** The US enacts its tariff in round 1. China still sells 4 `I` in round 2; the US firm, paying the tariff on its input, holds 19.6593 `USD` after round 2.
- **F. Adaptation.** US `Restrict`, China `Adaptive`: China's technology capacity is 0, 0, 0, 2, 4, 4. Reversed, the US's industrial capacity is 0, 0, 1.996, 3.992, 5.992, 5.992.
- **H. Invariants.** US `Hawk`, China `Reformer`, firms `Exporter`: every round passes `checkRun`, INV-38 included; the replay handler reproduces every round-end World; every reordering of each round's inputs settles to the same World; and each currency's total holds. A control enacted every round is one Rule beside the export tax. Under the US control in round 2, a US gift of 3 `T`, a joint candidate Transaction moving 1 `T`, and a Chinese routed purchase of 4 `T` at limit 1 all fail: China holds no `T`, is short 3, and at least three `Blocked` records are written.
- **Routine.** A human China seat with nothing to say: its routine sells 4 `I`. When its speech orders one technology investment, the routine sells none; an order of 4 against the maximum of 2 also leaves it selling none.
- **Dependence view.** US `Restrict`. China's technology row after rounds 2 and 6: requirement 3, capacity 0, coverage 0.00, exposure 3, no trade, shortfall 3, one barrier against it. China's industrial row after round 2: capacity 8, coverage 1.00, exports 4, receipts 8.
- **Counterfactual.** US `Restrict` against `FreeTrade`, six rounds: China's technology shortfall is 15 against 2.69; the US firm's receipts 27.56 against 45.9 `USD`, its sales at home 23.56 against 20.41 and abroad 11.77 against 35.07, and 5 of its orders blocked against none.
- **Private demand.** Free trade: China's households buy 0.77 `T` abroad in round 3, with no capacity; 4.78 over six rounds.

### 10.2 Firms, loans and the households

- **I. Loans.** China's firm `Exporter`: repressing (China `StatusQuo`), China lends it 17.6 `RMB` in round 1, a fifth of its RMB, free; rebalanced (`FreeTrade`), 4.4 at 5%. In round 2 China's households deposit 4.595 and 4.601. Either way the firm holds 3 `K_I` in round 2.
- **J. Dividends.** Both firms `Patron`. In round 1 the US firm pays its households 2.25 `USD` and China's firm 1.5 `RMB`, on top of wages; in round 2 the US firm sells 1.52 `T` at home.
- **K. Selling only at home.** Both firms `Builder`: China's firm loses its US input from round 2 and runs only its input-free half: 2, 1.5, 2, 0.0889, 1.1019, 2.3156 by round.
- **L. Levies.** The US tariff in force: the tariffs and the export tax are 2, 8.2241, 8.2161, 7.8624, 8.3017, 7.8875 by round, wages and upkeep apart.
- **M. China saves, America spends.** Both firms `Patron`: after round 6 China's households hold 9.519 `RMB` and America's 2.7215 `USD`; over the run China's bought 2.79 units of goods and America's 15.9745.
- **N. Who an export control hurts.** After six rounds under its own control China's state holds 20 unsold `I`; after four rounds under America's the US firm holds 11.45 unsold `T` and the US state none.
- **O. Input inventory.** Both firms `Hedger`. Under China's control the US firm produces 8, 8, 6.8572, 4, 4, 4, against 8, 8, 4 without an inventory (C); under the US control China's firm produces 2, 2, 2, 1.4, 1, 1, against 2, 2, 1 (B).

### 10.3 Money between the countries

- **Q. The currency.** Under free trade the dollar falls: 0.995, 0.9887, 0.9905, 0.9852, 0.973, 0.9563 `RMB` per `USD`. Under `StatusQuo` China's reserves hold it up: 1.025, 1.0344, 1.0533, 1.0657, 1.0735, 1.0774.
- **T. The rate reaches the buyer.** America's households pay 1.8556 `USD` per unit of Chinese industrial inputs under a weak renminbi (`StatusQuo`), against 2.1302 under a strong one (US `Rebalance`), over rounds 2 to 6.
- **R. The asset fee.** US `Rebalance` (the fee high): the rest of the world lends the US state 4.4 `USD` in round 2 against 11 without the fee, and sells dollars; the dollar falls to 0.995, 0.9074, 0.8321, 0.7625, 0.6918, 0.6186.

### 10.4 Balance

Over 400 settings of every seat's four tools, drawn deterministically from the 81 each seat has, the seats meet 986, 961, 976 and 984 of the 1,600 criteria each holds over the sample (US, China, US firm, Chinese firm). The bars are set so; a change to any dial is checked against this sample.

### 10.5 Circuits, accounting and loops

- **Circuits.** US transfer low, China repression and reserves low, both firms selling half at home with low investment and dividends, six rounds: every leg of the four circuits (§6.1) carries a flow, and no currency Flow has an end outside an Actor or the dealer.
- **Bonds.** Under `StatusQuo`, after round 2, every open bond has a lender and a borrower that differ; the templates open are the two US bonds, China's high deposit and China's high loan, the last two at 0%.
- **INV-38.** A currency added to China's account with no Flow behind it unbalances its account and trips INV-38; the run as played does not.
- **Repression.** Under `StatusQuo` over six rounds China's households give up 2.2598 `RMB` against the 5% rate, and its firm saves 8.5761.
- **Debt-driven postponement.** China's firm opening with no RMB, investing high and repressed: it repays every loan from the next and is never in arrears, while what it owes grows from 17.6 to 40.4807 `RMB` over eight rounds selling at home, and to 41.0457 selling abroad.
- **Arrears.** Both firms opening with no money, China under its own export control, China's firm investing high and paying high dividends, eight rounds: it cannot repay 0.0882 `RMB` in round 4, rising to 1.647 in round 8. China's output is 8, 10, 9.6557, then 8 a round, clearing its floor every round, and its viability still fails, on the arrears alone.
- **F1, F2.** Under `Substitute` America's industrial competence grows from 1.1025 after round 2 to 3.3698 after round 6; idle under free trade it wears from 0.225 to 0.1329.
- **F3.** China's firm investing high ends with 7.0785 `K_I` repressing against 4.7867 rebalanced; with no money of its own, 5.2098 against 3.7521.
- **F4.** At the off setting China's firm pays 0.9422 `RMB` of interest on the loans it repaid by round 6; at the high setting, none.
- **F5, F6.** The dollar ends round 6 at 0.9563 under free trade, and at 1.0774 with China's reserves.
- **F7.** America builds a unit of industrial capacity a round. With China's reserves on, the dollar at 1.0596 leaves it idle: it makes 1.225 units over the run and its competence ends at 2.229. With the asset fee on, the dollar at 0.5841, it makes 14.2127 and its competence ends at 3.3698.
- **F8.** Firms selling half at home: paying high dividends, America's households buy 12.5523 units at home, against 8.2937 without dividends.
- **F9.** Under its own tariff America's households import 5.1744 `I` over the run, against 6.8236 without.
- **F10.** Under China's export control with America building high, a US that kept 3 units of industrial competence is short 9.008 `I` over the run; one with a quarter of a unit, 14.5315.
- **F11.** `Cautious` China has its high deposits open in 2 rounds without a shock, and in 5 under a US export control.
- **F12.** Under its own export control the US firm ends round 6 with 19.8498 `USD` and 17.872 unsold `T`, against 26.9933 and none under free trade.
- **Indicators.** Under `StatusQuo`, after round 6: China's net external position is 30.9835, America's −38.7715 and the rest of the world's 79.8735; neither country's households absorb any of its output (every firm exports all); China's firm owes 4.6215 times what it sells, China's state 0.5678 and the US firm 0.0955; China's viability margin is 1.7748 and America's −0.6; America's substitute gap is 0.087 (idle) and China's −0.0964 (running).
- **No dead ends.** Over eight rounds of every preset, no Actor's currency runs out.

### 10.6 Engine fixtures

Literal worlds cut from Pacific with no production or requirements, run through `resolveRound` alone.

- **P-A.** The US firm sells 4 `T` to China and pays 2 `USD` of export tax: it ends with 17 `USD` and 4 `T`, China with 2 `USD` and 4 `T`, and the US state with 114 `USD`.
- **P-H.** The US firm sells 2 `T` at home to its state at 1.25, untaxed: the firm ends with 17.5 `USD` and 6 `T`, the state with 109.5 `USD` and 4 `T`.
- **P-B.** The US enacts its export control in round 1; in round 2 the same exports move nothing: the firm keeps 15 `USD` and 8 `T`, China 6 `USD` and no `T`.
- **P-G.** China's firm offers 2 `I` and nobody buys: it keeps 10 `RMB` and 2 `I`.
- **P-C.** China's firm invests one unit, of 2 `I`: in round 2 it holds 3 `K_I` and 3.33 `S_I`.
- **P-R.** The US transfers 10 `USD` to its households, which opened with 10, and in round 2 they buy 2 `T` at home from the US state at 1.25: the state ends with 104.5 `USD`, its households with 17.5 and the 2 `T`.
- **P-X.** The currency dealer. The US firm sells 10 `USD` in round 1 at an average of 0.975, getting 9.75 `RMB`: it holds 5 `USD` and 13.75 `RMB`, the dealer 160 and 140.25. In round 2 China buys 10 `USD` back for the same 9.75 `RMB`, and the dealer is back at 150 and 150.
- **P-F.** The US enacts its high asset fee in round 1. In round 2 China lends the US 4 `USD` on the US bond and pays 0.2 of fee: China holds 1.8 and the US 116.2. Nothing moves in round 3, the bond's term; in round 4 the US repays 4.2: China holds 6 and the US 112.

## 11. Experiments

Each experiment states the conditions under which its feedback appears, and the case where a countervailing force undoes it.

- **Political trap.** States with China's reserves on, firms exporting all, six rounds. China's industrial output is 10, 10, 9.8889, 9.9929, 9.7748, 9.9966 repressing and 10, 10, 9.8889, 9.9929, 9.7732, 9.9945 rebalanced: rebalancing costs it no output. A US export control cuts it to 9 a round, still above its floor of 8. Its households buy 4.5845 units rebalanced against 4.5198 repressed. Repression alone does not produce a political trap in Pacific; what keeps China repressing is a Strategy that weighs its viability (F11) and the debt its firm carries.
- **Dollar trap.** America building one unit of industrial capacity a round, China repressing with reserves. The asset fee takes the dollar from 1.0589 to 0.7209 after round 6. US industry, costing nearly what imports do, makes 1.225 units under the strong dollar and 14.2316 under the weak one, and America's households, paid its wages, buy 11.4615 units against 9.3683. Where US industry is cheap (a wage of 1.5), it makes 14.2316 either way.
- **Lost knowledge.** Two US starting positions with 4 units of industrial capacity, one with 6 units of competence and one with a quarter, under China's export control. The skilled one makes 4 a round and is short 1 from round 2; the unskilled one makes 0.25 rising to 0.3191 and is short 4.75 falling to 4.6961. Dependence is a shortage of capital only where the workforce is still there.
- **International leverage.** China's export control (supply) costs America 5 `I` a round while it builds nothing, and while it builds its substitute the shortfall falls 5, 3.7774, 2.7161, 1.6039, 1.4341, 1.2558, 1.0686 from round 2 to round 8: substitution erodes supply leverage. China's import ban (demand) cuts the US firm's sales abroad from 60.7592 to 35.5556 units over eight rounds and its money from 29.9179 to 20.7627 `USD`, while its debt barely moves (1.2118 against 1.2432): demand pressure costs sales and money, not solvency, since the firm's borrowing is set by its households' lending.
- **Mutually reinforcing dependence.** Under the status quo China lends the US state 0, 3, 3.3333, 6.4692, 7.1919, 10.3526 `USD` by round, while America buys 6, 6, 6, 5.8889, 5.9929, 5.7748 units of China's industrial inputs and China's industry produces about 10 a round: China's export earnings finance US borrowing, and US purchasing power sustains Chinese output.
- **Status quo against rebalancing.** Over eight rounds, each country building one unit of its substitute a round and each firm selling half at home, four arms; then each arm again with the other country's export control from round 5.

| Arm | US viability margin | China's | Dollar after round 8 | US owes abroad | US `S_I` after round 4 | China's `S_T` | US short under China's control | China short under America's |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| Both keep their arrangements | 0.4872 | 0.2966 | 1.2221 | 83.1047 | 2.7519 | 1.845 | 11.9118 | 2.6403 |
| America rebalances alone | 0.4872 | 0.2765 | 0.849 | 36.3737 | 3.0605 | 1.845 | 3.7437 | 5.5435 |
| China rebalances alone | 0.4324 | 0.2924 | 1.007 | 46.8063 | 2.7519 | 1.845 | 11.8548 | 2.65 |
| Both rebalance | 0.4838 | 0.2412 | 0.6205 | 2.4262 | 3.0605 | 1.71 | 3.7437 | 7.7485 |

The result is that, if both keep their arrangements, America is the more vulnerable. The strong dollar idles its substitute industry, its competence lags, and China's export control costs it 11.9118 `I` against 3.7437 when it has rebalanced. America's arrangement keeps China's exposure low: when America rebalances, the weak dollar makes US technology cheap, China's substitute stands idle, and China is short 5.5435 to 7.7485 `T` under America's control against 2.6403. Each country's rebalancing shifts exposure to the other; neither is less exposed in total.

## 12. Sensitivity

Each dial changed alone: spend shares of 0.6 (US) and 0.3 (China), a US capacity split of 4 : 6, a Chinese split of 6 : 4, a tax rate of 0.25 or 1, China lending its firm 10% of its RMB at the off setting, and a home premium of 1.5. In every variant China's own export control leaves it holding industrial inputs it could not export, a firm's input inventory keeps it producing more under a control on its input, China's households lend their state more than America's lend theirs, and the US state collects tax on its firm's exports.
