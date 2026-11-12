# PremiseComposition: Pacific

**Status:** Experimental configuration.  
**PremiseComposition ID:** `pacific`, version `v0`

## 1. The game

Two countries, the United States and China, each make something the other needs. The US makes technology and China makes industrial inputs, and each needs some of both. Each has its own currency: the dollar (`USD`) and the renminbi (`RMB`). Trading leaves both better off, and gives each a hold over the other.

Pacific is about economic hysteresis. Each country lives by an arrangement that pays now and makes changing course dearer the longer it lasts:

- **China** holds household income down to finance production. Its households are paid little interest on what they save, the state lends it to industry cheaply, and the industry it builds depends on foreign demand.
- **The United States** lives on foreign demand for dollar assets. The world's savings, and China's reserves, buy US bonds and hold the dollar up; a strong dollar makes imports cheap and leaves US industry idle, and an idle industry loses the competence to run it, which money alone cannot buy back.

Both arrangements meet in the same currency market. China's surplus and the world's savings hold the dollar up; the strong dollar keeps America importing; America's imports keep China's industry selling. Political viability, debt, industrial competence and the exchange rate all arise from a few rules (§4.3), and each state is scored on its viability every round (§7), so a reform that pays later must survive the rounds it costs.

Each country is played by two seats: its **state** and its **firm**. Behind them stands the country's **private sector**, which has no seat: it is paid wages for what its country produces, spends a share of what it holds, and lends part of the rest to its state. A third Aggregate with no seat, the **rest of the world's savings**, buys and lends dollars. Every seat balances two dilemmas, one abroad and one at home, and holds four tools, two for each, each set off, low or high every round.

| Seat | Its dilemma |
|---|---|
| US state | **Purchasing power or industrial renewal.** Keep the benefits of the strong dollar, cheap imports and foreign lending, or accept dearer imports and a weaker dollar to rebuild production at home. Abroad: a trade barrier and industrial policy. At home: a transfer to households, and a fee on foreign lending. |
| China state | **Industrial competitiveness or household income.** Keep producers' cheap credit, paid for by its households, or rebalance income toward them. Abroad: a trade barrier and industrial policy. At home: financial repression, and dollar reserves. |
| US firm | **Returns abroad or capability at home.** Sell on the larger world market, exposed to China's barriers, or at home; distribute its earnings or invest in capacity and the workforce that runs it. |
| China firm | **Capacity or profitable demand.** Expand on the state's loans, or serve a home market its households keep small by saving, and pay them more of what it earns. |

The two countries are built differently, so the same dilemmas weigh differently.

| | China: state-led | United States: market-led |
|---|---|---|
| Capacity | The state holds most of China's industrial capacity (8 of 10). | The firm holds most of America's technology capacity (8 of 10). |
| Exports | Mostly the state's. | Mostly the firm's. |
| State income | Its export sales, any tariff, its households' deposits, and the interest on its loans to its firm. | A tax on US exports, in force from the start, any tariff, its households' lending, and foreign lending. |
| Lending | Asymmetric: the state sets both ends of one pipe, what it pays its households for their savings and what it charges its firm to borrow them. | Asymmetric: the US firm finances its own investment; the state borrows from its households and from abroad, and its lever is what foreign lenders pay to lend. |
| Private sector | Spends 20% of what it holds and lends 40% to its state each round. | Spends 80% and lends 10%. |
| Industrial competence | Strong in industrial inputs, none in technology. | Strong in technology, a quarter of a unit in industrial inputs. |

The economy is a circular flow in two currencies. Producing pays wages to households; households spend at home and abroad, which is the producers' revenue; what they do not spend they save, and part of their savings they lend to their state, which repays it with interest. A good is priced in its producer's currency, so every import needs the other currency first, bought on the currency Exchange, and the exchange rate is what an import costs. A producer that cannot pay its wages produces less; goods nobody buys still cost their wages; an input cut off abroad cuts output, wages and spending at home.

```text
                 UNITED STATES (USD)                                        CHINA (RMB)

  ┌─────────────────────────────────────┐                    ┌─────────────────────────────────────┐
  │ state                               │                    │ state                               │
  │  abroad  barrier · industrial policy│ ◀── I, for RMB ─── │  abroad  barrier · industrial policy│
  │  at home transfer · asset fee       │ ─── T, for USD ──▶ │  at home repression · reserves      │
  │  K_T 2 · K_I 0 · S_I ¼              │                    │  K_I 8 · K_T 0 · S_T 0              │
  │  borrows: US bonds ◀────────────────┼── reserves USD ─── │  lends its firm; takes deposits     │
  └──────────┬───────────────▲──────────┘                    └──────────┬───────────────▲──────────┘
             │               │                                          │               │
   control ▼ │               │ ▲ export tax                   loans ▼   │               │ ▲ interest
   barrier ▼ │               │ ▲ tariff on input        barrier ▼       │               │ ▲ tariff on input
             │               │                                          │               │
  ┌──────────┴───────────────┴──────────┐                    ┌──────────┴───────────────┴──────────┐
  │ firm                                │ ─── sells T ─────▶ │ firm                                │
  │  abroad  world share · inventory I  │ ◀── sells I ────── │  abroad  world share · inventory T  │
  │  at home investment · dividends     │                    │  at home investment · dividends     │
  │  K_T 8, half runs on 1 I a round    │                    │  K_I 2, half runs on 1 T a round    │
  └──────────┬──────────────────────────┘                    └──────────┬──────────────────────────┘
             │ wages, dividends, transfers                              │ wages, dividends
             ▼                                                          ▼
     private sector: spends most,                               private sector: saves most,
     lends its state at 5%                                      lends its state at the rate it sets
             │                                                          │
             └──── USD ⇄ RMB on the currency Exchange (usd-rmb, a dealer) ◀──── rest of the world's savings:
                                                                              buys USD, lends the US state
```

Each arrow between the countries is goods, paid for in the seller's currency at the Exchange's price, or money lent. Every producer pays its wages to its own private sector; every importer buys the other currency on the currency Exchange; every lender holds a bond (§5.4).

Every outcome is a change in holdings, flows, production or the Rules in force. All values are normalized game units.

## 2. Seats

Each seat holds four tools, each set off, low or high every round, so every seat chooses among 3⁴ = 81 settings a round. The countries differ in where they start, not in how many choices their seats have.

| Seat | Actor | # | Tool | Off / low / high | What it trades off |
|---|---|---|---|---|---|
| US state | `US` | 1 | Trade barrier | none / a tariff on Chinese industrial inputs, paid by the importer / an export control on US technology to China | Tariff income, or denying China technology, against its importers' cost, its firm's sales and retaliation |
| | | 2 | Industrial policy: build industrial capacity, and train the workforce to run it | none / 1 unit / 2 units a round | Technology and construction wages now, for less dependence on China later |
| | | 3 | Transfer to US households | none / 5% / 15% of its USD a round | Its own USD, for household purchases and home demand |
| | | 4 | Asset fee on foreign lending to the US | none / 0.02 / 0.05 USD per USD lent | Fee income and a weaker dollar, which makes US industry competitive, against foreign financing and dearer imports |
| China state | `CN` | 1 | Trade barrier | none / a tariff on US technology, paid by the importer / an export control on Chinese industrial inputs to the US | Tariff income, or denying America inputs, against its own exporters' sales and retaliation |
| | | 2 | Industrial policy: build technology capacity, and train the workforce to run it | none / 1 unit / 2 units a round | Industrial inputs and construction wages now, for less dependence on the US later |
| | | 3 | Financial repression | off: households paid 5%, the firm lent 5% of the state's RMB at 5% / low: 2%, 10% at 1% / high: nothing, 20% free | Cheap credit for industry, against household income and home demand |
| | | 4 | Dollar reserves | none / 3 / 6 USD bought a round, and lent to the US state | A weaker renminbi and dearer US technology, against RMB spent on dollar claims |
| US firm | `US-firm` | 1 | World-market share: technology offered on the world Exchange, the rest at home | none / half / all | A larger market, against exposure to China's barrier and the export tax |
| | | 2 | Input inventory: stockpile industrial inputs | none / 1 / 2 units a round, within half its RMB | Production that continues into a Chinese export control, against RMB tied up |
| | | 3 | Capital investment in technology capacity | none / 1 unit / 2 units a round | Capacity, competence and future sales, against goods and construction wages now |
| | | 4 | Dividends to US households | none / 5% / 15% of its USD a round | Its own USD, for a larger home market |
| China firm | `CN-firm` | 1 | World-market share: industrial inputs offered on the world Exchange, the rest at home | none / half / all | A larger market, against exposure to the US barrier |
| | | 2 | Input inventory: stockpile technology | none / 1 / 2 units a round, within half its USD | Production that continues into a US export control, against USD tied up |
| | | 3 | Capital investment in industrial capacity | none / 1 unit / 2 units a round | Capacity and future sales, against goods and construction wages, which its state's loans pay for |
| | | 4 | Dividends to Chinese households | none / 5% / 15% of its RMB a round | Its own RMB, for a home market that grows slowly, since households save |

The tools meet across the seats. A state's barrier cuts the other country's sales; an export control denies the other's firm the input half its capacity runs on, which is why a firm holds an inventory. The asset fee, China's reserves and the rest of the world's savings all move the dollar, and the dollar decides whether each country's substitute industry runs (§3.2). Repression sets what China's firm pays for its loans and what its households earn on their deposits. Wages, a transfer and dividends all reach the same households, whose spending is the home market where a firm sells what it does not offer abroad.

A firm's world-market share applies to what it holds of its own good after its investment's inputs: that share is offered on the world Exchange, and the rest on its home Exchange at the home price. A seat's investment is never ordered beyond the goods and the money for its construction wages it holds.

The US firm keeps dividends as its fourth tool rather than buying financial assets: with dividends every seat's choices balance (§9.4), and dividends are the firm's share of the same choice between investment and household income that repression is for China.

What no seat decides runs by itself (§6.1): a state's routine trade, its currency and its own production, a firm's production and the purchase of its input and its currency, the private sectors' and the rest of the world's spending and lending, wages, and the export tax. Each seat may be played by a human, an AI agent or a preset (§6.3), and is briefed with its description, which names its four tools, and its objective (§7).

## 3. The economy

### 3.1 Resources and holdings

| Resource | ID | Moves | Use |
|---|---|---|---|
| Dollar | `USD` | Freely | The numeraire, and America's currency. Its total is fixed at 352: only trades, Transfers, levies, wages and bonds move it, and lending moves money and creates a claim, never money. |
| Renminbi | `RMB` | Freely | China's currency, its total fixed at 374 likewise. |
| Technology | `T` | Freely | A good, priced in USD; consumed by its holders' requirements, by private sectors when bought, and by the Chinese firm's production. |
| Industrial inputs | `I` | Freely | A good, priced in RMB; consumed likewise, and by the US firm's production. |
| Technology capacity | `K_T` | Not at all | Produces `T`: held for the round by each unit of production. |
| Industrial capacity | `K_I` | Not at all | Produces `I`, likewise. |
| Technology competence | `S_T` | Not at all | What a workforce knows how to run of `K_T`: held for the round by each unit of production, learned and lost (§3.2). |
| Industrial competence | `S_I` | Not at all | The same for `K_I`. |

| Opening holding | US | US firm | US private | CN | CN firm | CN private | Rest of world | Currency dealer |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| `USD` | 112 | 50 | 10 | 6 | 4 | | 20 | 150 |
| `RMB` | 12 | 4 | | 88 | 50 | 10 | 60 | 150 |
| `T` | 2 | 8 | | 0 | 1 | | | |
| `I` | 0 | 1 | | 8 | 2 | | | |
| `K_T` | 2 | 8 | | 0 | | | | |
| `K_I` | 0 | | | 8 | 2 | | | |
| `S_T` | 3 | 12 | | 0 | | | | |
| `S_I` | 0.25 | | | 12 | 3 | | | |

The goods are one production cycle's output, available in round 1. Each firm holds one round's input, and each importer two rounds' worth of the other currency. Each private sector starts with 10 of its own currency. A producer running its capacity fully holds competence at 1.5 times it (§3.2), so each country opens there in its own good; America has a quarter of a unit of industrial competence. The rest of the world's RMB stands for all its non-dollar savings. The US starts with more money than China because its state spends on imports and China's state earns from exports; the split is a dial, set so neither state is favoured (§9.4).

### 3.2 Production, competence, wages and requirements

- **Production.** All production is a Transformation (§5.2), delivered the next round. Each unit holds one unit of capacity and one of the competence to run it for the round, and pays its producer's private sector a wage in the producer's currency: 0.25 for a country's own good, 1.95 `USD` for a US unit of industrial inputs and 0.97 `RMB` for a Chinese unit of technology (dials).
- **Competence.** Each unit produced teaches 0.15 of a unit of its competence, and every holder's competence wears a tenth away each round (dials). So a producer running fully settles at 1.5 times its capacity, one that stops loses competence, and capacity runs only as far as competence allows: `units ≤ min(K, S)`. Each investment also trains a unit of competence per unit (§5.2).
- **The states' production.** Each state runs its own good's capacity as far as its competence and the wages it can pay allow. It runs its substitute, the other country's good, only when that is cheaper than importing at the round's prices: the US makes industrial inputs when its wage is at most their price in RMB divided by the exchange rate (and the tariff, if its own is in force), China makes technology when its wage is at most its price in USD times the rate. Under the other country's export control it produces regardless. Each substitute costs a little under what importing it costs at one RMB per USD, so the exchange rate decides whether it runs.
- **The firms' production** runs in two parts. Half the firm's runnable capacity (a dial) needs no input. The other half needs the foreign good: 1/4 of a unit of `I` per unit of the US firm's output, 1 unit of `T` per unit of the Chinese firm's, so each firm needs one unit of input a round at full capacity.
- **States' requirements.** Each round the US consumes 6 `T` and 5 `I`, and China 3 `T` and 4 `I`, after the round's trades. What cannot be met is a shortfall.
- **Private sectors' requirements.** A private sector consumes whatever it buys.

### 3.3 Exchanges

| Exchange | Base | Quote | Open to | Price |
|---|---|---|---|---:|
| `technology-world` | `T` | `USD` | every Actor | 1 |
| `industry-world` | `I` | `RMB` | every Actor | 2 |
| `technology-us` | `T` | `USD` | the US, its firm and its private sector | 1.25 |
| `industry-cn` | `I` | `RMB` | China, its firm and its private sector | 2.5 |
| `usd-rmb` | `USD` | `RMB` | every Actor and the rest of the world | 1, moving |

A home Exchange pays its sellers a premium of a quarter (a dial). The goods Exchanges hold no reserves, so every trade is a pair between a buyer and a seller ([02 §5.2](02_kernel.md#52-exchanges)): a seller is paid only for what a buyer takes, and when one side is longer it is cut back pro rata. Each firm and each private sector counts as its country for Rules, so a trade at home is never foreign.

### 3.4 The currency Exchange and the exchange rate

`usd-rmb` is a dealer: it holds 150 of each currency and trades each round's net from its reserves, at a price, RMB per USD, that starts at 1 and moves half a hundredth for each USD of net buying, within 100 USD either way (dials; [02 §5.1](02_kernel.md#51-cost-function)). Its position carries from round to round, so the exchange rate is the running balance of what has been bought and sold. Net buying of dollars strengthens the dollar, net selling weakens it.

Who trades there:

- **Every importer** buys the other currency to cover its next imports: each state two rounds of its foreign requirement, each firm three rounds of its input, each private sector the share of its spending it sends abroad. America's importers sell USD for RMB; China's buy USD with RMB.
- **China's reserves** (state tool 4) buy USD with RMB.
- **The rest of the world's savings** buy USD with a fifth of their RMB each round while the asset fee is not in force, and sell 30% of their USD while it is.

So America's deficit weakens the dollar, and China's reserves and the world's demand for dollar assets hold it up.

## 4. A round

### 4.1 The sequence

Each round follows the canonical sequence ([02 §8.1](02_kernel.md#81-canonical-resolution-sequence)):

1. **Prepare.** Rule changes enacted last round take effect, new capacity and competence arrive, last round's production is delivered, competence wears, and bonds fall due.
2. **Decide.** Each seat sets its tools from its View (§2).
3. **Routines.** Each state's commerce routine orders its production and drafts its trades and its currency; each firm's supply routine orders its production and buys its input and its currency; each private sector drafts its purchases and its loan; the rest of the world drafts its currency and its loan.
4. **Resolve.** Orders are funded from opening holdings. The Exchanges clear; export controls refuse their pairs and levies are charged on theirs; loans' principal moves and bonds are entered; production and investments use their inputs and pay their wages; requirements are consumed.
5. **Commit.** The round-end World is stored. Money received this round is spendable from the next.

### 4.2 Accounting

Three identities hold every round, and each is checked separately (§9.5):

- **Currency conservation.** Each currency's total, across every Actor and the dealer's reserves, never changes: 352 `USD` and 374 `RMB`. INV-05 checks it ([02 §9](02_kernel.md#9-architectural-invariants-and-numerics)), since no rule creates or destroys a currency.
- **Asset and liability matching.** Every open bond is its lender's claim and its borrower's liability, in one currency, at its principal.
- **External balance.** For each country (an Actor group: its state, firm and private sector) and each currency, the round's change in its position equals the flows across its border. Its position is what it holds of the currency, plus what the rest of the world owes it in bonds, less what it owes the rest of the world. The flows are four: **goods** (paid for goods sold abroad, less paid for goods bought from abroad), **currency trades** (bought on the currency Exchange, less sold, the dealer counting as abroad), **levies** (received from foreign payers, less paid abroad), and **interest** (lending across the border, principal and repayments, plus the change in its bonds, so a loan is not income and a repayment with interest is). Whatever else crossed the border is the residual, which is zero. Summed over currencies in the numeraire at the round's closing values, goods, levies and interest are the current account, currency trades the financial account, and the change in the opening position's value from the exchange rate moving is a separate valuation line.

The Finance view shows the open bonds, the external accounts and the current account each round ([07 §9.2](07_simulator.md#92-run-details)).

### 4.3 Circular flows and feedback loops

Four circuits close, each with a source and a sink:

1. **Domestic.** Production pays wages to households; households spend at home, which is the firm's home revenue; the firm pays wages and dividends. Saving, imports and taxes leak out; transfers, interest and the state's purchases flow in.
2. **Savings.** Households lend to their state; China's state lends to its firm, which invests in capacity and competence; output and wages follow; interest and principal flow back.
3. **External.** US importers buy RMB and pay Chinese sellers; Chinese importers buy USD and pay American sellers; China's reserves buy USD and lend it to the US state, which spends it, and US households spend part of it on imports.
4. **World savings.** The rest of the world buys USD, lends it to the US state, and is repaid with interest: the only saving from outside both countries.

Twelve feedback loops arise from the rules. None is scripted; each holds under conditions, and some are countered by others (§9.5).

| # | Loop | Sign | From the rules | Holds when |
|---|---|---|---|---|
| F1 | Learning by doing | Reinforcing | Each unit produced teaches competence (§3.2) | Capacity runs |
| F2 | Industrial decay | Reinforcing | Competence wears a tenth a round | Capacity stands idle |
| F3 | Repression growth model | Reinforcing | Repression lends the firm more, cheaply (§5.5) | The firm's investment is short of money; otherwise loans change nothing |
| F4 | Debt burden | Reinforcing | Loans fall due with interest, and are rolled over (§5.4) | The loan rate is above zero |
| F5 | Currency adjustment | Balancing | America's imports sell USD on the dealer (§3.4) | Reserves and world savings do not buy it back |
| F6 | Recycling blocks adjustment | Counters F5 | China's reserves and world savings buy USD | They are on, and the asset fee is not |
| F7 | Strong-dollar trap | Reinforcing | A substitute runs only when cheaper than importing at the rate (§3.2), and an idle one decays (F2) | Its cost is within the dollar's swing of the import price |
| F8 | Home market | Reinforcing | Wages and dividends are household purchases at home | The firm offers at home |
| F9 | Tariff erosion | Balancing | Under its own tariff a private sector sends less abroad (§6.2) | A tariff is in force |
| F10 | Restriction leverage | Reinforcing | Under the other's export control a substitute runs only as far as its competence | Competence was lost before the control |
| F11 | Political constraint | Balancing, through behaviour | Viability is an objective (§7), not a limit; a Strategy that weighs it delays adjustment | A Strategy responds to it, such as `Cautious` (§6.3) |
| F12 | Overcapacity cost | Balancing | Output nobody buys still pays its wages | A producer cannot sell what it makes |

## 5. The levers

### 5.1 Barriers, the export tax and the asset fee

| Rule kind | Governor | Effect |
|---|---|---|
| `us-import-tariff` | US | Charges 1 `USD` (a dial) for each unit of `I` coming into the US from China, paid by the importer to the US. |
| `us-export-control` | US | Blocks `T` leaving the US for China. |
| `cn-import-tariff` | China | Charges 1 `RMB` (the same dial) for each unit of `T` coming into China from the US, paid by the importer to China. |
| `cn-export-control` | China | Blocks `I` leaving China for the US. |
| `us-export-tax` | US | Charges 0.5 `USD` (a dial) for each unit of `T` sold from the US to China, paid by the seller to the US. In force from the start. |
| `us-asset-fee` | US | Charges 0.02 or 0.05 `USD` (dials) for each `USD` that China or the rest of the world Transfers to the US, paid by the lender to the US. |

A Rule takes effect at the next round boundary and stays in force until repealed; enacting one already in force adds nothing. An export control refuses each pair between the two countries' orders: neither the good nor the money moves, the seller keeps its goods, and both orders are recorded as `Blocked`. It refuses a gift Transfer or a joint candidate Transaction between them in the same way, and a routed order pairs like any other, so a control holds however a trade is arranged ([03 §5.5](03_premise.md#55-geography-access-and-rules)). Every kind is declared with the economy, so a routine can weigh a barrier before any is enacted; the strategic Premise gives each state the authority to enact its own. A tariff is paid by the importer in its own currency, so a state that imports pays its own tariff to itself, and its firm and private sector pay it to the state. The asset fee falls on a loan's principal because the principal moves as a Transfer (§5.4), which the Rules in force screen; the rest of the world sees it in force and lends less (§6.2). The wage is declared as a rule kind too, `wage`, so its payments are recorded as levies.

### 5.2 Production and investment

| Transformation | Executor | Uses | Creates | Arrives |
|---|---|---|---|---|
| `us-technology-production`, `us-industry-production` | US | 1 `K_T` and 1 `S_T`, or 1 `K_I` and 1 `S_I`, held for the round | +1 `T` or +1 `I`, and 0.15 of its competence | the next round |
| `cn-technology-production`, `cn-industry-production` | CN | the same | the same | the next round |
| `us-firm-base-production` | US firm | 1 `K_T` and 1 `S_T`, held | +1 `T`, +0.15 `S_T` | the next round |
| `us-firm-production` | US firm | 1/4 `I`, and 1 `K_T` and 1 `S_T` held | +1 `T`, +0.15 `S_T` | the next round |
| `cn-firm-base-production` | CN firm | 1 `K_I` and 1 `S_I`, held | +1 `I`, +0.15 `S_I` | the next round |
| `cn-firm-production` | CN firm | 1 `T`, and 1 `K_I` and 1 `S_I` held | +1 `I`, +0.15 `S_I` | the next round |
| `us-industrial-investment` | US | 2 `T`; 2 `USD` of construction wages | +2 `K_I`, +1 `S_I` | the next round |
| `cn-technology-investment` | CN | 4 `I`; 2 `RMB` of construction wages | +2 `K_T`, +1 `S_T` | two rounds later |
| `us-firm-investment` | US firm | 2 `T`; 4 `USD` of construction wages | +1 `K_T`, +1 `S_T` | the next round |
| `cn-firm-investment` | CN firm | 2 `I`; 4 `RMB` of construction wages | +1 `K_I`, +1 `S_I` | the next round |

Each unit of production also pays its wage (§3.2), and each unit of investment its construction wages to its country's private sector. Production is bounded by the capacity and competence held; each investment is at most two units per Actor per round. Capacity and competence produce from the round after they arrive.

### 5.3 Transfers and dividends

The US state's transfer to its private sector, and a firm's dividends to its own, are ordinary Transfers of a share of its own currency, the private sector's to spend from the next round.

### 5.4 Bonds

Every loan in Pacific is one bond template, set by its currency and rate: the lender Transfers the principal to the borrower, and they enter the bond, which the borrower repays the next round with the bond's interest. What the borrower cannot pay carries another round, with interest again; if it holds less than one unit of the currency, the bond lapses and the lender loses what is owed ([03 §15](03_premise.md#15-authoring-laws-and-agreements)). The loan is a joint candidate Transaction the borrower signs.

| Template | Currency | Rate | Lender → borrower |
|---|---|---:|---|
| `us-bond` | `USD` | 5% | US households, China's state and the rest of the world → the US state |
| `cn-deposit-off`, `-low`, `-high` | `RMB` | 5%, 2%, 0% | China's households → China's state |
| `cn-loan-off`, `-low`, `-high` | `RMB` | 5%, 1%, 0% | China's state → China's firm |

Each round America's households lend their state 10% of their USD, and China's 40% of their RMB (dials). The US commerce routine signs every US bond, so the US state borrows what it is offered. China's households offer a deposit at each repression setting and China's state signs only the one at its setting, so the others lapse; its firm signs every loan its state offers.

### 5.5 Financial repression and dollar reserves

Financial repression (China's tool 3) is one lever over both ends of China's savings: each round the state lends its firm a share of its RMB (5%, 10% or 20%) at the setting's loan rate, and signs its households' deposits at the setting's deposit rate. It moves income only through those rates and volumes: wages and spending shares stay fixed, so the model can tell whether repression alone produces China's imbalance. What it moves is reported from the bonds: households give up their deposits times 5% less their rate, and the firm saves its loans times 5% less its rate. Under the status quo (`StatusQuo`, §6.3) over six rounds, China's households give up 1.5888 `RMB` and its firm saves 5.2504.

The answer the model gives: **repression alone does not produce the imbalance.** It moves a few RMB a round from households to the firm, but China's industry is not short of money, so repressed or rebalanced it produces the same (§10). Repression raises investment only where the firm's investment is short of money (F3), and the imbalance in Pacific comes from what is built in: China's households saving 80% of what they hold, and the dollar held up by reserves and world savings.

Dollar reserves (China's tool 4) buy 3 or 6 USD a round on the currency Exchange and lend the US state whatever China holds beyond two rounds of its technology imports, on the US bond.

## 6. Strategies

### 6.1 Routines

`pacific-commerce-v0` is each state's routine ([04 §6](04_run.md#6-strategy-initialization)). It runs after the state's player and reads what the player put forward. For each good it sells `max(0, holding − requirement − planned inputs)` and buys `max(0, requirement − holding)` on the good's world Exchange. It orders the state's production (§3.2), and buys the other currency to cover two rounds of its foreign requirement. It reads each state's own requirement (`Due`) and planned investment (`Planned`), so one routine serves both. It signs the US bonds.

`pacific-supply-v0` is each firm's routine. It orders the input-free half of its runnable capacity as far as wages allow, and the other half as far as its input and the remaining wages allow; it buys `max(0, 2 × one round's input − input held)` on the world Exchange, and the other currency to cover three rounds of its input. What a firm holds beyond that is its inventory. It signs China's loans.

### 6.2 Private demand and world savings

Each private sector buys its own good at home with half its spend share of what it holds. It turns the other half into the foreign currency on the currency Exchange (under its own country's tariff, only the part that leaves room for the tariff at the price), and buys the foreign good abroad with all the foreign currency it holds. The spend share is America's 0.8 and China's 0.2 (dials). It also lends to its state (§5.4).

The rest of the world's savings, while the asset fee is not in force, turn a fifth of their RMB into USD and lend the US state half their USD each round; while it is, they lend a fifth and sell 30% of their USD (dials).

### 6.3 Presets

A preset is the policy every seat plays in a simulated run, written as each tool's level (`Premises.Pacific.Strategies`: `statePolicy`, `firmPolicy`). A state preset sets each state's tools, with each firm offering all it holds on the world Exchange and its other tools off; a firm preset sets both firms' tools, with each state's tools off. The default is `FreeTrade`.

| Preset | Seat | US: barrier · industrial policy · transfer · asset fee | China: barrier · industrial policy · repression · reserves |
|---|---|---|---|
| `FreeTrade` | states | none · none · none · none | none · none · off · none |
| `Tariff` | states | tariff · none · none · none | tariff · none · off · none |
| `Restrict` | states | control · none · none · none | control · none · off · none |
| `Substitute` | states | none · 1 · none · none | none · 1 · off · none |
| `StatusQuo` | states | none · none · none · none | none · none · high · high |
| `Rebalance` | states | none · none · none · high | none · none · off · none |
| `Hawk` | states | control · none · none · high | control · none · high · high |
| `Populist` | states | tariff · none · 15% · none | tariff · none · off · none |
| `Planner` | states | none · 2 · none · high | none · 2 · high · none |
| `Reformer` | states | none · 1 · 5% · none | none · 1 · off · none |

| Preset | Seat | World-market share · input inventory · capital investment · dividends |
|---|---|---|
| `Exporter` | firms | all · none · 1 unit · none |
| `Merchant` | firms | all · none · none · 15% |
| `Builder` | firms | none · none · 2 units · none |
| `Patron` | firms | none · none · none · 15% |
| `Hedger` | firms | all · 2 units · none · none |

Three state presets react. `Adaptive` builds one unit of its substitute while the other country keeps a barrier against it. `Retaliate` controls its exports to the other the round after the other raised a barrier. `Cautious` is the adaptive political Strategy F11 is tested with: China keeps its reserves on and stays rebalanced while its firm's production on US technology kept up last round (at least 0.5, a dial), and represses at the high setting when it did not; America's tools are off. No preset repeals, since a repeal names the Rule's id; a human or agent seat can.

## 7. Objectives and reporting

Each seat has four criteria, evaluated from the recorded run ([03 §10.5](03_premise.md#105-evaluation)). A seat's score is the number it meets, from 0 to 4. Each bar is set near the 40th percentile of its measure over the balance sample, so a seat meets each about three times in five (§9.4); the bars are dials.

| Seat | Criteria |
|---|---|
| Each state | **Security:** its shortfall of the good it imports is at most 18.5 (US) or 13 (China) over the run. **Industry:** its firm ends with at least 12.9 `K_T` (US) or 6.5 `K_I` (China). **Viability, every round:** US households buy at least 1.25 units of goods, or China's state and firm produce at least 9.25 `I`, in each round of the run. **Prosperity:** its households buy at least 14.5 (US) or 1.25 (China) units of goods over the run. |
| Each firm | **Wealth:** it ends with at least 3.9 `USD` (US) or 17.5 `RMB` (China). **Growth:** the same capacity bar as its state's industry. **Output:** its input-dependent production is at least 11.7 units (US) or 1.65 (China) over the run. **Home demand:** the same bar as its state's prosperity. |

Viability is political support, derived from the economy rather than a Resource of its own: for America, what its households can buy; for China, its industry's employment, which its output stands for since wages are paid per unit. It is a stylized short-horizon criterion, not a claim about either electorate, and it is an objective, not a limit: a state may take a step that breaks its floor, and pays for it in its score (F11). Prosperity and home demand count goods bought, whatever currency paid for them. A state and its firm share two criteria, industry and their households' purchases, so a country's seats gain together at home and contend abroad. Viability and the households' criteria are added by the `private-sector` Premise (§8).

For each state and good, `coverage = min(1, capacity / requirement)` and `exposure = max(0, requirement − capacity)`, where capacity is the most its production can run: the least it holds of what each unit draws, its capacity and its competence. The Dependence view shows them each round with imports, exports, receipts, shortfall, the barriers in force, capacity on order and output left unsold ([07 §9.2](07_simulator.md#92-run-details)). The Finance view shows the bonds, the external accounts and the current account (§4.2). Compare sets out, per run, each seller's sales at home and abroad and its orders blocked, each Actor's receipts valued in the numeraire, final coverage, shortfall, and each private sector's purchases ([07 §10.2](07_simulator.md#102-views)). Net worth values a second currency at the currency Exchange's price, and a good priced in it at its price there.

## 8. How it is built

Pacific is three Premises, each at version `v0`, each a layer on the ones before ([03 §2](03_premise.md#2-premises-and-compositions)). The authored source is `Premises.Pacific`.

| Premise | Adds |
|---|---|
| `bilateral-economy` | The states and firms, both currencies and goods, capacities and competences and their decay, every production Transformation, the requirements, all five Exchanges, the firms' investments, the groups, both routines, every Rule kind, the bond templates, and the criteria that read only these. It is a complete economy on its own, with no wages paid and no one lending. |
| `strategic-dependence` | The export tax (in force), the states' investments, and, by `extends`, each state's authority over its tariff and export control, America's over the asset fee, and its own investment. |
| `private-sector` | The private sectors and the rest of the world's savings, their demand, lending and requirements, the wage laws, the parameters of the tools' levels, and, by `extends`, their access to the Exchanges, each state's viability and each seat's households' criterion. |

The second and third Premises each extend what the first defines, so neither loads alone.

**Dials.** `Premises.Pacific.Dials` holds every number: the prices and the home premium, the currency Exchange, opening money in each currency, how each country's capacity splits between state and firm, the requirements, each investment's goods, construction wages, yield and delay, competence (opening, learning, decay, training), the input share and input per unit, each production's wage, the tariff and tax rates, `Cautious`'s threshold, the private sectors' shares and the rest of the world's (`WorldDials`: bond rates, repression's three settings, the fee, the reserves, world savings), each tool's low and high level, and the objectives' bars.

**Engine features it uses.** Trades settling as pairs, and a dealer Exchange whose price moves with its position ([02 §5](02_kernel.md#5-cost-functions-exchanges-and-solvers)); Rules acting on pairs and Transfers, and a levy paid by the receiver ([03 §5.5](03_premise.md#55-geography-access-and-rules)); Actor groups; an executor's draws on two reservable Resources, and depreciation ([03 §5.3](03_premise.md#53-transformations)); an action law charged on a Transformation's units, and multi-round agreements entered in play by a joint candidate Transaction that Transfers and enters ([03 §15](03_premise.md#15-authoring-laws-and-agreements)); a routine beside a player ([04 §6](04_run.md#6-strategy-initialization)); Aggregates ([02 §7](02_kernel.md#7-aggregates-and-representation)); the `acting` condition, kind-specific "Rules in force", last round's Transformations and the quotient expression ([03 §8](03_premise.md#8-strategy-catalog)); criteria added by `extends`, every-round criteria, and the `bought` and `produced` measures ([03 §10.5](03_premise.md#105-evaluation)); and a composition's default preset ([04 §2](04_run.md#2-instantiation-configuration)).

# Part II: Fixtures

Each scenario plays the composition with the presets named, the states' then the firms', and checks the round-end World and the round's records. Expected values are literals.

| Suite | Holds |
|---|---|
| `packages/handlers/test/Test/Handlers/Pacific.purs` | §9.1–§9.4 and §11 |
| `packages/handlers/test/Test/Handlers/Loops.purs` | §9.5 and §10 |
| `packages/premises/test/Test/Premises/Golden.purs` | §9.6, from `Premises.Pacific.Fixtures` |
| `packages/premises/test/Test/Premises/Compose.purs` | The three Premises and the dials |
| `packages/views/test/Test/Views/Dependence.purs` | The Dependence view and Compare |
| `apps/server/test/Test/Server/Compositions.purs`, `zz-pacific.spec.js` | The composition served, launched and shown |

## 9. Golden scenarios

### 9.1 Trade, barriers and substitution

Firms on the default (all offered on the world Exchange) unless named.

- **A. Free trade.** Every seat `FreeTrade`, six rounds. In round 1 the US firm exports 8 `T`; China buys 3 and its firm 1; the US buys 5 `I` with RMB it holds, China's state sells 4 and its firm 2; the export tax is 2 `USD`. After round 1 the US holds 112.5 `USD`, the US firm 52, China 93.6 `RMB` and the Chinese firm 57.9. Every round, 352 `USD` and 374 `RMB`.
- **B. US export control.** US `Restrict`. From round 2 China is short 3 `T`; 4 orders are `Blocked` in round 2. China's firm, cut off from its input, produces 2, 2, 1, 1, 1, 1 by round.
- **C. China's export control.** China `Restrict`. From round 2 the US is short 5 `I`. The US firm runs only its input-free half once its inventory is used: 8, 8, 4, 4, 4, 4 by round, and China, whose technology came from it, goes short of 0, 0.331, 0.447, 1.7489, 1.8962, 1.9444 `T`. By round 6 China's state holds 20 unsold `I`.
- **D. US substitution.** US `Substitute`. Its industrial capacity is 0, 2, 3.996, 5.992, 5.992, 5.992 by round, but its competence, 0.225, 1.1025, 2.0555, 3.0565, 3.2093, 3.3698, lets it run only 0, 1.2226, 2.2839, 3.3961, 3.5659, 3.7442: what it builds runs only as fast as it learns.
- **E. The importer pays.** US `Tariff`. China still sells 4 `I` in round 2; the US firm, paying the tariff on its input, holds 53.0093 `USD` after round 2.
- **F. Adaptation.** US `Restrict`, China `Adaptive`: China's technology capacity by round is 0, 0, 0, 2, 4, 4. Reversed, the US's industrial capacity is 0, 0, 1.996, 3.992, 5.992, 5.992.
- **H. Invariants.** US `Hawk`, China `Reformer`, firms `Exporter`: every round passes `checkRun`, the replay handler reproduces every round-end World, every reordering of each round's inputs settles to the same World, and each currency's total holds. A control enacted every round is one Rule beside the export tax. Under the US control in round 2, a US gift of 3 `T`, a joint candidate Transaction moving 1 `T` that China signs, and a Chinese routed purchase of 4 `T` at limit 1 all fail: China holds no `T`, is short 3, and at least three `Blocked` records are written.
- **Routine.** A human China seat with nothing to say: its routine sells 4 `I`. When its speech orders one technology investment, the routine sells none; an order of 4 against the maximum of 2 also leaves it selling none.
- **Dependence view.** US `Restrict`. China's technology row after rounds 2 and 6: requirement 3, capacity 0, coverage 0.00, exposure 3, no trade, shortfall 3, one barrier against it. China's industrial row after round 2: capacity 8, coverage 1.00, exports 4, receipts 8.
- **Counterfactual.** US `Restrict` against `FreeTrade`, six rounds: China's technology shortfall over the run is 15 against 3.24; the US firm's receipts 27.56 against 45.73 `USD`, its sales at home 23.56 against 19.68 and abroad 11.91 against 35.67, and 5 of its orders blocked against none.
- **Private demand.** Free trade: China's private sector buys 1.09 `T` abroad in round 3, its requirement and its imports, with no capacity; 6 `T` over six rounds.

### 9.2 Firms, loans and the private sectors

- **I. Loans.** China's firm `Exporter`: repressing (China `StatusQuo`), China lends it 17.6 `RMB` in round 1, a fifth of its RMB, free; rebalanced (`FreeTrade`), 4.4 at 5%. In round 2 China's households deposit 6.195 and 6.281. Either way the firm holds 3 `K_I` in round 2.
- **J. Dividends.** Both firms `Patron`. In round 1 each pays its private sector 7.5, on top of wages; in round 2 the US firm sells 3.856 `T` at home.
- **K. Selling only at home.** Both firms `Builder` (nothing on the world Exchange, high investment): China's firm loses its US input from round 2 and runs only its input-free half, which its investment grows: 2, 1.5, 2, 2.375, 2.875, 3.4687 by round.
- **L. Levies.** US `Tariff`: the tariffs and the export tax are 2, 8.2241, 8.2963, 7.9305, 8.3026, 7.9833 by round, wages apart.
- **M. China saves, America spends.** Both states `Populist`: after round 6 China's private sector holds 11.1816 `RMB` and America's 18.6689 `USD`; over the run China's bought 3.5849 units of goods and America's 12.8052. China's households lend their state most of what they do not spend.
- **N. Who an export control hurts.** After six rounds under its own control China's state holds 20 unsold `I`; after four rounds under America's the US firm holds 11.45 unsold `T` and the US state none.
- **O. Input inventory.** Both firms `Hedger`. Under China's control the US firm produces 8, 8, 6.8572, 4, 4, 4 by round, against 8, 8, 4 without an inventory (C); under the US control China's firm produces 2, 2, 2, 1.4, 1, 1, against 2, 2, 1 (B).

### 9.3 Money between the countries

- **Q. The currency.** Under free trade America's imports sell USD and the dollar falls: 0.995, 0.9867, 0.9889, 0.9863, 0.9768, 0.9626 `RMB` per `USD` by round. Under `StatusQuo` China's reserves buy it back: 1.025, 1.0323, 1.0514, 1.0666, 1.0773, 1.084.
- **R. The asset fee.** US `Rebalance` (the fee high): the rest of the world lends the US state 6.5 `USD` in round 2 against 16.25 without the fee, and sells dollars; the dollar falls to 0.995, 0.8897, 0.8113, 0.7423, 0.6747, 0.6051.

### 9.4 Balance

Over 400 settings of every seat's four tools, drawn deterministically from the 81 each seat has, the seats meet 974, 991, 987 and 994 of their criteria (US, China, US firm, Chinese firm): 2.44, 2.48, 2.47 and 2.49 of 4 a run. In every one of the 400 runs, every external account balances. The bars are set so; a change to any dial is checked against this sample.

### 9.5 Circuits, accounting and loops

- **Circuits.** US transfer low, China repression low and reserves low, both firms selling half at home with low investment and dividends, six rounds: every leg of the four circuits (§4.3) carries a flow, and no currency Flow has an end outside an Actor or the dealer.
- **Bonds.** Under `StatusQuo`, after round 2, every open bond has a lender and a borrower that differ and a positive principal; the templates open are the US bond, China's high deposit and China's high loan, both at 0%.
- **External accounts.** In every preset over eight rounds, every country's account in every currency balances: the residual is zero.
- **Repression.** Under `StatusQuo` over six rounds China's households give up 1.5888 `RMB` against the 5% rate, and its firm saves 5.2504 (§5.5).
- **F1, F2.** Under `Substitute` America's industrial competence grows from 1.1025 after round 2 to 3.3698 after round 6; idle under free trade it wears from 0.225 to 0.1329.
- **F3.** With China's firm opening with no RMB and investing high, it ends with 2.875 `K_I` repressing against 2.7652 rebalanced; with its 50 `RMB`, 9.05 either way.
- **F4.** At the off setting China's firm pays its state 1.2082 `RMB` of interest on the loans it repaid by round 6; at the high setting, none.
- **F5, F6.** The dollar ends round 6 at 0.9626 under free trade, and at 1.084 with China's reserves.
- **F7.** America builds a unit of industrial capacity a round. With China's reserves on, the dollar at 1.0645 leaves it idle, so it makes 1.225 units over the run and its competence ends at 2.229; with the asset fee on, the dollar at 0.5617, it makes 14.2127 and its competence ends at 3.3698.
- **F8.** Firms selling half at home: paying high dividends, America's households buy 20.9296 units at home, all the US firm's home sales, against 8.5109 without dividends.
- **F9.** Under its own tariff America's private sector imports 5.5769 `I` over the run, against 7.352 without.
- **F10.** Under China's export control with America building high, a US that kept 3 units of industrial competence is short 9.008 `I` over the run; one with a quarter of a unit, 14.5315.
- **F11.** `Cautious` China represses in 1 round without a shock (the first, before its firm has produced), and in 4 under a US export control.
- **F12.** Under its own export control the US firm ends round 6 with 53.9454 `USD` and 17.6984 unsold `T`, against 60.6411 and none under free trade.
- **No dead ends.** Over eight rounds of every preset, no Actor's currency runs out, but the US firm's under `Builder`: investing two units a round into a home market too thin to pay for them, it spends its USD by round 8. That is the cost of the strategy, and is kept.

### 9.6 Engine fixtures

Literal worlds cut from Pacific with no production or requirements, run through `resolveRound` alone.

- **P-A.** The US firm sells 4 `T` to China on the world Exchange and pays 2 `USD` of export tax: it ends with 52 `USD` and 4 `T`, China with 2 `USD` and 4 `T`, and the US state with 114 `USD`. The Exchange holds nothing.
- **P-H.** The US firm sells 2 `T` at home to its state at 1.25, untaxed: the firm ends with 52.5 `USD` and 6 `T`, the state with 109.5 `USD` and 4 `T`.
- **P-B.** The US enacts its export control in round 1; in round 2 the same exports move nothing, leaving the firm with 50 `USD` and 8 `T` and China with 6 `USD` and no `T`.
- **P-G.** The Chinese firm offers 2 `I` and nobody buys: it keeps 50 `RMB` and 2 `I`.
- **P-C.** The Chinese firm invests one unit, of 2 `I`: in round 2 it holds 3 `K_I`, and 3.33 `S_I`: its 3, less a tenth, the unit trained, and a tenth off that.
- **P-R.** The US transfers 10 `USD` to its private sector, which opened with 10, and in round 2 buys 2 `T` at home from the US state at 1.25: the state ends with 104.5 `USD` and no `T`, its private sector with 17.5 `USD` and the 2 `T`.
- **P-X.** The currency dealer. The US firm sells 10 `USD` in round 1: the price falls half a hundredth for each, so it averages 0.975 and the firm gets 9.75 `RMB`, holding 40 `USD` and 13.75 `RMB`; the dealer holds 160 and 140.25. In round 2 China buys 10 `USD` from the position the sale left, for the same 9.75 `RMB`: China holds 16 `USD` and 78.25 `RMB`, and the dealer is back at 150 and 150.
- **P-F.** The US enacts its high asset fee in round 1. In round 2 China lends the US 4 `USD` on the US bond and pays 0.2 of fee: China holds 1.8 `USD` and the US 116.2. In round 3 the US repays 4.2: China holds 6 and the US 112.

## 10. Experiments

Each experiment states the conditions under which its feedback appears, and the case where a countervailing force undoes it.

- **Political trap.** States with China's reserves on, firms exporting all, six rounds. China's industrial output (state and firm) is 10, 10, 9.8889, 9.9514, 9.7742, 9.9291 repressing and 10, 10, 9.8889, 9.9494, 9.7718, 9.9235 rebalanced: rebalancing costs it no output, so its viability floor of 9.25 holds either way. Only a US export control breaks it, at 9 from round 3 rebalanced. Its households buy 5.7554 units over the run rebalanced against 5.6023 repressed; wages are 14.8835 against 14.8859. So the political trap does not come from repression alone in Pacific: the floor is at risk when an export control cuts China's firm, and a state that weighs it (F11) represses again then.
- **Dollar trap.** America building one unit of industrial capacity a round, China repressing with reserves. The asset fee takes the dollar from 1.063 to 0.6992 after round 6. US industry, costing nearly what imports do, makes 1.225 units over the run under the strong dollar and 14.2316 under the weak one, and America's households, paid its wages, buy 12.2293 units of goods against 10.0555. Where US industry is cheap (a wage of 1.5), it makes 14.2316 either way: the dollar does not decide.
- **Lost knowledge.** Two US starting positions with 4 units of industrial capacity, one with 6 units of competence and one with a quarter, under China's export control from the start. The skilled one makes 4 a round and is short 1 a round from round 2; the unskilled one makes 0.25, 0.2625, 0.2757, 0.2895, 0.3039, 0.3191, learning a twentieth more a round, and is short 4.75 falling to 4.6961. Dependence is a shortage of capital only where the workforce is still there.
- **Status quo against rebalancing.** Over eight rounds, each country building one unit of its substitute a round and each firm selling half at home, four arms; then each arm again with the other country's export control from round 5.

| Arm | US viability margin | China's | Dollar after round 8 | US owes abroad | US `S_I` after round 4 | China's `S_T` | US short under China's control | China short under America's |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| Both keep their arrangements | 0.4589 | 0.2522 | 1.236 | 77.5913 | 2.7519 | 1.845 | 12.0944 | 2.6929 |
| America rebalances alone | 1.1432 | 0.2522 | 0.838 | 35.3039 | 3.0605 | 1.845 | 3.7876 | 7.5852 |
| China rebalances alone | 0.3609 | 0.2496 | 1.0227 | 37.264 | 2.7519 | 1.845 | 12.0458 | 2.706 |
| Both rebalance | 1.1398 | 0.2243 | 0.6111 | 0.7647 | 3.0605 | 1.71 | 3.7437 | 7.8636 |

The answer: yes. Kept, each arrangement leaves America the more vulnerable: the strong dollar idles its substitute industry, its competence lags, and China's export control costs it 12.09 `I` against 3.79 when it has rebalanced. Its viability margin is lower too, since an idle industry pays no wages. And America's arrangement is what keeps China's vulnerability low: when America rebalances, the weak dollar makes US technology cheap, China's substitute stands idle, and China is short 7.59 `T` under America's control against 2.69. Each country's rebalancing shifts the exposure to the other; neither is less exposed in total. China's own rebalancing changes almost nothing here, since its repression moves little income (§5.5).

## 11. Sensitivity

Each dial changed alone: spend shares of 0.6 (US) and 0.3 (China), a US capacity split of 4 : 6, a Chinese split of 6 : 4, a tax rate of 0.25 or 1, China lending its firm 10% of its RMB at the off setting, and a home premium of 1.5. In every variant China's own export control leaves it holding industrial inputs it could not export, a firm's input inventory keeps it producing more under a control on its input, China's households lend their state more than America's lend theirs, and the US state collects tax on its firm's exports.
