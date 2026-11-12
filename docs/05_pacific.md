# PremiseComposition: Pacific

**Status:** Experimental configuration.  
**PremiseComposition ID:** `pacific`, version `v0`

## 1. The game

Two countries, the United States and China, each make something the other needs. The US makes technology and China makes industrial inputs, and each needs some of both. Trading leaves both better off, and gives each a hold over the other.

Each country is played by two seats: its **state** and its **firm**. Behind them stands the country's **private sector**, which has no seat: it spends what it is given. Every seat balances two dilemmas, one abroad and one at home, and holds four tools, two for each dilemma, each set off, low or high every round.

| Seat | Abroad | At home |
|---|---|---|
| State | **Press or protect.** Raise a trade barrier on the other country's good, a tariff its own importers pay or an import ban, to cut the other side's sales, at the cost of its own supply and of retaliation. Or pursue industrial policy, building its own capacity in place of imports, which costs goods now and pays only later. | **Direct or stimulate.** Keep its Money, and set the terms of its firm's investment, or give households a fiscal stimulus, whose spending absorbs output at home. |
| Firm | **Export or sell at home.** The world Exchange is larger but exposed to the barriers the states decide; the home Exchange pays a premium but is only as large as its private sector's demand. Its production needs a unit of the other country's good each round, which it can hold in inventory against a ban. | **Invest or distribute.** Spend its goods on capital investment in more capacity, or distribute Money to households as wages and dividends, whose spending grows the home market the firm sells into. |

The two countries are built differently, so the same dilemmas weigh differently.

| | China: state-led | United States: market-led |
|---|---|---|
| Capacity | The state holds most of China's industrial capacity (8 of 10). | The firm holds most of America's technology capacity (8 of 10). |
| Exports | Mostly the state's. | Mostly the firm's. |
| State income | Its own export sales, and any tariff. | A tax on US exports, in force from the start, and any tariff, so it lives off what its firm and its importers pay. |
| The firm's investment | Draws on state credit, at the lending rate the state sets. | Free, and the state may subsidise it. |
| Private sector | Saves most of what it holds. | Spends most of what it holds. |

So in China the state's choice between direction and stimulus is a choice about control: whether to let the market expand, and how dearly its firm borrows. In the US it is a choice about distribution: whether the gains the firm earns reach the private sector. A barrier binds the whole country that raises it, its firm too: a US ban on industrial inputs cuts the US firm's own input.

```text
             UNITED STATES                                          CHINA

  ┌─────────────────────────────────┐                    ┌─────────────────────────────────┐
  │ state                           │                    │ state                           │
  │  abroad   press  ◀─▶ protect    │                    │  abroad   press  ◀─▶ protect    │
  │    barrier · industrial policy  │ ◀── US bars I ✕ ── │    barrier · industrial policy  │
  │  at home  direct ◀─▶ stimulate  │ ── CN bars T ✕ ──▶ │  at home  direct ◀─▶ stimulate  │
  │    stimulus · capex subsidy     │                    │    stimulus · lending rate      │
  │                                 │                    │                                 │
  │  holds 2 of 10 T capacity       │ ◀──── I to US ──── │  holds 8 of 10 I capacity       │
  │  lives on taxes and tariffs     │ ──── T to CN ────▶ │  lives on sales and tariffs     │
  └────────────────┬────────────────┘                    └────────────────┬────────────────┘
                   │                                                      │
         subsidy ▼ │  ▲ export tax $                             credit ▼ │  ▲ interest $
         barrier ▼ │  ▲ tariff on input $                       barrier ▼ │  ▲ tariff on input $
                   │                                                      │
  ┌────────────────┴────────────────┐                    ┌────────────────┴────────────────┐
  │ firm                            │                    │ firm                            │
  │  abroad   export ◀─▶ home       │                    │  abroad   export ◀─▶ home       │
  │    export share · inventory I   │ ──── sells T ────▶ │    export share · inventory T   │
  │  at home  invest ◀─▶ distribute │ ◀──── sells I ──── │  at home  invest ◀─▶ distribute │
  │    capex · wages                │                    │    capex on credit · wages      │
  │                                 │                    │                                 │
  │  holds 8 of 10 T capacity       │ ◀──── input I ──── │  holds 2 of 10 I capacity       │
  │  needs 1 I a round              │ ──── input T ────▶ │  needs 1 T a round              │
  └────────────────┬────────────────┘                    └────────────────┬────────────────┘
                   │                                                      │
stimulus and wages ▼                                   stimulus and wages ▼
         private sector                                         private sector
        spends most of it                                      saves most of it
```

Each arrow between the countries is goods, paid for at the Exchange's price. Between the states, China's state sells its industrial inputs to the US and buys its technology, which comes from the US firm. Between the firms, each exports its own good, and each imports the other country's good as the input its production needs. Between each state and its firm run two pairs: the state's support for the firm's investment (the US subsidy, China's credit) against what the firm pays back (the export tax, the interest), and the state's barrier, which binds its own firm, against the tariff the firm pays on its input.

Every outcome is a change in holdings, flows, production or the Rules in force. All values are normalized game units.

## 2. Seats

Each seat faces the same two dilemmas, one abroad and one at home (§1), and holds four tools, each set off, low or high every round, so every seat chooses among 3⁴ = 81 settings a round. The countries differ in where they start, not in what their seats can do.

| Seat | Actor | # | Tool | Off / low / high | What it trades off |
|---|---|---|---|---|---|
| US state | `US` | 1 | Trade barrier on Chinese industrial inputs | none / tariff, paid by the importer / import ban | Tariff income and pressure on China's sales, against its own importers' cost, its own supply and retaliation |
| | | 2 | Industrial policy: build industrial capacity from technology, in place of imports | none / 1 unit / 2 units a round | Technology now, for less dependence on China later |
| | | 3 | Fiscal stimulus to US households | none / 5% / 15% of its Money a round | Its own Money, for home demand for the US firm's technology |
| | | 4 | Investment subsidy to the US firm | none / 1 M / 2 M per unit begun the round before | Its own Money, for the firm's growth |
| China state | `CN` | 1 | Trade barrier on US technology | none / tariff, paid by the importer / import ban | Tariff income and pressure on the US firm's sales, against its own supply and retaliation |
| | | 2 | Industrial policy: build technology capacity from industrial inputs, in place of imports | none / 1 unit / 2 units a round | Industrial inputs now, and the export income they would earn, for less dependence on the US later |
| | | 3 | Fiscal stimulus to Chinese households | none / 5% / 15% of its Money a round | Its own Money, for home demand; households save most of it, so it acts slowly |
| | | 4 | Lending rate on its firm's state credit | none / 1 M / 2 M per unit invested, paid to the state | State income and control, against the firm's growth |
| US firm | `US-firm` | 1 | Export share: technology sold abroad, the rest at home | none / half / all | A larger market abroad, against exposure to China's barrier and the export tax |
| | | 2 | Input inventory: stockpile industrial inputs | none / 1 / 2 units a round | A hedge for its own production against a Chinese ban, against Money tied up and supply its own state cannot buy |
| | | 3 | Capital investment in technology capacity | none / 1 unit / 2 units a round | Capacity and future sales, against goods not sold now |
| | | 4 | Wages and dividends to US households | none / 5% / 15% of its Money a round | Its own Money, for a larger home market |
| China firm | `CN-firm` | 1 | Export share: industrial inputs sold abroad, the rest at home | none / half / all | A larger market abroad, against exposure to the US barrier |
| | | 2 | Input inventory: stockpile technology | none / 1 / 2 units a round | A hedge for its own production against a US ban, against Money tied up and supply its own state cannot buy |
| | | 3 | Capital investment in industrial capacity, on state credit | none / 1 unit / 2 units a round | Capacity and future sales, against the credit's cost and goods not sold now |
| | | 4 | Wages and dividends to Chinese households | none / 5% / 15% of its Money a round | Its own Money, for a home market that grows slowly, since households save |

The tools meet across the seats. A state's barrier cuts the other country's sales and its own firm's input, which is why a firm holds inventory. A state's subsidy or lending rate sets what its firm's investment costs. A state's stimulus and its firm's wages and dividends grow the same home market, which is where the firm sells what it does not export.

The private sectors, `US-private` and `CN-private`, are Aggregates that spend by a fixed rule (§6.2). A state's routine trade and a firm's purchase of its input are automatic (§6.1). Each seat may be played by a human, an AI agent or a preset (§6.3), and is briefed with its description, which names its four tools, and its objective (§7).

## 3. The economy

### 3.1 Resources and holdings

| Resource | ID | Moves | Use |
|---|---|---|---|
| Money | `M` | Freely | The numeraire. Only Transfers, trades, levies and fees move it, so its total is fixed at 300. |
| Technology | `T` | Freely | A good, consumed each round. |
| Industrial inputs | `I` | Freely | A good, consumed each round. |
| Technology capacity | `K_T` | Not at all | Produces `T`. |
| Industrial capacity | `K_I` | Not at all | Produces `I`. |
| Chinese credit | `CN-credit` | Not at all | Held by the Chinese state; each unit of the Chinese firm's investment draws on one. |

| Opening holding | US | US firm | CN | CN firm | Each private sector |
|---|---:|---:|---:|---:|---:|
| `M` | 120 | 50 | 80 | 50 | 0 |
| `T` | 2 | 8 | 0 | 0 | 0 |
| `I` | 0 | 0 | 8 | 2 | 0 |
| `K_T` | 2 | 8 | 0 | | |
| `K_I` | 0 | | 8 | 2 | |
| `CN-credit` | | | 2 | | |

The goods are one production cycle's output, available in round 1. The private sectors start empty. The US starts with more Money than China because its state spends on imports and China's state earns from exports; the split is a dial, set so neither state is favoured (§9.4).

### 3.2 Production and requirements

- **Production.** Each unit of capacity produces one unit of its good each round, delivered the next round.
- **States' requirements.** Each round the US consumes 6 `T` and 5 `I`, and China 3 `T` and 4 `I`, after the round's trades. What cannot be met is recorded as a shortfall.
- **Firms' inputs.** Each round the US firm consumes 1 `I` and the Chinese firm 1 `T`. What cannot be met is recorded as a shortfall.
- **Private sectors' requirements.** A private sector consumes whatever it buys.

At the start the US firm's 8 `T` meet the US state's 4 missing `T`, China's 3 and the Chinese firm's 1; China's state exports 4 `I` and its firm 2, meeting the US's 5 and the US firm's 1. With free trade every requirement is met.

### 3.3 Exchanges

| Exchange | Good | Open to | Price |
|---|---|---|---:|
| `technology-world` | `T` | every Actor | 1 |
| `industry-world` | `I` | every Actor | 2 |
| `technology-us` | `T` | the US, its firm and its private sector | 1.25 |
| `industry-cn` | `I` | China, its firm and its private sector | 2.5 |

A home Exchange pays its sellers a premium of a quarter (a dial). No Exchange holds reserves, so every trade is a pair between a buyer and a seller ([02 §5.2](02_kernel.md#52-exchanges)): a seller is paid only for what a buyer takes, the good and the Money move together or not at all, and when one side is longer it is cut back pro rata. Each firm and each private sector counts as its country for Rules, so a trade at home is never foreign.

## 4. A round

Each round follows the canonical sequence ([02 §8.1](02_kernel.md#81-canonical-resolution-sequence)):

1. **Prepare.** Rule changes enacted last round take effect, new capacity arrives, and last round's production is delivered.
2. **Decide.** Each seat sets its tools from its View (§2).
3. **Routines.** Each state's commerce routine drafts its trades, each firm's supply routine buys its input, and each private sector drafts its purchases.
4. **Resolve.** Orders are funded from opening holdings. The Exchanges clear; bans refuse their pairs and levies are charged on theirs; investments use their inputs and pay their fees; requirements are consumed.
5. **Commit.** The round-end World is stored. Money received this round, from sales, a stimulus, wages or a subsidy, is spendable from the next.

## 5. The levers

### 5.1 Barriers and the export tax

| Rule kind | Governor | Effect |
|---|---|---|
| `us-import-tariff` | US | Charges 1 `M` (a dial) for each unit of `I` coming into the US from China, paid by the importer to the US. |
| `us-import-ban` | US | Blocks `I` coming into the US from China. |
| `cn-import-tariff` | China | Charges 1 `M` (the same dial) for each unit of `T` coming into China from the US, paid by the importer to China. |
| `cn-import-ban` | China | Blocks `T` coming into China from the US. |
| `us-export-tax` | US | Charges 0.5 `M` (a dial) for each unit of `T` sold from the US to China, paid by the seller to the US. In force from the start. |

A Rule takes effect at the next round boundary and stays in force until repealed; enacting one already in force adds nothing. A ban refuses each pair between the two countries' orders: neither the good nor the Money moves, the seller keeps its goods, and both orders are recorded as `Blocked`. It refuses a gift Transfer or a joint candidate Transaction between them in the same way, and a routed order pairs like any other, so a ban holds however a trade is arranged ([03 §5.5](03_premise.md#55-geography-access-and-rules)). A levy is charged once per pair, on the units moved ([03 §5.5](03_premise.md#55-geography-access-and-rules)). A tariff is paid by the importer, so a state that imports pays its own tariff to itself, and its firm and private sector pay it to the state; the private sector, paying the price and the tariff, buys less of the foreign good (§6.2), and that is how a tariff reaches the other country's sales. The export tax is due on the state's own exports too, which pay it to itself, so in effect the firm pays.

### 5.2 Investment

| Transformation | Executor | Uses | Creates | Capacity arrives |
|---|---|---|---|---|
| `us-industrial-investment` | US | 2 `T` | +2 `K_I` | the next round |
| `cn-technology-investment` | CN | 4 `I` | +2 `K_T` | two rounds later |
| `us-firm-investment` | US firm | 2 `T` | +1 `K_T` | the next round |
| `cn-firm-investment` | CN firm | 2 `I` and a draw on 1 `CN-credit` | +1 `K_I` | the next round |
| `cn-firm-investment-low` | CN firm | the same, and 1 `M` (a dial) to China | +1 `K_I` | the next round |
| `cn-firm-investment-high` | CN firm | the same, and 2 `M` (a dial) to China | +1 `K_I` | the next round |

At most two units per Actor per round. Capacity produces from the round after it arrives. The Chinese firm's investment draws on the state's credit, so each order is a joint candidate Transaction the Chinese state must sign. The firm offers its investment at all three rates, and the state signs only the one at the lending rate it has set, so the rate is the price of credit and the others lapse.

### 5.3 Stimulus, wages and the subsidy

A state's fiscal stimulus to its own private sector, and a firm's wages and dividends to its own, are ordinary Transfers of a share of its Money. The Money is the private sector's to spend from the next round. The US subsidy is a Transfer from the US state to its firm, 1 or 2 `M` (dials) for each unit of investment the firm began the round before.

## 6. Strategies

### 6.1 Routines

`pacific-commerce-v0` is each state's routine ([04 §6](04_run.md#6-strategy-initialization)). It runs after the state's player and reads what the player put forward. For each good it sells `max(0, holding − requirement − planned inputs)` and buys `max(0, requirement − holding)`, on the world Exchange at its price. It reads each state's own requirement (`Due`) and planned investment (`Planned`), so one routine serves both.

`pacific-supply-v0` is each firm's routine: it buys `max(0, input requirement − holding)` of its foreign input on the world Exchange, so an inventory is used before anything is bought.

### 6.2 Private demand

Each private sector buys its own country's good at home and the other good on the world Exchange. Of each it buys its Money times half its spend share, divided by the price; under its own country's tariff, it divides by the price and the tariff for the foreign good. The spend share is America's 0.8 and China's 0.2 (dials).

### 6.3 Presets

A preset is the policy every seat plays in a simulated run, written as each tool's level (`Premises.Pacific.Strategies`: `statePolicy`, `firmPolicy`). A state preset sets both states' tools, with each firm exporting all it holds and its other tools off; a firm preset sets both firms' tools, with each state's tools off. The default is `FreeTrade`.

| Preset | Seat | Trade barrier / export share | Industrial policy / input inventory | Stimulus / capital investment | Subsidy or lending rate / wages and dividends |
|---|---|---|---|---|---|
| `FreeTrade` | state | none | none | none | none |
| `Tariff` | state | tariff | none | none | none |
| `Restrict` | state | ban | none | none | none |
| `Substitute` | state | none | 1 unit | none | none |
| `Rebalance` | state | none | none | 15% | none |
| `Hawk` | state | ban | none | none | high |
| `Populist` | state | tariff | none | 15% | none |
| `Planner` | state | none | 2 units | none | high |
| `Reformer` | state | none | 1 unit | 5% | none |
| `Exporter` | firm | all | none | 1 unit | none |
| `Merchant` | firm | all | none | none | 15% |
| `Builder` | firm | none | none | 2 units | none |
| `Patron` | firm | none | none | none | 15% |
| `Hedger` | firm | all | 2 units | none | none |

Two state presets react to the other side: `Adaptive` builds one unit of capacity while the other country keeps a barrier against it, and `Retaliate` bans the other's good the round after the other raised a barrier. No preset repeals, since a repeal names the Rule's id; a human or agent seat can.

## 7. Objectives and reporting

Each seat has four criteria, evaluated from the recorded run ([03 §10.5](03_premise.md#105-evaluation)). A seat's score is the number it meets, from 0 to 4. The bars are dials.

| Seat | Criteria |
|---|---|
| Each state | **Security:** its shortfall of the good it imports is at most 14 over the run. **Power:** it ends with at least the other state's Money. **Industry:** its firm ends with at least 9 `K_T` (US) or 3 `K_I` (China). **Prosperity:** its private sector ends with at least 10 `M` (US) or 20 `M` (China). |
| Each firm | **Wealth:** it ends with at least 50 `M` (US) or 30 `M` (China). **Growth:** the same capacity bar as its state's industry. **Supply:** its shortfall of its input is at most 3 (US) or 5 (China) over the run. **Home demand:** its private sector ends with at least 10 `M` (US) or 20 `M` (China). |

A state and its firm share two criteria, industry and the private sector's Money, so a country's seats gain together at home and contend abroad. The private-sector criteria are added by the `private-sector` Premise (§8).

For each state and good, `coverage = min(1, capacity / requirement)` and `exposure = max(0, requirement − capacity)`. The run's Dependence view shows them each round with imports, exports, receipts, shortfall, the bans in force, capacity on order and output left unsold, and a row for each private sector's purchases ([07 §9.2](07_simulator.md#92-run-details)). Compare sets out, per run, each seller's sales at home and abroad and its orders blocked by Rules, each Actor's receipts, final coverage, shortfall, and each private sector's purchases ([07 §10.2](07_simulator.md#102-views)). Comparing a run with the same run without a ban gives the ban's realized effect.

## 8. How it is built

Pacific is three Premises, each at version `v0`, each a layer on the ones before ([03 §2](03_premise.md#2-premises-and-compositions)). The authored source is `Premises.Pacific`.

| Premise | Adds |
|---|---|
| `bilateral-economy` | The states and firms, the goods, capacities and credit, production and requirements (the firms' inputs among them), all four Exchanges, the firms' investments at each lending rate, the groups, both routines, and the criteria that read only these. It is a complete economy on its own. |
| `strategic-dependence` | The tariffs, the bans and the export tax (in force), the states' investments, and, by `extends`, each state's authority over its own. |
| `private-sector` | The private sectors, their demand and requirements, the parameters of the tools' levels, and, by `extends`, their access to the Exchanges and each seat's private-sector criterion. |

The second and third Premises each extend what the first defines, so neither loads alone.

**Dials.** `Premises.Pacific.Dials` holds every number: the prices and the home premium, opening Money, how each country's capacity splits between state and firm, the requirements and the firms' inputs, each investment, Chinese credit and its fees, the subsidy, the tariff and tax rates, the spend shares, each tool's low and high level, the firms' Money, and the objectives' bars.

**Engine features it uses.** Trades settling as pairs ([02 §5.5](02_kernel.md#55-simultaneous-clearing)), Rules acting on pairs and a levy paid by the receiver ([03 §5.5](03_premise.md#55-geography-access-and-rules)), Actor groups, an owner-signed draw and a Transformation's payment ([03 §5.3](03_premise.md#53-transformations)), a routine beside a player ([04 §6](04_run.md#6-strategy-initialization)), Aggregates ([02 §7](02_kernel.md#7-aggregates-and-representation)), the `acting` condition, kind-specific "Rules in force" and last round's Transformations ([03 §8](03_premise.md#8-strategy-catalog)), criteria added by `extends` ([03 §2](03_premise.md#2-premises-and-compositions)), and a composition's default preset ([04 §2](04_run.md#2-instantiation-configuration)).

# Part II: Fixtures

Each scenario plays the composition with the presets named, the states' then the firms', and checks the round-end World and the round's records. Expected values are literals.

| Suite | Holds |
|---|---|
| `packages/handlers/test/Test/Handlers/Pacific.purs` | §9.1, §9.2, §9.4 and §10 |
| `packages/premises/test/Test/Premises/Golden.purs` | §9.3, from `Premises.Pacific.Fixtures` |
| `packages/premises/test/Test/Premises/Compose.purs` | The three Premises and the dials |
| `packages/views/test/Test/Views/Dependence.purs` | The Dependence view and Compare |
| `apps/server/test/Test/Server/Compositions.purs`, `zz-pacific.spec.js` | The composition served, launched and shown |

## 9. Golden scenarios

### 9.1 Trade, barriers and substitution

Firms on the default (export all) unless named.

- **A. Free trade.** Every seat `FreeTrade`, six rounds. In round 1 the US firm exports 8 `T`; China buys 3 and the US 5 `I`; China's state sells 4 `I` and its firm 2. After round 1 the US holds 108 `M`, China 85, the US firm 54 and the Chinese firm 53; after round 6 the US holds 48. The export tax is 2 `M` every round. No shortfall, and all Money totals 300.
- **B. US import ban.** US `Restrict`. From round 2 the US is short 5 `I` and its firm 1; China sells no `I` but still buys 3 `T`; 4 orders are `Blocked` a round. By round 6 China's state holds 20 unsold `I` and its firm 10.
- **C. Chinese import ban.** China `Restrict`. From round 2 China is short 3 `T` and its firm 1; the US firm sells 4 `T`, to its own state, and the US still imports 5 `I`. By round 6 the US firm holds 20 unsold `T`.
- **D. US substitution.** US `Substitute`. Investing 2 `T` leaves the US state 2 `T` short in round 1. It holds 4 `K_I` and imports 3 `I` in round 3, and 6 `K_I` and imports 1 in round 4.
- **E. The importer pays.** US `Tariff`. China still sells 4 `I` in round 2; the US firm, paying 1 `M` of tariff on its input, holds 57 `M` after round 2, and China 90, as under free trade.
- **F. Adaptation.** US `Restrict`, China `Adaptive`: China's technology capacity by round is 0, 0, 0, 2, 4, 4. Reversed, the US's industrial capacity is 0, 0, 2, 4, 6, 6.
- **H. Invariants.** US `Hawk`, China `Reformer`, firms `Exporter`: every round passes `checkRun`, the replay handler reproduces every round-end World, every reordering of each round's inputs settles to the same World, and all Money totals 300. A ban enacted every round is one Rule beside the export tax. Under China's ban in round 2, a US gift of 3 `T`, a joint candidate Transaction moving 1 `T` that China signs, and a Chinese routed purchase of 4 `T` at limit 1 all fail: China holds no `T`, is short 3, and at least three `Blocked` records are written.
- **Routine.** A human China seat with nothing to say: its routine sells 4 `I`. When its speech orders one technology investment, the routine sells none; an order of 4 against the maximum of 2 also leaves it selling none.
- **Dependence view.** China `Restrict`. China's technology row after round 2: requirement 3, capacity 0, coverage 0.00, exposure 3, no trade, shortfall 3; the same after round 6. China's industrial row after round 2: capacity 8, coverage 1.00, exports 4, receipts 8.
- **Counterfactual.** China `Restrict` against `FreeTrade`, six rounds: China's technology shortfall over the run is 15 against 0; the US firm's receipts 28 against 48, its sales at home 24 either way and abroad 4 against 24, and 5 of its orders blocked against none.
- **Private demand.** China `Rebalance`: China's private sector buys 1.04 `T` abroad in round 2, its requirement and its imports, with no capacity; 10.35 `T` over six rounds.

### 9.2 Firms, credit and the private sectors

- **I. Credit.** China's firm `Exporter`: at the high lending rate (China `Planner`) it pays 2 `M` for its unit and holds 48 `M` after round 1, against 50 at no rate (China `Reformer`); either way it holds 3 `K_I` in round 2.
- **P. Subsidy.** US `Hawk` (high subsidy) against `Restrict` (none), US firm `Exporter`: after round 2 the subsidised firm holds 2 `M` more.
- **J. Wages and dividends.** Both firms `Patron`. After round 1 each private sector holds 7.5 `M`; in round 2 the US firm sells 3 `T` at home.
- **K. Accumulating into a weak market.** Both firms `Builder`: China's firm holds 8 `K_I` after round 6, and the US firm, with little demand at home, holds over 40 unsold `T`.
- **L. Levies.** US `Tariff`: levies are 2, 8, 8, 8, 8, 8 `M` by round, the export tax alone and then with the tariff on the US's 5 imports and its firm's 1.
- **M. China saves, America spends.** Both states `Rebalance` (high stimulus): after round 6 China's private sector holds 49.5238 `M` and America's 31.0646.
- **N. Who a ban hurts.** After six rounds under a US ban China's state holds 20 unsold `I` and its firm 10; under a Chinese ban the US firm holds 20 unsold `T` and the US state none.
- **O. Input inventory.** Both firms `Hedger`. Under a US ban the US firm, whose input it cuts, is short 0, 0, 0.75, 1, 1, 1 `I` by round, against 0, 1, 1, 1, 1, 1 without an inventory; under a Chinese ban China's firm is short 0, 0, 0.6, 1, 1, 1 `T`.

### 9.3 Engine fixtures

Literal worlds cut from Pacific with no production or requirements, run through `resolveRound` alone.

- **P-A.** The US firm sells 4 `T` to China on the world Exchange and pays 2 `M` of export tax: it ends with 52 `M` and 4 `T`, China with 76 `M` and 4 `T`, and the US state with 122 `M`. The Exchange holds nothing.
- **P-H.** The US firm sells 2 `T` at home to its state at 1.25, untaxed: the firm ends with 52.5 `M` and 6 `T`, the state with 117.5 `M` and 4 `T`.
- **P-B.** China enacts its ban on US technology in round 1; in round 2 the same exports move nothing, leaving the firm with 50 `M` and 8 `T` and China with 80 `M` and no `T`.
- **P-G.** The Chinese firm offers 2 `I` and nobody buys: it keeps 50 `M` and 2 `I`.
- **P-C.** The Chinese firm orders an investment that China signs: in round 2 it holds no `I` and 3 `K_I`.
- **P-R.** China gives its private sector a stimulus of 10 `M`, which in round 2 buys 2 `I` at home from China at 2.5: China ends with 75 `M` and 6 `I`, its private sector with 5 `M` and 2 `I`.

### 9.4 Balance

Over 400 settings of every seat's four tools, drawn deterministically from the 81 each seat has, the seats meet 980, 1011, 1002 and 1018 of their criteria (US, China, US firm, Chinese firm): 2.45, 2.53, 2.51 and 2.55 of 4 a run. The opening Money, the bars and the firms' inputs are set so; a change to any of them is checked against this sample.

## 10. Sensitivity

Each dial changed alone: spend shares of 0.6 (US) and 0.3 (China), a US capacity split of 4 : 6, a Chinese split of 6 : 4, a tax rate of 0.25 or 1, 4 units of credit, and a home premium of 1.5. In every variant a US ban leaves China holding industrial inputs it could not export, a firm's input inventory shortens its shortfall under a ban on its input, given the same stimulus China's private sector saves more than America's, and the US state collects tax on its firm's exports.
