# PremiseComposition: Pacific

**Status:** Experimental configuration.  
**PremiseComposition ID:** `pacific`, version `v0`

## 1. The game

Two countries, the United States and China, each make something the other needs. The US makes technology and China makes industrial inputs, and each needs some of both. Trading leaves both better off, and gives each a hold over the other.

Each country is played by two seats: its **state** and its **firm**. Behind them stands the country's **private sector**, which has no seat: it spends what it is given. Every seat balances two dilemmas, one abroad and one at home.

| Seat | Abroad | At home |
|---|---|---|
| State | **Press or protect.** Ban its exports, or for the US tax China's, to hurt the other side, at the cost of its own sales and of retaliation. Or invest in a substitute, which costs goods now and pays only later. | **Direct or release.** Keep what it holds for strategic production, or release it to its private sector, whose demand absorbs output at home. |
| Firm | **Export or sell at home.** The world Exchange is larger but exposed to bans and levies the states decide; the home Exchange pays a premium but is only as large as its private sector's demand. | **Accumulate or spend.** Reinvest its goods in more capacity, or pay its private sector, whose demand grows the home market the firm sells into. |

The two countries are built differently, so the same dilemmas weigh differently.

| | China: state-led | United States: market-led |
|---|---|---|
| Capacity | The state holds most of China's industrial capacity (8 of 10). | The firm holds most of America's technology capacity (8 of 10). |
| Exports | Mostly the state's. | Mostly the firm's. |
| State income | Its own export sales. | A tax on US exports and the tariff, so it lives off what its firm earns. |
| Firm autonomy | The firm invests only on state credit, which the state may refuse. | The firm invests freely. |
| Private sector | Saves most of what it holds. | Spends most of what it holds. |

So in China the state's choice between direction and release is a choice about control: whether to let the market expand. In the US it is a choice about distribution: whether the gains the firm earns reach the private sector. Each country's ban binds its own firm, so a US ban falls mostly on the US firm, and a Chinese ban mostly on the Chinese state.

```text
                     ABROAD                                      AT HOME

   ┌──────────┐                       ┌──────────┐        ┌──────────────────────┐
   │ US state │  ── ban T, tax I ──▶  │ CN state │        │ state                │
   │          │  ◀──── ban I ─────    │          │        │   direct ─▶ capacity │
   └──────────┘    invest to need     └──────────┘        │   release ─▶ private │
     protect        less of the        protect            │                sector│
                    other's good                          └──────────┬───────────┘
                                                                     │ demand
   ┌──────────┐   world Exchange      ┌──────────┐        ┌──────────▼───────────┐
   │ US firm  │ ◀──── larger, ────▶   │ CN firm  │        │ firm                 │
   │          │       exposed         │          │        │   accumulate ─▶ more │
   └────┬─────┘                       └────┬─────┘        │      capacity        │
        │ home Exchange:                   │ home         │   spend ─▶ private   │
        ▼ premium, as large as             ▼              │      sector ─▶ home  │
     US private sector              CN private sector     │      demand          │
                                                          └──────────────────────┘
```

Every outcome is a change in holdings, flows, production or the Rules in force. All values are normalized game units.

## 2. Seats

| Seat | Actor | Decides each round |
|---|---|---|
| US state | `US` | Whether to ban technology exports to China, levy Chinese industrial inputs, invest technology in industrial capacity, and release Money to the US private sector. |
| China state | `CN` | Whether to ban industrial-input exports to the US, invest industrial inputs in technology capacity, release Money to China's private sector, and sign its firm's credit. |
| US firm | `US-firm` | Whether to sell technology abroad or at home, and whether to invest it in capacity or pay the US private sector. |
| China firm | `CN-firm` | Whether to sell industrial inputs abroad or at home, and whether to invest them in capacity (on credit) or pay China's private sector. |

The private sectors, `US-private` and `CN-private`, are Aggregates that spend by a fixed rule (§6.2). A state's routine trade is automatic (§6.1). Each seat may be played by a human, an AI agent or a preset (§6.3), and is briefed with its description, which names its two dilemmas, and its objective (§7).

## 3. The economy

### 3.1 Resources and holdings

| Resource | ID | Moves | Use |
|---|---|---|---|
| Money | `M` | Freely | The numeraire. Only Transfers, trades and levies move it, so its total is fixed. |
| Technology | `T` | Freely | A good, consumed each round. |
| Industrial inputs | `I` | Freely | A good, consumed each round. |
| Technology capacity | `K_T` | Not at all | Produces `T`. |
| Industrial capacity | `K_I` | Not at all | Produces `I`. |
| Chinese credit | `CN-credit` | Not at all | Held by the Chinese state; the Chinese firm's investment draws on it. |

| Opening holding | US | US firm | CN | CN firm | Each private sector |
|---|---:|---:|---:|---:|---:|
| `M` | 100 | 50 | 100 | 50 | 0 |
| `T` | 2 | 8 | 0 | 0 | 0 |
| `I` | 0 | 0 | 8 | 2 | 0 |
| `K_T` | 2 | 8 | 0 | | |
| `K_I` | 0 | | 8 | 2 | |
| `CN-credit` | | | 1 | | |

The goods are one production cycle's output, available in round 1. The private sectors start empty.

### 3.2 Production and requirements

- **Production.** Each unit of capacity produces one unit of its good each round, delivered the next round.
- **States' requirements.** Each round the US consumes 6 `T` and 6 `I`, and China 4 `T` and 4 `I`, after the round's trades. What cannot be met is recorded as a shortfall.
- **Private sectors' requirements.** A private sector consumes whatever it buys.

At the start the US firm's 8 `T` meet the US state's 4 missing `T` and China's 4; China's state exports 4 `I` and its firm 2, meeting the US's 6. With free trade every requirement is met.

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
2. **Decide.** Each seat chooses from its View (§2).
3. **Routines.** Each state's commerce routine drafts its trades; each private sector drafts its purchases.
4. **Resolve.** Orders are funded from opening holdings. The Exchanges clear; bans refuse their pairs and levies are charged on theirs; investments use their inputs; requirements are consumed.
5. **Commit.** The round-end World is stored. Money received this round, from sales, a release or pay, is spendable from the next.

## 5. The levers

### 5.1 Bans and levies

| Rule kind | Governor | Effect |
|---|---|---|
| `us-technology-ban` | US | Blocks `T` from the US to China. |
| `cn-industrial-ban` | China | Blocks `I` from China to the US. |
| `us-import-tariff` | US | Charges China 1 `M` (a dial) for each unit of `I` it sells to the US, paid to the US. |
| `us-export-tax` | US | Charges 0.5 `M` (a dial) for each unit of `T` sold from the US to China, paid to the US. In force from the start. |

A Rule takes effect at the next round boundary and stays in force until repealed; enacting one already in force adds nothing. A ban refuses each pair between the two countries' orders: neither the good nor the Money moves, the seller keeps its goods, and both orders are recorded as `Blocked`. It refuses a gift Transfer or a joint candidate Transaction between them in the same way, and a routed order pairs like any other, so a ban holds however a trade is arranged ([03 §5.5](03_premise.md#55-geography-access-and-rules)). A levy is charged once per pair, on the units moved, to the seller. The export tax is due on the state's own exports too, which pay it to itself, so in effect the firm pays.

### 5.2 Investment

| Transformation | Executor | Uses | Creates | Capacity arrives |
|---|---|---|---|---|
| `us-industrial-investment` | US | 2 `T` | +2 `K_I` | the next round |
| `cn-technology-investment` | CN | 4 `I` | +2 `K_T` | two rounds later |
| `us-firm-investment` | US firm | 2 `T` | +1 `K_T` | the next round |
| `cn-firm-investment` | CN firm | 2 `I` and a draw on 1 `CN-credit` | +1 `K_I` | the next round |

At most one unit per Actor per round. Capacity produces from the round after it arrives. The Chinese firm's investment draws on the state's credit, so each order is a joint candidate Transaction the Chinese state must sign; unsigned, it lapses.

### 5.3 Release and pay

A state releases Money to its own private sector, and a firm pays its own, by an ordinary Transfer. The Money is the private sector's to spend from the next round.

## 6. Strategies

### 6.1 Commerce

`pacific-commerce-v0` is each state's routine ([04 §6](04_run.md#6-strategy-initialization)). It runs after the state's player and reads what the player put forward. For each good it sells `max(0, holding − requirement − planned inputs)` and buys `max(0, requirement − holding)`, on the world Exchange at its price. It reads each state's own requirement (`Due`) and planned investment (`Planned`), so one routine serves both.

### 6.2 Private demand

Each private sector buys its own country's good at home and the other good on the world Exchange. Of each it buys its Money times half its spend share, divided by the price. The spend share is America's 0.8 and China's 0.2 (dials).

### 6.3 Presets

A preset is the policy every seat plays in a simulated run. A state preset gives the states its policy and each firm the default of exporting all it holds beyond its investment; a firm preset gives the firms its policy and each state nothing strategic. A clause another Actor has no authority for drafts nothing, and a clause any seat could draft names its seat with `acting` ([03 §8](03_premise.md#8-strategy-catalog)). The default is `FreeTrade`.

| Preset | Seat | Abroad | At home | Signs China's credit |
|---|---|---|---|---|
| `Hawk` | state | Press: ban, and for the US the tariff | Direct | No |
| `Populist` | state | Press | Release a tenth (a dial) of its Money a round | Yes |
| `Planner` | state | Protect: invest while capacity, held and on order, is below its requirement | Direct | No |
| `Reformer` | state | Protect | Release | Yes |
| `Exporter` | firm | Export | Accumulate | |
| `Merchant` | firm | Export | Spend half its Money a round (a dial) | |
| `Builder` | firm | Sell at home | Accumulate | |
| `Patron` | firm | Sell at home | Spend | |

The single-lever state presets remain for experiments: `FreeTrade` (nothing strategic), `Restrict`, `Substitute`, `RestrictAndSubstitute`, `Adaptive` (substitute only while the other's ban is in force), `Directed` (invest one unit every round), `Rebalance`, `Balanced` (substitute and release half as much), `Tariff` and `Retaliate` (China bans the round after the US restricts it). Each signs China's credit. No preset repeals, since a repeal names the Rule's id; a human or agent seat can.

## 7. Objectives and reporting

Objectives are evaluated from the recorded run ([03 §10.5](03_premise.md#105-evaluation)):

| Seat | Objective | Criteria |
|---|---|---|
| US state | Meet domestic needs and reduce dependence on Chinese industrial inputs, while the gains reach the US private sector | No shortfall of `T` or `I` over the run; ends with `K_I` at least 6; its private sector ends with at least 1 `M`. |
| China state | Meet domestic needs, reduce dependence on US technology and keep industrial export income, while keeping control of what China produces | No shortfall of `T` or `I` over the run; ends with `K_T` at least 4; its private sector ends with at least 1 `M`. |
| Each firm | Grow a business that earns, without losing it to foreign Rules | Ends with at least 10 `M` and at least its opening capacity. |

For each state and good, `coverage = min(1, capacity / requirement)` and `exposure = max(0, requirement − capacity)`. The run's Dependence view shows them each round with imports, exports, receipts, shortfall, the bans in force, capacity on order and output left unsold, and a row for each private sector's purchases ([07 §9.2](07_simulator.md#92-run-details)). Compare sets out, per run, each seller's sales at home and abroad and its orders blocked by Rules, each Actor's receipts, final coverage, shortfall, and each private sector's purchases ([07 §10.2](07_simulator.md#102-views)). Comparing a run with the same run without a ban gives the ban's realized effect.

## 8. How it is built

Pacific is three Premises, each at version `v0`, each a layer on the ones before ([03 §2](03_premise.md#2-premises-and-compositions)). The authored source is `Premises.Pacific`.

| Premise | Adds |
|---|---|
| `bilateral-economy` | The states and firms, the goods, capacities and credit, production and requirements, all four Exchanges, the firms' investments, the groups, and the commerce routine. It is a complete economy on its own. |
| `strategic-dependence` | The bans, the tariff and the export tax (in force), the states' investments, and, by `extends`, each state's authority over its own. |
| `private-sector` | The private sectors, their demand and requirements, the release and pay shares, and, by `extends`, their access to the Exchanges. |

The second and third Premises each extend what the first defines, so neither loads alone.

**Dials.** `Premises.Pacific.Dials` holds every number: the prices and the home premium, opening Money, how each country's capacity splits between state and firm, the requirements, each investment, Chinese credit, the tariff and tax rates, the spend shares, the release and pay shares, and the firms' Money.

**Engine features it uses.** Trades settling as pairs ([02 §5.5](02_kernel.md#55-simultaneous-clearing)), Rules acting on pairs ([03 §5.5](03_premise.md#55-geography-access-and-rules)), Actor groups, an owner-signed draw ([03 §5.3](03_premise.md#53-transformations)), a routine beside a player ([04 §6](04_run.md#6-strategy-initialization)), Aggregates ([02 §7](02_kernel.md#7-aggregates-and-representation)), the `acting` condition and kind-specific "Rules in force" ([03 §8](03_premise.md#8-strategy-catalog)), and a composition's default preset ([04 §2](04_run.md#2-instantiation-configuration)).

# Part II: Fixtures

Each scenario plays the composition with the presets named, the states' then the firms', and checks the round-end World and the round's records. Expected values are literals.

| Suite | Holds |
|---|---|
| `packages/handlers/test/Test/Handlers/Pacific.purs` | §9.1, §9.2 and §10 |
| `packages/premises/test/Test/Premises/Golden.purs` | §9.3, from `Premises.Pacific.Fixtures` |
| `packages/premises/test/Test/Premises/Compose.purs` | The three Premises and the dials |
| `packages/views/test/Test/Views/Dependence.purs` | The Dependence view and Compare |
| `apps/server/test/Test/Server/Compositions.purs`, `zz-pacific.spec.js` | The composition served, launched and shown |

## 9. Golden scenarios

### 9.1 Trade, bans and substitution

Firms on the default (export) unless named.

- **A. Free trade.** Every seat `FreeTrade`, six rounds. Each round the US firm exports 8 `T`, 4 to its own state and 4 to China; China's state sells 4 `I` and its firm 2. After round 1 the US holds 86 `M`, its firm 56, China 104 and its firm 54; after round 6 the US holds 16. The export tax is 2 `M` every round. No shortfall, and all Money totals 300.
- **B. US technology ban.** US `Restrict`. From round 2 China is short 4 `T`; the US firm, still selling to its own state, sells 4 and holds 4 unsold, 20 by round 6; China's 4 `I` still cross; a `Blocked` record is written.
- **C. Chinese industrial ban.** China `Restrict`. It binds China's firm too: from round 2 the US is short 6 `I` and China's firm sells nothing, while 4 `T` still reach China. By round 6 China holds 20 unsold `I` and its firm 10.
- **D. US substitution.** US `Substitute`. Investing 2 `T` leaves the US state 2 `T` short in round 1. It holds 4 `K_I` and imports 4 `I` in round 3, and 6 `K_I` and imports nothing in round 6.
- **F. Ban and adaptation.** US `Restrict`, China `Adaptive`: China's technology shortfall by round is 0, 4, 4, 4, 2, 0. Reversed, the US industrial shortfall is 0, 6, 6, 4, 2, 0.
- **G. No phantom trade.** US `Substitute`: in round 6 China holds 12 unsold `I` and its firm 6, and China is paid nothing for `I`.
- **H. Invariants.** US `Hawk`, China `Reformer`, firms `Exporter`: every round passes `checkRun`, the replay handler reproduces every round-end World, every reordering of each round's inputs settles to the same World, and all Money totals 300. A ban enacted every round is one Rule beside the export tax. Under a US ban in round 2, a US gift of 3 `T`, a joint candidate Transaction moving 1 `T` that China signs, and a Chinese routed purchase of 4 `T` at limit 1 all fail: China holds no `T`, is short 4, and at least three `Blocked` records are written.
- **Routine.** A human China seat with nothing to say: its routine sells 4 `I`. When its speech orders one technology investment, the routine sells none; an order of 4 against the maximum of 1 also leaves it selling none.
- **Dependence view.** US `Restrict`, China `Adaptive`. China's technology row after round 2: requirement 4, capacity 0, coverage 0.00, exposure 4, no trade, shortfall 4, one ban against it, 2 capacity on order. After round 6: capacity 4, coverage 1.00, exposure 0, shortfall 0. China's industrial row after round 2: capacity 8, coverage 1.00, no exports.
- **Counterfactual.** US `Restrict` against `FreeTrade`, six rounds: China's technology shortfall over the run is 20 against 0; the US firm's receipts 28 against 48, its sales at home 24 either way and abroad 4 against 24, and 5 of its orders blocked against none.
- **Private demand.** China `Rebalance`: China's private sector buys 0.89 `T` abroad in round 2, its requirement and its imports, with no capacity; 9.25 `T` over six rounds.

### 9.2 Firms, credit and the asymmetry

- **I. Credit.** China `Reformer`, its firm `Exporter`: the state signs and its firm holds 3 `K_I` in round 2. China `Planner`: the state refuses and its firm stays at 2.
- **J. Spending grows the home market.** Both firms `Patron`. After round 1 the US private sector holds 25 `M`; in round 2 it buys 10 `T` at home from its firm, which receives 12.5 `M`, and the US state, finding no technology on the world Exchange, is short 4 `T`.
- **K. Accumulating into a weak market.** Both firms `Builder`: with no private demand at home, the US firm builds 13 `K_T` by round 6 and holds 46 unsold `T`.
- **L. The US state's income.** US `Tariff`: levies are 2, 8, 8, 8, 8, 8 `M` by round, the export tax alone and then with the tariff.
- **M. China saves, America spends.** Both states `Rebalance`: after round 6 China's private sector holds 43.525 `M` and America's 13.2591.
- **N. Who a ban hurts.** After six rounds under a US ban the US state holds 0 `T` and its firm 20; under a Chinese ban China's state holds 20 `I` and its firm 10.

### 9.3 Engine fixtures

Literal worlds cut from Pacific with no production or requirements, run through `resolveRound` alone.

- **P-A.** The US firm sells 4 `T` to China on the world Exchange and pays 2 `M` of export tax: it ends with 52 `M` and 4 `T`, China with 96 `M` and 4 `T`, and the US state with 102 `M`. The Exchange holds nothing.
- **P-H.** The US firm sells 2 `T` at home to its state at 1.25, untaxed: the firm ends with 52.5 `M` and 6 `T`, the state with 97.5 `M` and 4 `T`.
- **P-B.** The US enacts its ban in round 1; in round 2 the same exports move nothing, leaving the firm with 50 `M` and 8 `T` and China with 100 `M` and no `T`.
- **P-G.** The Chinese firm offers 2 `I` and nobody buys: it keeps 50 `M` and 2 `I`.
- **P-C.** The Chinese firm orders an investment that China signs: in round 2 it holds no `I` and 3 `K_I`.
- **P-R.** China releases 10 `M` to its private sector, which in round 2 buys 2 `I` at home from China at 2.5: China ends with 95 `M` and 6 `I`, its private sector with 5 `M` and 2 `I`.

## 10. Sensitivity

Each dial changed alone: spend shares of 0.6 (US) and 0.3 (China), a US capacity split of 4 : 6, a Chinese split of 6 : 4, a tax rate of 0.25 or 1, 2 units of credit, and a home premium of 1.5. In every variant a US ban leaves the US firm holding technology it could not export, China's firm grows only when its state signs, released alike China's private sector saves more than America's, and the US state collects tax on its firm's exports.
