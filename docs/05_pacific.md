# PremiseComposition: Pacific

**Status:** Experimental configuration.  
**PremiseComposition ID:** `pacific`, version `v0`

## 1. Purpose

Pacific is the smallest PremiseComposition that expresses one strategic tension between two countries. Each produces something the other needs, so trade benefits both. Each can ban its exports to impose a cost on the other. Each can invest in a domestic substitute, which reduces its exposure to the other's ban but uses up goods it could have consumed or sold. Successful substitution reduces the leverage and the export income of the original supplier.

Every consequence comes from Stocks, Flows, Transformations, Exchanges and Laws ([03 §3.1](03_premise.md#31-strategic-tensions)). There is no leverage, security or prosperity Resource, and no geopolitical score. The composition adds no mechanism of its own to the engine. The general features it needed are recorded where they belong: Exchange trades settling as buyer–seller pairs, so an Exchange with no reserves only matches ([02 §5.2](02_kernel.md#52-exchanges), [§5.5](02_kernel.md#55-simultaneous-clearing)); Rules acting on those pairs as on Transfers ([03 §5.5](03_premise.md#55-geography-access-and-rules)); repeated enactment adding nothing ([03 §6](03_premise.md#6-actor-catalog)); and an Actor's routine beside its player ([04 §6](04_run.md#6-strategy-initialization)).

Version 0 has no currencies, credit, third countries, firms or sectors, and declares no tariff, though the engine would charge one on an Exchange trade (§11, levy scenario). All values are normalized game units, not empirical statistics.

# Part I: Economy

## 2. Premises

Two Premises, in this order, each at version `v0` ([03 §2](03_premise.md#2-premises-and-compositions)):

| Premise | Writes |
|---|---|
| `bilateral-economy` | The header, the Actors, the Resources, the two Exchanges, the recurring cycle, the opening holdings, and the commerce Strategy `pacific-commerce-v0`. On its own it is a complete economy with no bans or investments. |
| `strategic-dependence` | The two Rule kinds, the two investment Transformations, and, under `extends`, the Capabilities that enact each country's ban and run its investment. On its own it fails at load, because it extends Actors only the other Premise defines. |

The authored source is `Premises.Pacific`. Its dials (§10) are the only way a variant changes a number.

## 3. Actors and Resources

Two required Actors, each a seat: `US` and `CN`. Each is a government and its national economy combined: one account holds its holdings, and it exercises its own authority. Each may trade in both Exchanges and transfer Money and both goods. `strategic-dependence` adds the authority to enact its own ban and to run its own investment, and nothing that acts for the other country.

| Resource | ID | Transferability | Use |
|---|---|---|---|
| Money | `M` | Free | The numeraire. No mechanism creates or destroys it. |
| Technology | `T` | Free | A circulating good. |
| Industrial inputs | `I` | Free | A circulating good. |
| Technology capacity | `K_T` | Non-transferable | Produces `T` each round. |
| Industrial capacity | `K_I` | Non-transferable | Produces `I` each round. |

Opening holdings. The goods are one production cycle's output, available in round 1.

| Resource | US | CN |
|---|---:|---:|
| `M` | 100 | 100 |
| `T` | 10 | 0 |
| `I` | 0 | 10 |
| `K_T` | 10 | 0 |
| `K_I` | 0 | 10 |

## 4. Circulation

- **Production.** One unit of each capacity produces one unit of its good each round, delivered in the next round. There are entries for both countries and both goods, including the ones with no capacity yet, so an investment activates a production entry that already exists.
- **Requirements.** Fixed each round: the US needs 6 `T` and 6 `I`, China 4 `T` and 4 `I`. A requirement settles after the round's batch, so goods bought in a round meet that round's need. A shortfall is recorded, with no penalty, and does not stop later rounds.

The baseline balance: the US produces 10 `T`, consumes 6 and exports 4; China produces 10 `I`, consumes 4 and exports 6. Both meet every requirement when trade is free.

## 5. Exchanges

| Exchange | Base | Quote | Price |
|---|---|---|---:|
| `technology-market` | `T` | `M` | 1 |
| `industry-market` | `I` | `M` | 2 |

Both are constant (slope 0), both countries have access, and neither holds reserves. Each domain starts at its position, so a batch's net is zero and every trade settles as a pair between the two countries ([02 §5.2](02_kernel.md#52-exchanges)): the Exchange never buys unsold output, and a seller is paid only for what a buyer takes. Each pair moves the good and the Money together or not at all.

## 6. A round

Each round follows the canonical sequence ([02 §8.1](02_kernel.md#81-canonical-resolution-sequence)):

1. **Prepare.** Due Rule changes take effect, new capacity arrives, and last round's production is delivered.
2. **Observe and decide.** Each country's player (a preset, an agent or a human) chooses its strategic actions from its View: enact or repeal its ban, invest, or nothing.
3. **Commerce.** Each country's routine then drafts its sales and purchases, holding back what the player's investment will consume.
4. **Resolve.** Orders are funded from the opening World, the Exchanges clear, bans refuse their pairs, investments consume their inputs, and requirements are consumed from what is held after the batch.
5. **Commit.** The round-end World and its record are stored. Receipts from a round's sales are spendable from the next round.

## 7. Strategies

### 7.1 Commerce

`pacific-commerce-v0` is each country's routine ([04 §6](04_run.md#6-strategy-initialization)). It runs after whoever plays the seat, a preset, an agent or a human, and reads what they put forward. For each good it:

- sells `max(0, holding − requirement − planned inputs)` at the market's price, where planned inputs are what the seat's investment this round consumes of the good (`Planned`, [03 §8](03_premise.md#8-strategy-catalog));
- buys `max(0, requirement − holding)` at the market's price.

It does not speculate or accumulate imports beyond its requirement. One Strategy serves both countries, since each reads its own requirement (`Due`).

### 7.2 Presets

A preset is a country's strategic policy only. A clause that names the other country's ban or investment drafts nothing, since the acting Actor lacks the authority, so one preset serves both seats.

| Preset | Behaviour |
|---|---|
| `FreeTrade` | No ban, no investment. |
| `Restrict` | Enacts its own ban while none of its Rules is in force. |
| `Substitute` | Invests one unit a round while its capacity held plus capacity on order (`Scheduled`) is below its requirement of the good the capacity produces. |
| `RestrictAndSubstitute` | Both. |
| `Adaptive` | Invests as `Substitute`, but only while the other country's ban is in force. |

`Adaptive` does not repeal: a repeal names the Rule's id, which the decision language cannot name. A player who wants to lift a ban does so through a human or agent seat.

## 8. Investment and bans

| Transformation | Executor | Consumes | Creates | Capacity arrives | First extra output |
|---|---|---|---|---|---|
| `us-industrial-investment` | US | 2 `T` | +2 `K_I` | 1 round after the order | 2 rounds after the order |
| `cn-technology-investment` | CN | 4 `I` | +2 `K_T` | 2 rounds after the order | 3 rounds after the order |

At most one unit per country per round. Inputs are consumed when the order settles; no Money is spent and no other Actor is paid.

| Rule kind | Governor | Blocks |
|---|---|---|
| `us-technology-ban` | US | `T` from the US to China |
| `cn-industrial-ban` | China | `I` from China to the US |

Each is `BlockFlows GovernedBySource`. A ban takes effect at the next round boundary and stays in force until repealed. Enacting it again while it is in force adds nothing. A ban refuses the pair between the two countries' orders, so neither the good nor the Money moves, the seller keeps its goods, and the price is unchanged; both orders are recorded as removed, `Blocked`. It also refuses a gift Transfer and a joint candidate Transaction between the two, and a routed order pairs like any other, so a ban cannot be passed by changing how a trade is arranged.

## 9. Objectives and reporting

Each country's objective is evaluated from the recorded Run and never changes the economy ([03 §10.5](03_premise.md#105-evaluation)):

| Country | Objective | Criteria |
|---|---|---|
| US | Meet domestic needs while reducing dependence on Chinese industrial inputs | No shortfall of `T` or `I` over the run; final `K_I` at least its `I` requirement (6). |
| China | Meet domestic needs while reducing dependence on US technology and keeping industrial export income | No shortfall of `T` or `I` over the run; final `K_T` at least its `T` requirement (4). |

Strategic dependence is derived, not declared: `coverage = min(1, capacity / requirement)` and `exposure = max(0, requirement − capacity)`, per country and good. The run's Dependence view shows them each round beside imports, exports, receipts, shortfall, the bans in force and the capacity on order ([07 §9.2](07_simulator.md#92-run-details)). Comparing a run with its counterfactual without a ban gives the ban's realized effect as the difference in shortfall, beside receipts (prosperity) and final coverage (autonomy) in the Compare key values ([07 §10.2](07_simulator.md#102-views)).

## 10. Dials

`Premises.Pacific.Dials` holds every number: both goods' prices, each country's opening Money, each capacity, each requirement, and each investment's cost, yield and lead time. A variant changes these and nothing else.

# Part II: Fixtures

Each scenario plays the composition with the Strategies named and checks the round-end World and the round's records. Expected values are literals. The tests are:

- `packages/handlers/test/Test/Handlers/Pacific.purs`: the golden scenarios (§11), the routine, and the sensitivity sweep (§12);
- `packages/premises/test/Test/Premises/Golden.purs`: the engine fixtures (§11.1), from `Premises.Pacific.Fixtures`;
- `packages/premises/test/Test/Premises/Compose.purs`: the two Premises and the dials;
- `packages/views/test/Test/Views/Dependence.purs`: the Dependence view and the counterfactual;
- `apps/server/test/Test/Server/Compositions.purs` and the dashboard's `zz-pacific.spec.js`: the composition served, launched and shown.

## 11. Golden scenarios

- **A. Free trade.** Both `FreeTrade`, six rounds. Each round 4 `T` go from the US to China and 6 `I` from China to the US. After round 1 the US holds 92 `M` and China 108; after round 6, 52 and 148. No shortfall. Money totals 200 every round.
- **B. US technology ban.** US `Restrict`, China `FreeTrade`. No Rule is in force after round 1 and one after round 2. In round 2 no `T` crosses, China is short 4 `T`, US technology receipts are 0, the US keeps 4 `T`, 6 `I` still cross, and a `Blocked` record is written.
- **C. Chinese industrial ban.** US `FreeTrade`, China `Restrict`. In round 2 no `I` crosses, the US is short 6 `I`, Chinese industrial receipts are 0, and 4 `T` still cross.
- **D. US substitution.** The US invests once, in round 1. Its round 1 technology exports are 2. It holds 2 `K_I` after round 2 and still imports 6 `I` in round 2. In round 3 it imports 4 `I`, China's receipts from industrial sales are 8 `M`, and the US is not short.
- **E. Chinese substitution.** China invests once, in round 1. Its round 1 industrial exports are 2. It holds 2 `K_T` after round 3 and still imports 4 `T` in round 3. In round 4 it imports 2 `T`, US technology receipts are 2 `M`, and China is not short.
- **F. Ban and adaptation.** US `Restrict`, China `Adaptive`. China's technology shortfall by round is 0, 4, 4, 4, 2, 0. After round 6 it holds 4 `K_T` and imports nothing, so the ban, still in force, costs it nothing. Reversed (US `Adaptive`, China `Restrict`), the US industrial shortfall is 0, 6, 6, 4, 2, 0.
- **G. No phantom trade.** US `Substitute`, China `FreeTrade`. In round 5 the US buys no `I`, China is paid nothing for `I`, holds 12 unsold `I`, and holds 122 `M`.
- **H. Invariants.** Every round of every scenario passes `checkRun`. For US `Restrict` with China `Adaptive`, and both `RestrictAndSubstitute`, the replay handler reproduces every round-end World, and every reordering of each round's inputs settles to the same World. A ban enacted twice in one round is one Rule in force. Under a US ban in round 2, a US gift of 3 `T`, a joint candidate Transaction moving 1 `T` that China signs, and a Chinese routed purchase of 4 `T` at limit 1 all fail: China holds no `T`, is short 4, and at least three `Blocked` records are written.
- **Levy on a pair.** A US import levy of 1 `M` per unit of `I` from China, in force from the start, both `FreeTrade`, one round: China ships 6 `I` and pays 6 `M` once, holding 102 `M`; the US holds 98 `M`; one levy is recorded.
- **Routine.** A human US seat is composed with its commerce routine. With nothing spoken, the routine sells 4 `T`; when the human's interpreted speech orders one industrial investment, it sells 2, and an order of 4 against the maximum of 1 also leaves it selling 2.
- **Dependence view.** US `Restrict`, China `Adaptive`. China's technology row after round 2: requirement 4, capacity 0, coverage 0.00, exposure 4, no trade, shortfall 4, one ban against it, 2 capacity on order. After round 6: capacity 4, coverage 1.00, exposure 0, shortfall 0. China's industrial row after round 2: capacity 10, coverage 1.00, exports 2, receipts 4.
- **Counterfactual.** US `Restrict` against both `FreeTrade`, six rounds. China's technology shortfall over the run is 20 against 0, and US sales receipts are 4 against 24.

### 11.1 Engine fixtures

Three literal worlds cut from Pacific, with no circulation, through `resolveRound` alone (`Premises.Pacific.Fixtures`, run by `Golden.purs` with Meridian's):

- **P-A.** Each country submits its round-1 commerce at the prices. US: 92 `M`, 6 `T`, 6 `I`. China: 108 `M`, 4 `T`, 4 `I`. Both Exchanges hold no `M` and no goods.
- **P-B.** The US enacts its ban in round 1; in round 2 the same commerce leaves the US with 88 `M`, 10 `T`, 6 `I`, and China with 112 `M`, 0 `T`, 4 `I`.
- **P-G.** China offers 6 `I` and nobody buys. China keeps 100 `M` and 10 `I`; the Exchange holds nothing.

## 12. Sensitivity

Over the dials, each changed alone: technology price 2, industrial price 1 or 3, Chinese investment cost 2 or 6 and lead time 1 or 3, Chinese technology requirement 2 or 5, and US opening Money 60. Each is played for ten rounds. In every variant, free trade meets every requirement for the first six rounds, a US ban leaves China short in round 2, `Adaptive` ends China's shortfall before round 10, and Chinese substitution under free trade lowers US technology receipts.

Free trade is not stable when one side runs a deficit. With industrial inputs at 3, or US opening Money at 60, the US pays more than it earns each round and runs short of Money in rounds 7 to 8. Its purchases are then rationed, and because one funding ratio scales every claim of its scale group ([02 §8.2](02_kernel.md#82-funding-and-settlement)), its technology sales are scaled with them, so China goes short of `T` with no ban in force. This is the engine's rationing rule, not a Pacific mechanism; version 0 has no credit to finance a deficit.
