-- | Every number Pacific is built from (docs/05 §9): a variant changes these and nothing else. The regime and the
-- | clearing builders read them.
module Premises.Pacific.Dials
  ( Dials
  , Investment
  , Competence
  , Objectives
  , defaultDials
  , gameSeed
  ) where

import Core.Num (Q)
import Premises.Pacific.Private (PrivateDials, WorldDials, defaultPrivate, defaultWorld)
import Schema.Build.Quantity (q)

-- | The game seed of the reference instance.
gameSeed :: Int
gameSeed = 1949

-- | Every authored number (docs/05 §9): prices (technology in USD, industrial inputs in RMB) and the home premium,
-- | the currency Exchange, opening holdings and how each country's capacity is split between its state and its
-- | firm, requirements, each investment, competence, wages, the tariff and tax rates, the private sectors' and the
-- | rest of the world's. A variant changes these and nothing else.
type Dials =
  { technologyPrice :: Q
  , industryPrice :: Q
  , homePremium :: Q
  , stock :: { technology :: Q, industry :: Q }
  , stockSlope :: { technology :: Q, industry :: Q }
  , homeStock :: { technology :: Q, industry :: Q }
  , homeStockSlope :: { technology :: Q, industry :: Q }
  , currency :: { rate :: Q, slope :: Q, depth :: Q, reserves :: Q }
  , money :: { us :: Q, cn :: Q }
  , foreignMoney :: { us :: Q, cn :: Q, usFirm :: Q, cnFirm :: Q }
  , technologyCapacity :: { us :: Q, usFirm :: Q }
  , industryCapacity :: { cn :: Q, cnFirm :: Q, us :: Q }
  , technologyNeed :: { us :: Q, cn :: Q }
  , industryNeed :: { us :: Q, cn :: Q }
  , usInvestment :: Investment
  , cnInvestment :: Investment
  , firmInvestment :: Investment
  , competence :: Competence
  , inputPerUnit :: { usFirm :: Q, cnFirm :: Q }
  , inputShare :: Q
  , wage :: { usTechnology :: Q, usIndustry :: Q, cnTechnology :: Q, cnIndustry :: Q }
  , tariffRate :: Q
  , taxRate :: Q
  , cautiousOutput :: Q
  , upkeep :: Q
  , private :: PrivateDials
  , world :: WorldDials
  , objectives :: Objectives
  }

-- | An investment: the goods a unit costs, the construction wages it pays its country's households, the capacity it
-- | yields, and the rounds until it arrives.
type Investment = { cost :: Q, wages :: Q, yield :: Q, delay :: Int }

-- | Industrial competence (docs/05 §3.2): what each producer opens with of each good's competence, the share of it
-- | that wears away each round, what each unit produced teaches, and what a unit of investment trains.
type Competence =
  { decay :: Q
  , learning :: Q
  , us :: { technology :: Q, industry :: Q }
  , cn :: { technology :: Q, industry :: Q }
  , usFirm :: Q
  , cnFirm :: Q
  , stateTraining :: Q
  , firmTraining :: Q
  }

-- | The bars each seat's four criteria set (docs/05 §8). A state: the most of the good it imports it may go short
-- | over the run, the capacity its firm ends with, its political viability every round (America's households'
-- | purchases, China's industrial output), and its households' purchases over the run. A firm: the money it ends
-- | with in its home currency, its capacity (the same bar as its state's), its production on the foreign input, and
-- | its households' purchases over the run, its home market.
type Objectives =
  { security :: { us :: Q, cn :: Q }
  , viability :: { us :: Q, cn :: Q }
  , prosperity :: { us :: Q, cn :: Q }
  , industry :: { us :: Q, cn :: Q }
  , wealth :: { us :: Q, cn :: Q }
  , output :: { us :: Q, cn :: Q }
  , market :: { us :: Q, cn :: Q }
  }

-- | The reference numbers (docs/05 §3). The US splits its technology capacity 2 : 8 between state and firm, China
-- | its industrial capacity 8 : 2, so each country's exports come mostly from a different seat.
defaultDials :: Dials
defaultDials =
  { technologyPrice: q 1
  , industryPrice: q 2
  , homePremium: q "1.25"
  -- each world Exchange holds a stock of its good: it sells from it when buyers want more than sellers offer, its
  -- price rising along its curve, and buys back only what it sold, its price falling back (docs/02 §5.1)
  , stock: { technology: q 4, industry: q 6 }
  , stockSlope: { technology: q "0.05", industry: q "0.1" }
  -- each home Exchange holds a stock of its country's good likewise, its price rising a quarter more per unit
  , homeStock: { technology: q 4, industry: q 6 }
  , homeStockSlope: { technology: q "0.0625", industry: q "0.125" }
  -- one RMB per USD to start; the price moves half a hundredth for each USD of net buying, within 100 USD either way
  , currency: { rate: q 1, slope: q "0.005", depth: q 100, reserves: q 150 }
  , money: { us: q 112, cn: q 88 }
  -- each opens with two rounds of the other currency its imports need
  , foreignMoney: { us: q 12, cn: q 6, usFirm: q 4, cnFirm: q 4 }
  , technologyCapacity: { us: q 2, usFirm: q 8 }
  -- America opens with no industrial capacity of its own
  , industryCapacity: { cn: q 8, cnFirm: q 2, us: q 0 }
  , technologyNeed: { us: q 6, cn: q 3 }
  , industryNeed: { us: q 5, cn: q 4 }
  -- capacity arrives `delay` rounds after the order, and its first output one round after that
  , usInvestment: { cost: q 2, wages: q 2, yield: q 2, delay: 1 }
  , cnInvestment: { cost: q 4, wages: q 2, yield: q 2, delay: 2 }
  , firmInvestment: { cost: q 2, wages: q 4, yield: q 1, delay: 1 }
  -- a producer running its capacity fully holds competence at learning / decay = 1.5 times it, where what a round
  -- teaches makes up what it wears away; so each country opens at that level in its own good, and America with a
  -- quarter of a unit of industrial competence, so capacity it builds runs only as fast as it learns
  , competence: { decay: q "0.1", learning: q "0.15", us: { technology: q 3, industry: q "0.25" }, cn: { technology: q 0, industry: q 12 }, usFirm: q 12, cnFirm: q 3, stateTraining: q 1, firmTraining: q 1 }
  -- half of a firm's capacity runs on the foreign input, at this much per unit: each firm's uses one unit a round
  , inputPerUnit: { usFirm: q "0.25", cnFirm: q 1 }
  , inputShare: q "0.5"
  -- what a unit produced pays its producer's households, in its currency. Each country makes its own good cheaply;
  -- the other's it can make for a little under what importing it costs at one RMB per USD (2 RMB is 2 USD for
  -- industrial inputs, 1 USD is 1 RMB for technology), so the exchange rate decides whether its substitute runs
  , wage: { usTechnology: q "0.25", usIndustry: q "1.95", cnTechnology: q "0.97", cnIndustry: q "0.25" }
  , tariffRate: q 1
  , taxRate: q "0.5"
  , cautiousOutput: q "0.5"
  -- what each unit of capacity held costs a round in maintenance, idle or not, paid to its country's households
  , upkeep: q "0.05"
  , private: defaultPrivate
  , world: defaultWorld
  -- each bar is set near the 40th percentile of its measure over the balance sample (docs/05 §10.4), so a seat
  -- meets each about three times in five
  , objectives:
      { security: { us: q "19.52", cn: q "13.6" }
      , viability: { us: q "0.6", cn: q 8 }
      , prosperity: { us: q "12.5", cn: q "0.63" }
      , industry: { us: q "10.9", cn: q 4 }
      , wealth: { us: q "3.35", cn: q "8.85" }
      , output: { us: q "5.1", cn: q "1.5" }
      , market: { us: q "12.5", cn: q "0.63" }
      }
  }

