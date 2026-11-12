-- | Pacific's private sector (docs/05_pacific.md §2, §6.2): each country's private sector, an Aggregate holding
-- | private Money and demand, and its firm, a seat with a business of its own. A private sector is funded by its
-- | state's release and its firm's pay, and spends a share of what it holds: China's saves most of it, America's
-- | spends most. What it buys abroad it buys dearer under its own country's tariff, so it buys less.
module Premises.Pacific.Private
  ( PrivateDials
  , Level2
  , defaultPrivate
  , privateOf
  , firmOf
  , countries
  , homeOf
  , demandStrategies
  , parameters
  , rateParam
  , tariffedRateParam
  , releaseParam
  , payParam
  , stockpileParam
  , subsidyParam
  , tariffRate
  , lendParam
  , bondIssue
  , bond
  ) where

import Prelude

import Core.Num (Q)
import Core.Num as Q
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Laws.StrategyRules (StrategyRules, Template(..))
import Laws.Types (ActorId, ExchangeId, IntentF(..), ResourceId, RuleKindId, StrategyId, idText)
import Premises.Pacific.Names (p)
import Schema.Build.Decision (holding, mulQ, named, not', orderAt, param, priceAt, rule, ruleInForce)
import Schema.Build.Quantity (q)

-- | A tool's two settings above off: low and high.
type Level2 = { low :: Q, high :: Q }

-- | The private sector's numbers, and the firms': the share of what each private sector holds that it spends each
-- | round (China's low, America's high), the share of its Money a state releases and a firm pays at each level, the
-- | units of the foreign good a stockpiling firm buys at each level, and each firm's opening Money.
type PrivateDials =
  { spendShare :: { us :: Q, cn :: Q }
  , release :: Level2
  , pay :: Level2
  , stockpile :: Level2
  , lend :: { us :: Q, cn :: Q }
  , interest :: Q
  , firmMoney :: { us :: Q, cn :: Q }
  }

defaultPrivate :: PrivateDials
defaultPrivate =
  { spendShare: { us: q "0.8", cn: q "0.2" }
  , release: { low: q "0.05", high: q "0.15" }
  , pay: { low: q "0.05", high: q "0.15" }
  , stockpile: { low: q 1, high: q 2 }
  -- the share of its Money a private sector lends its state each round, and the interest on it
  , lend: { us: q "0.1", cn: q "0.4" }
  , interest: q "0.05"
  , firmMoney: { us: q 50, cn: q 50 }
  }

-- | Each country: its state, private sector, firm, private demand Strategy, its own good and that good's home
-- | Exchange and world Exchange, the foreign good's world Exchange, and its own tariff on the foreign good.
countries :: Array { state :: ActorId, private :: ActorId, firm :: ActorId, demand :: StrategyId, good :: ResourceId, home :: ExchangeId, ownWorld :: ExchangeId, foreign :: ResourceId, world :: ExchangeId, tariff :: RuleKindId }
countries =
  [ { state: p.us, private: p.usPrivate, firm: p.usFirm, demand: p.usPrivateDemand, good: p.technology, home: p.technologyUs, ownWorld: p.technologyWorld, foreign: p.industry, world: p.industryWorld, tariff: p.usImportTariff }
  , { state: p.cn, private: p.cnPrivate, firm: p.cnFirm, demand: p.cnPrivateDemand, good: p.industry, home: p.industryCn, ownWorld: p.industryWorld, foreign: p.technology, world: p.technologyWorld, tariff: p.cnImportTariff }
  ]

privateOf :: ActorId -> ActorId
privateOf state = if state == p.us then p.usPrivate else p.cnPrivate

firmOf :: ActorId -> ActorId
firmOf state = if state == p.us then p.usFirm else p.cnFirm

-- | A country's home Exchange: for its own good, open only to its own Actors.
homeOf :: ActorId -> ExchangeId
homeOf state = if state == p.us then p.technologyUs else p.industryCn

-- | The parameter a private sector's demand for a good reads: its spend share divided by the good's price, so
-- | Money times it is a quantity. The share is split evenly between the two goods.
rateParam :: ActorId -> ResourceId -> String
rateParam private good = idText private <> "-" <> idText good <> "-rate"

-- | The same, for the foreign good under its own country's tariff: the price it pays is the price and the tariff.
tariffedRateParam :: ActorId -> ResourceId -> String
tariffedRateParam private good = rateParam private good <> "-tariffed"

-- | The share of its Money a state releases at a level, "low" or "high".
releaseParam :: String -> String
releaseParam level = "release-" <> level

-- | The share of its Money a firm pays its private sector at a level.
payParam :: String -> String
payParam level = "pay-" <> level

-- | The units of the foreign good a stockpiling firm buys in a round at a level.
stockpileParam :: String -> String
stockpileParam level = "stockpile-" <> level

-- | The Money the US state pays its firm per unit of investment at a level.
subsidyParam :: String -> String
subsidyParam level = "subsidy-" <> level

-- | The share of its Money a private sector lends its state each round.
lendParam :: ActorId -> String
lendParam private = idText private <> "-lend-share"

-- | The templates of a household bond: its issue, which advances the principal, and the bond, repaid with interest.
bondIssue :: String
bondIssue = "bond-issue"

bond :: String
bond = "bond"

-- | The rate of either country's import tariff, per unit of the foreign good.
tariffRate :: String
tariffRate = "tariff-rate"

-- | The parameters the private sector's and the seats' Strategies read.
parameters :: PrivateDials -> { technologyPrice :: Q, industryPrice :: Q, tariff :: Q, subsidy :: Level2 } -> Object Q
parameters d prices = Object.fromFoldable
  ( [ Tuple tariffRate prices.tariff, Tuple (lendParam p.usPrivate) d.lend.us, Tuple (lendParam p.cnPrivate) d.lend.cn ]
      <> levels releaseParam d.release
      <> levels payParam d.pay
      <> levels stockpileParam d.stockpile
      <> levels subsidyParam prices.subsidy
      <> do
        c <- countries
        g <- [ Tuple p.technology prices.technologyPrice, Tuple p.industry prices.industryPrice ]
        let Tuple good price = g
        let share = if c.state == p.us then d.spendShare.us else d.spendShare.cn
        -- the share is split evenly across the two goods
        let rate pr = fromMaybe Q.zero (Q.divMaybe share (Q.mul (Q.fromInt 2) pr))
        [ Tuple (rateParam c.private good) (rate price), Tuple (tariffedRateParam c.private good) (rate (Q.add price prices.tariff)) ]
  )
  where
  levels f l = [ Tuple (f "low") l.low, Tuple (f "high") l.high ]

-- | A private sector's baseline: of its own good, at home, and of the foreign good, on the world Exchange, it buys
-- | its Money times the good's rate, at the Exchange's price; under its own country's tariff it buys the foreign good
-- | at the tariffed rate. What it buys it consumes.
demandStrategies :: Array (Tuple StrategyId StrategyRules)
demandStrategies = map (\c -> Tuple c.demand { rules: [ buy c.home (rateParam c.private c.good) [], abroad c false, abroad c true, lend c ], sign: [] }) countries
  where
  -- lend a share of its savings to its own state, as a bond the state signs (docs/05 §5.4)
  lend c = rule [] [ CandidateTransactionT { signers: [ c.state ], procedure: Nothing, intents: [ Enter { template: bondIssue, args: [ { name: "principal", value: mulQ (param (lendParam c.private)) (holding p.money) } ], parties: [ { role: "lender", actor: c.private }, { role: "borrower", actor: c.state } ] } ] } ]
  buy x rate conds = rule conds [ orderAt (named x) (mulQ (holding p.money) (param rate)) (Just (priceAt (named x))) ]
  abroad c tariffed =
    let
      inForce = ruleInForce { by: c.state, kinds: [ c.tariff ] }
    in
      if tariffed then buy c.world (tariffedRateParam c.private c.foreign) [ inForce ] else buy c.world (rateParam c.private c.foreign) [ not' inForce ]
