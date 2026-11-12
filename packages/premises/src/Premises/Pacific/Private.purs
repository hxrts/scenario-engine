-- | Pacific's private sector and its money (docs/05_pacific.md §2, §3.4, §6.2): each country's private sector, an
-- | Aggregate holding its home currency and demand; the rest of the world's savings, an Aggregate without a seat;
-- | the two currencies and the currency Exchange between them; and the loans every lender makes, all on one bond
-- | template. A private sector is paid wages, transfers, dividends and interest, spends a share of what it holds
-- | (China's saves most, America's spends most), and lends part of the rest to its state. What it buys abroad it
-- | buys with the other currency, bought first on the currency Exchange.
module Premises.Pacific.Private
  ( PrivateDials
  , WorldDials
  , Level2
  , Level3
  , Country
  , defaultPrivate
  , defaultWorld
  , countries
  , countryOf
  , level
  , demandStrategies
  , worldStrategy
  , parameters
  , releaseParam
  , payParam
  , stockpileParam
  , tariffRate
  , feeParam
  , reserveParam
  , loanShareParam
  , usBond
  , deposit
  , loan
  , ofForeign
  , lendTo
  , neg
  , addQ
  ) where

import Prelude

import Core.Num (Q)
import Core.Num as Q
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Laws.StrategyRules (Expr, StrategyRules, Template(..))
import Laws.Types (ActorId, ExchangeId, IntentF(..), ResourceId, RuleKindId, StrategyId, idText)
import Premises.Pacific.Names (p)
import Schema.Build.Decision (divQ, holding, lit, max', mulQ, named, not', orderAt, param, priceAt, rule, ruleInForce, subQ)
import Schema.Build.Quantity (q)

-- | A tool's two settings above off: low and high.
type Level2 = { low :: Q, high :: Q }

-- | A tool's three settings.
type Level3 = { off :: Q, low :: Q, high :: Q }

-- | The private sectors' numbers and the firms': the share of what each private sector holds that it spends each
-- | round (China's low, America's high), the share of its home currency a state transfers and a firm pays at each
-- | level, the units of its foreign input a stockpiling firm buys at each level, the share of its savings each
-- | private sector lends its state, and each firm's and private sector's opening money.
type PrivateDials =
  { spendShare :: { us :: Q, cn :: Q }
  , release :: Level2
  , pay :: Level2
  , stockpile :: Level2
  , lend :: { us :: Q, cn :: Q }
  , firmMoney :: { us :: Q, cn :: Q }
  , privateMoney :: { us :: Q, cn :: Q }
  }

defaultPrivate :: PrivateDials
defaultPrivate =
  { spendShare: { us: q "0.8", cn: q "0.2" }
  , release: { low: q "0.05", high: q "0.15" }
  , pay: { low: q "0.05", high: q "0.15" }
  , stockpile: { low: q 1, high: q 2 }
  , lend: { us: q "0.1", cn: q "0.4" }
  , firmMoney: { us: q 50, cn: q 50 }
  , privateMoney: { us: q 10, cn: q 10 }
  }

-- | The money between the countries (docs/05 §3.4, §5.5): the interest on US bonds; financial repression's three
-- | settings (the rate China pays its households, the rate it charges its firm, and the share of its RMB it lends
-- | the firm each round); the US asset fee per USD a foreigner lends; the USD China buys as reserves each round;
-- | and the rest of the world's savings, the share of its RMB it turns into USD each round, the share of its USD it
-- | lends the US state, and while the fee is in force the share it lends and the share it sells.
type WorldDials =
  { usRate :: Q
  , depositRate :: Level3
  , loanRate :: Level3
  , loanShare :: Level3
  , fee :: Level2
  , reserves :: Level2
  , savings :: { usd :: Q, rmb :: Q, buy :: Q, lend :: Q, lendUnderFee :: Q, flight :: Q }
  }

defaultWorld :: WorldDials
defaultWorld =
  { usRate: q "0.05"
  , depositRate: { off: q "0.05", low: q "0.02", high: q 0 }
  , loanRate: { off: q "0.05", low: q "0.01", high: q 0 }
  , loanShare: { off: q "0.05", low: q "0.1", high: q "0.2" }
  , fee: { low: q "0.02", high: q "0.05" }
  , reserves: { low: q 3, high: q 6 }
  , savings: { usd: q 20, rmb: q 60, buy: q "0.2", lend: q "0.5", lendUnderFee: q "0.2", flight: q "0.3" }
  }

-- | A country: its seats and private sector, its good and the foreign good, the Exchanges each trades on, its own
-- | tariff and export control and the other country's, and its home and the foreign currency.
type Country =
  { state :: ActorId
  , private :: ActorId
  , firm :: ActorId
  , other :: ActorId
  , demand :: StrategyId
  , good :: ResourceId
  , foreign :: ResourceId
  , home :: ExchangeId
  , ownWorld :: ExchangeId
  , world :: ExchangeId
  , tariff :: RuleKindId
  , control :: RuleKindId
  , otherControl :: RuleKindId
  , money :: ResourceId
  , foreignMoney :: ResourceId
  }

countries :: Array Country
countries = [ america, china ]

america :: Country
america = { state: p.us, private: p.usPrivate, firm: p.usFirm, other: p.cn, demand: p.usPrivateDemand, good: p.technology, foreign: p.industry, home: p.technologyUs, ownWorld: p.technologyWorld, world: p.industryWorld, tariff: p.usImportTariff, control: p.usExportControl, otherControl: p.cnExportControl, money: p.usd, foreignMoney: p.rmb }

china :: Country
china = { state: p.cn, private: p.cnPrivate, firm: p.cnFirm, other: p.us, demand: p.cnPrivateDemand, good: p.industry, foreign: p.technology, home: p.industryCn, ownWorld: p.industryWorld, world: p.technologyWorld, tariff: p.cnImportTariff, control: p.cnExportControl, otherControl: p.usExportControl, money: p.rmb, foreignMoney: p.usd }

-- | The country an Actor belongs to: its state, firm or private sector.
countryOf :: ActorId -> Country
countryOf a = if a == china.state || a == china.firm || a == china.private then china else america

-- | A tool's setting as a word: 0 off, 1 low, 2 high.
levelName :: Int -> String
levelName n = if n <= 0 then "off" else if n == 1 then "low" else "high"

-- | A setting above off as the word its parameter is named with.
level :: Int -> String
level n = if n >= 2 then "high" else "low"

neg :: Expr -> Expr
neg x = subQ (lit 0) x

addQ :: Expr -> Expr -> Expr
addQ x y = subQ x (neg y)

-- Currency -----------------------------------------------------------------------------------

-- | Turn an amount of a country's home currency into the foreign currency on the currency Exchange, whose base is
-- | USD and whose price is RMB per USD: America sells that many USD; China buys as many USD as the RMB buys.
toForeign :: Country -> Expr -> Template
toForeign c amount =
  if c.money == p.usd then orderAt (named p.currency) (neg amount) Nothing
  else orderAt (named p.currency) (divQ amount (priceAt (named p.currency))) Nothing

-- | Buy an amount of the foreign currency with home currency: America sells the USD that many RMB cost; China buys
-- | that many USD.
ofForeign :: Country -> Expr -> Template
ofForeign c amount =
  if c.money == p.usd then orderAt (named p.currency) (neg (divQ amount (priceAt (named p.currency)))) Nothing
  else orderAt (named p.currency) amount Nothing

-- Loans ----------------------------------------------------------------------------------------

-- | The US bond, which every lender to the US state holds: households, the rest of the world, China's reserves.
usBond :: String
usBond = "us-bond"

-- | China's household deposit at a repression setting, and its loan to its firm at one.
deposit :: Int -> String
deposit n = "cn-deposit-" <> levelName n

loan :: Int -> String
loan n = "cn-loan-" <> levelName n

-- | A loan, on the bond template (§3.4): the lender Transfers the principal to the borrower, and they enter the
-- | bond for the principal, which the borrower repays the next round with the bond's interest. The borrower signs. The
-- | principal moves as a Transfer, so the Rules in force screen it, which is how the US asset fee is charged.
lendTo :: { template :: String, lender :: ActorId, borrower :: ActorId, money :: ResourceId, principal :: Expr } -> Template
lendTo l = CandidateTransactionT
  { signers: [ l.borrower ]
  , procedure: Nothing
  , intents:
      [ Transfer { from: l.lender, to: l.borrower, resource: l.money, quantity: l.principal }
      , Enter { template: l.template, args: [ { name: "principal", value: l.principal } ], parties: [ { role: "lender", actor: l.lender }, { role: "borrower", actor: l.borrower } ] }
      ]
  }

-- Parameters ------------------------------------------------------------------------------------

-- | The share of its home currency a private sector spends on its own good at home, and turns into the foreign
-- | currency for the foreign good, without and with its own country's tariff in force; and one over a good's price
-- | on an Exchange, so a holding times it is a quantity.
homeShareParam :: ActorId -> String
homeShareParam private = idText private <> "-home-share"

abroadShareParam :: ActorId -> String
abroadShareParam private = idText private <> "-abroad-share"

tariffedShareParam :: ActorId -> String
tariffedShareParam private = idText private <> "-abroad-share-tariffed"

priceParam :: ExchangeId -> String
priceParam x = idText x <> "-per-unit"

releaseParam :: String -> String
releaseParam l = "release-" <> l

payParam :: String -> String
payParam l = "pay-" <> l

stockpileParam :: String -> String
stockpileParam l = "stockpile-" <> l

-- | The US asset fee per USD lent at a level, and the USD China buys as reserves in a round at one.
feeParam :: String -> String
feeParam l = "asset-fee-" <> l

reserveParam :: String -> String
reserveParam l = "reserves-" <> l

-- | The share of its RMB China's state lends its firm at a repression setting.
loanShareParam :: Int -> String
loanShareParam n = "loan-share-" <> levelName n

-- | The share of its home currency a private sector lends its state each round.
lendParam :: ActorId -> String
lendParam private = idText private <> "-lend-share"

-- | The rate of either country's import tariff, per unit of the foreign good, in the importer's currency.
tariffRate :: String
tariffRate = "tariff-rate"

-- | The rest of the world's shares.
rowParam :: String -> String
rowParam x = "row-" <> x

-- | The parameters the private sectors', the rest of the world's and the seats' Strategies read.
parameters :: PrivateDials -> WorldDials -> { technologyPrice :: Q, industryPrice :: Q, homePremium :: Q, tariff :: Q } -> Object Q
parameters d w prices = Object.fromFoldable
  ( [ Tuple tariffRate prices.tariff
    , Tuple (lendParam p.usPrivate) d.lend.us
    , Tuple (lendParam p.cnPrivate) d.lend.cn
    , Tuple (rowParam "buy") w.savings.buy
    , Tuple (rowParam "lend") w.savings.lend
    , Tuple (rowParam "lend-under-fee") w.savings.lendUnderFee
    , Tuple (rowParam "flight") w.savings.flight
    , Tuple (loanShareParam 0) w.loanShare.off
    , Tuple (loanShareParam 1) w.loanShare.low
    , Tuple (loanShareParam 2) w.loanShare.high
    ]
      <> levels releaseParam d.release
      <> levels payParam d.pay
      <> levels stockpileParam d.stockpile
      <> levels feeParam w.fee
      <> levels reserveParam w.reserves
      <> map (\(Tuple x pr) -> Tuple (priceParam x) (fromMaybe Q.zero (Q.divMaybe Q.one pr)))
        [ Tuple p.technologyWorld prices.technologyPrice
        , Tuple p.industryWorld prices.industryPrice
        , Tuple p.technologyUs (Q.mul prices.homePremium prices.technologyPrice)
        , Tuple p.industryCn (Q.mul prices.homePremium prices.industryPrice)
        ]
      <> do
        c <- countries
        let share = if c.state == p.us then d.spendShare.us else d.spendShare.cn
        -- the share is split evenly between the two goods; under its own tariff, of the half for the foreign good
        -- it turns into the other currency only what leaves enough for the tariff, at the price
        let half = Q.div share (Q.fromInt 2)
        let price = if c.foreign == p.industry then prices.industryPrice else prices.technologyPrice
        [ Tuple (homeShareParam c.private) half
        , Tuple (abroadShareParam c.private) half
        , Tuple (tariffedShareParam c.private) (fromMaybe Q.zero (Q.divMaybe (Q.mul half price) (Q.add price prices.tariff)))
        ]
  )
  where
  levels f l = [ Tuple (f "low") l.low, Tuple (f "high") l.high ]

-- Strategies --------------------------------------------------------------------------------------

-- | A private sector's baseline (§6.2): it buys its own good at home with a share of its home currency; turns
-- | another share into the foreign currency (less under its own country's tariff, which it pays at home); buys the
-- | foreign good abroad with all the foreign currency it holds; and lends a share of what it holds to its state:
-- | America's at the US bond's rate, China's at the rate its state signs. What it buys it consumes.
demandStrategies :: Array (Tuple StrategyId StrategyRules)
demandStrategies = map (\c -> Tuple c.demand { rules: [ home c, convert c false, convert c true, abroad c ] <> lend c, sign: [] }) countries
  where
  home c = rule [] [ orderAt (named c.home) (mulQ (mulQ (holding c.money) (param (homeShareParam c.private))) (param (priceParam c.home))) (Just (priceAt (named c.home))) ]
  inForce c = ruleInForce { by: c.state, kinds: [ c.tariff ] }
  convert c tariffed =
    if tariffed then rule [ inForce c ] [ toForeign c (mulQ (holding c.money) (param (tariffedShareParam c.private))) ]
    else rule [ not' (inForce c) ] [ toForeign c (mulQ (holding c.money) (param (abroadShareParam c.private))) ]
  abroad c = rule [] [ orderAt (named c.world) (mulQ (holding c.foreignMoney) (param (priceParam c.world))) (Just (priceAt (named c.world))) ]
  principal c = mulQ (param (lendParam c.private)) (holding c.money)
  -- China's households offer a deposit at each repression setting; only the one their state signs goes ahead
  lend c =
    if c.state == p.us then [ rule [] [ lendTo { template: usBond, lender: c.private, borrower: c.state, money: c.money, principal: principal c } ] ]
    else [ rule [] (map (\n -> lendTo { template: deposit n, lender: c.private, borrower: c.state, money: c.money, principal: principal c }) [ 0, 1, 2 ]) ]

-- | The rest of the world's savings (§6.2): while the US asset fee is not in force it turns a share of its RMB into
-- | USD and lends the US state a share of its USD; while it is, it lends less and sells a share of its USD.
worldStrategy :: StrategyRules
worldStrategy =
  { rules:
      [ rule [ not' fee ] [ orderAt (named p.currency) (divQ (mulQ (param (rowParam "buy")) (holding p.rmb)) (priceAt (named p.currency))) Nothing ]
      , rule [ not' fee ] [ lendAt (rowParam "lend") ]
      , rule [ fee ] [ orderAt (named p.currency) (neg (mulQ (param (rowParam "flight")) (holding p.usd))) Nothing ]
      , rule [ fee ] [ lendAt (rowParam "lend-under-fee") ]
      ]
  , sign: []
  }
  where
  fee = ruleInForce { by: p.us, kinds: [ p.usAssetFee ] }
  lendAt share = lendTo { template: usBond, lender: p.row, borrower: p.us, money: p.usd, principal: max' (lit 0) (mulQ (param share) (holding p.usd)) }
