-- | The Clearing view (docs/07 §9.3; docs/05 §4.4): what crossed between countries, round by round, in the clearing
-- | system's three categories, and the Rules that stood at the borders. Generic: a country is an Actor group;
-- | everything outside it, the Exchanges' reserves included, is abroad. The exchange rate is the currency Exchanges'
-- | price, in the Exchanges view, and the current account is in the Finance view.
-- |
-- | - **Trade**: goods that moved from one country to another on the Exchanges.
-- | - **Capital**: currency lent across a border and repaid: principal Transferred, and agreements repaid.
-- | - **Rules**: the flows Rules in force after each round, with the country that governs each and the one it binds.
module Views.Clearing
  ( clearing
  , Step
  , goodsColumns
  , capitalColumns
  , rulesColumns
  , columns
  ) where

import Prelude

import Core.Kernel.Types (Endpoint(..))
import Core.Map (values)
import Core.Num as Q
import Data.Array (any, concatMap, filter, foldl, fromFoldable, nub, snoc, sort, unsnoc, zip)
import Data.String (joinWith)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..), fst, snd)
import Laws.External (groupOf)
import Laws.Slice (legs, select)
import Laws.Types (ResourceId, RuleF(..))
import Laws.World (Cause(..), Mechanism(..), WorldState)
import Core.Exchange (exchangePrice)
import Core.Num (Kind(..))
import Views.Common (Chart, Column, StoredRun, Table, col, column, displayName, names, pointSeries, quantity, unitOf, worlds)
import Views.Finance (externalAccounts)
import Views.Indicators (countryIndicators)
import Views.Standings (valueIn)

-- | One step of the circuit between the countries: its chart, and the table of what moved behind it.
type Step = { chart :: Chart, table :: Table }

-- | The circuit between the countries (docs/05 §4.4), a step at a time: the goods each sold abroad less what it
-- | bought, what it was lent from abroad less what it repaid, the position abroad that leaves it with, and the
-- | exchange rate between their currencies; then the Rules at the borders.
clearing :: StoredRun -> { trade :: Step, lending :: Step, position :: Step, rate :: Step, rules :: Table }
clearing r =
  { trade:
      { chart: { title: "Trade balance", unit: money, kind: Quantity, series: pointSeries balances }
      , table: { title: "Trade between countries", columns: names ([ col.round ] <> goodsColumns), rows: concatMap goodsRows steps }
      }
  , lending:
      { chart: { title: "Net lending received", unit: money, kind: Quantity, series: pointSeries lending }
      , table: { title: "Capital between countries", columns: names ([ col.round ] <> capitalColumns), rows: concatMap capitalRows steps }
      }
  , position:
      { chart: { title: "Net external position", unit: money, kind: Quantity, series: pointSeries positions }
      , table: { title: "Net external position", columns: names [ col.round, countryColumn, positionColumn ], rows: map (\x -> [ show x.round, x.name, maybe "n/a" quantity x.value ]) positions }
      }
  , rate:
      { chart: { title: "Exchange rate", unit: "", kind: Price, series: pointSeries rates }
      , table: { title: "Exchange rate", columns: names [ col.round, exchangeColumn, rateColumn ], rows: map (\x -> [ show x.round, x.name, maybe "n/a" quantity x.value ]) rates }
      }
  -- one row per Rule, with the rounds it stood after
  , rules: { title: "Rules at the borders", columns: names ([ roundsColumn ] <> rulesColumns), rows: map (\k -> [ spans (map fst (filter (\(Tuple _ x) -> x == k) inForce)) ] <> k) (nub (map snd inForce)) }
  }
  where
  steps = zip (worlds r) r.rounds
  money = unitOf r r.manifest.initial.numeraire
  indicators = countryIndicators r
  positions = map (\i -> { round: i.round, name: show i.country, value: Just i.netExternal }) indicators
  -- goods across the border, each currency's valued in the numeraire at the round's closing prices
  accounts = externalAccounts r
  balances = concatMap
    ( \x ->
        let
          lines = filter (\l -> l.round == x.record.round) accounts
        in
          map (\c -> { round: x.record.round, name: show c, value: Just (Q.sumQ (map (\l -> Q.mul (valueIn x.world l.currency) l.goods) (filter (\l -> l.country == c) lines))) }) (nub (map _.country lines))
    )
    r.rounds
  -- what each country was lent from abroad in the round, less what it repaid abroad
  lending = concatMap
    ( \(Tuple before x) ->
        let
          moves = concatMap
            ( \l -> case lendingCause l.cause, crossing x.world l of
                Just kind, Just (Tuple a b) -> [ { kind, from: a, to: b, worth: Q.mul l.moved (valueIn before l.resource) } ]
                _, _ -> []
            )
            (select legs x.record.events)
          net c = Q.sub (Q.sumQ (map _.worth (filter (\m -> m.kind == "lent" && m.to == c) moves))) (Q.sumQ (map _.worth (filter (\m -> m.kind == "repaid" && m.from == c) moves)))
        in
          map (\c -> { round: x.record.round, name: c, value: Just (net c) }) (nub (map (show <<< _.country) (filter (\i -> i.round == x.record.round) indicators)))
    )
    steps
  -- the price on each Exchange that trades one currency for another
  rates = concatMap (\x -> map (\m -> { round: x.record.round, name: displayName x.world m.id, value: Just (exchangePrice m) }) (filter (\m -> not (isGood x.world m.base) && m.base /= x.world.numeraire || any (\o -> o.quote == m.base) (values x.world.exchanges)) (fromFoldable (values x.world.exchanges)))) r.rounds
  inForce = concatMap (\x -> map (Tuple x.record.round) (rulesRows x)) r.rounds
  countryAt w = case _ of
    Held a -> Just (show (groupOf w a))
    InExchange x -> Just (displayName w x <> ", reserve")
    _ -> Nothing
  crossing w l = case countryAt w l.from, countryAt w l.to of
    Just a, Just b | a /= b -> Just (Tuple a b)
    _, _ -> Nothing
  -- every good that moved on an Exchange between two countries, by good and direction
  goodsRows (Tuple before x) =
    let
      w = x.world
      moves = concatMap
        ( \l -> case l.cause, crossing w l of
            OfMatch { exchange }, Just (Tuple a b) | isGood before l.resource -> [ { good: l.resource, from: a, to: b, quantity: l.moved, worth: Q.mul l.moved (priceOf before exchange) } ]
            _, _ -> []
        )
        (select legs x.record.events)
      keys = sort (nub (map (\m -> Tuple (displayName w m.good) (Tuple m.from m.to)) moves))
    in
      map
        ( \(Tuple g (Tuple a b)) ->
            let
              these = filter (\m -> displayName w m.good == g && m.from == a && m.to == b) moves
            in
              [ show x.record.round, g, a, b, quantity (Q.sumQ (map _.quantity these)), quantity (Q.sumQ (map _.worth these)) ]
        )
        keys
  -- what a unit of an Exchange's base was worth in the numeraire at its opening price
  priceOf w x = maybe Q.zero (\m -> Q.mul (Q.add m.cost.intercept (Q.mul m.cost.slope m.position)) (valueIn w m.quote)) (Map.lookup x w.exchanges)
  capitalRows (Tuple before x) =
    let
      w = x.world
      moves = concatMap
        ( \l -> case lendingCause l.cause, crossing w l of
            Just kind, Just (Tuple a b) -> [ { kind, currency: displayName w l.resource, from: a, to: b, worth: Q.mul l.moved (valueIn before l.resource), amount: l.moved } ]
            _, _ -> []
        )
        (select legs x.record.events)
      keys = sort (nub (map (\m -> [ m.kind, m.currency, m.from, m.to ]) moves))
    in
      map
        ( \k ->
            let
              these = filter (\m -> [ m.kind, m.currency, m.from, m.to ] == k) moves
            in
              [ show x.record.round ] <> k <> [ quantity (Q.sumQ (map _.amount these)), quantity (Q.sumQ (map _.worth these)) ]
        )
        keys
  lendingCause = case _ of
    OfIntent { mechanism: Direct } -> Just "lent"
    OfAgreement _ -> Just "repaid"
    _ -> Nothing
  rulesRows x =
    map
      ( \rl -> case rl of
          FlowRule f -> [ displayName x.world f.kind, show (groupOf x.world f.scope.governor), show (groupOf x.world f.scope.counterparty) ]
          HoldingRule h -> [ displayName x.world h.kind, "", "" ]
      )
      (fromFoldable (values x.world.rules))

-- | A Resource an Exchange trades that no Exchange quotes, and that is not the numeraire: a good.
isGood :: WorldState -> ResourceId -> Boolean
isGood w res = res /= w.numeraire && not (any (\m -> m.quote == res) (values w.exchanges))

goodsColumns :: Array Column
goodsColumns =
  [ column "Good" "A good: a Resource an Exchange trades that is not a currency."
  , column "From" "The country it left: an Actor group, or an Exchange's reserve."
  , column "To" "The country it reached."
  , column "Units" "How much of the good moved that way in the round."
  , column "Worth" "Its worth in the numeraire at the round's opening prices."
  ]

capitalColumns :: Array Column
capitalColumns =
  [ column "Flow" "Lent: a principal Transferred across the border. Repaid: what an agreement moved back."
  , column "Currency" "The currency a bond or an account is in: the numeraire, or a Resource an Exchange quotes."
  , column "From" "The country it left: an Actor group, or an Exchange's reserve."
  , column "To" "The country it reached."
  , column "Amount" "How much of the currency moved that way in the round."
  , column "Worth" "Its worth in the numeraire at the round's opening prices."
  ]

-- | Rounds as the spans they make up: 1, 2, 3 and 5 read "1–3, 5".
spans :: Array Int -> String
spans rounds = joinWith ", " (map (\s -> if s.from == s.to then show s.from else show s.from <> "–" <> show s.to) (foldl step [] (sort (nub rounds))))
  where
  step acc n = case unsnoc acc of
    Just { init, last } | last.to + 1 == n -> snoc init last { to = n }
    _ -> snoc acc { from: n, to: n }

roundsColumn :: Column
roundsColumn = column "Rounds in force" "The rounds the Rule stood after, as spans."

rulesColumns :: Array Column
rulesColumns =
  [ column "Rule kind" "The kind of a Rule in force."
  , column "Governor" "The country that governs the Rule."
  , column "Counterparty" "The country the Rule binds."
  ]

countryColumn :: Column
countryColumn = column "Country" "A group of Actors: an Actor and those that count as it for Rules. Everything outside it, the Exchanges' reserves included, is abroad."

positionColumn :: Column
positionColumn = column "Net external position" "The currencies the country holds other than its own, and what the rest of the world owes it less what it owes the rest of the world on open agreements, valued in the numeraire after the round. Its own currency is what its home Exchanges quote."

exchangeColumn :: Column
exchangeColumn = column "Currency exchange" "An Exchange that trades one currency for another."

rateColumn :: Column
rateColumn = column "Exchange rate" "The Exchange's price after the round: units of its quote currency for one of its base."

columns :: Array Column
columns = goodsColumns <> capitalColumns <> [ roundsColumn, exchangeColumn, rateColumn ] <> rulesColumns
