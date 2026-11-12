-- | Derived indicators (docs/05 §4.4; docs/07 §9.2): what a run's record says about each country and each seat, read
-- | from production, purchases, agreements and positions and never stored. Generic: a country is an Actor group, its
-- | households are the Aggregates in it, its goods are what its Transformations deliver, and its debts are its open
-- | agreements (`Laws.External`).
-- |
-- | - **Net external position**: a country's position abroad in every currency, valued in the numeraire.
-- | - **Domestic absorption**: the share of a country's output of a good its own households bought.
-- | - **Debt service**: what a seller owed on its agreements in a round, over its revenue from goods.
-- | - **Viability margin**: how far each every-round criterion cleared its bound, in its worst round so far.
module Views.Indicators
  ( CountryIndicators
  , countryIndicators
  , SeatIndicators
  , seatIndicators
  , indicators
  , countryColumns
  , seatColumns
  , columns
  ) where

import Prelude

import Checks.Objectives (evaluateObjectives)
import Core.Kernel.Types (Endpoint(..))
import Core.Map (values)
import Core.Num (Q)
import Core.Num as Q
import Data.Array (all, any, concatMap, elem, filter, fromFoldable, mapMaybe, nub, null, sort, take, zipWith)
import Data.String (joinWith)
import Data.Foldable (minimum)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)

import Laws.External (bondsOf, currenciesOf, groupOf)
import Laws.Query (holding)
import Laws.Slice (exchangeFills, legs, select, transformationFills)
import Laws.Types (ActorId, ActorKind(..), ResourceId, TransformationId)
import Laws.World (Cause(..), WorldState)
import Schema.Composition (PremiseComposition, Span(..))
import Views.Common (Column, StoredRun, Table, col, column, names, quantity, ratio)
import Views.Finance (earningsOf)
import Views.Standings (valueIn)

-- | A country's indicators for a round: its net external position, and the share of its output of each good its own
-- | households bought (`Nothing` when it made none of it).
type CountryIndicators = { round :: Int, country :: ActorId, netExternal :: Q, absorption :: Array { good :: ResourceId, share :: Maybe Q } }

countryIndicators :: StoredRun -> Array CountryIndicators
countryIndicators r = concatMap round r.rounds
  where
  round x =
    let
      w = x.world
      countries = sort (nub (map (groupOf w) (fromFoldable (Map.keys w.actors))))
    in
      map
        ( \c ->
            let
              inC a = groupOf w a == c
              households = filter (\a -> inC a && isAggregate w a) (fromFoldable (Map.keys w.actors))
              made g = Q.sumQ (map (\f -> Q.mul f.quantity (perUnit w g f.transformation)) (filter (\f -> inC f.by) (select transformationFills x.record.events)))
              bought g = Q.sumQ (map _.quantity (filter (\f -> f.actor `elem` households && f.resource == g && f.quantity > Q.zero) (select exchangeFills x.record.events)))
              goods = filter (\g -> made g > Q.zero) (goodsOf w)
            in
              { round: x.record.round
              , country: c
              , netExternal: netExternal w inC
              , absorption: map (\g -> { good: g, share: Q.divMaybe (bought g) (made g) }) goods
              }
        )
        (filter (\c -> any (\a -> isAggregate w a && groupOf w a == c) (fromFoldable (Map.keys w.actors))) countries)

-- | A country's net external position in a World: the currencies it holds other than its own, and what the rest of
-- | the world owes it less what it owes the rest of the world on open agreements, every currency valued in the
-- | numeraire. Its own currency is what its home Exchanges quote: those open to its Actors alone.
netExternal :: WorldState -> (ActorId -> Boolean) -> Q
netExternal w inC = Q.sumQ (map one (currenciesOf w))
  where
  members = filter inC (fromFoldable (Map.keys w.actors))
  home = nub (map _.quote (filter (\x -> not (null x.access) && all inC x.access) (values w.exchanges)))
  bonds = bondsOf w
  one m =
    let
      held = if m `elem` home then Q.zero else Q.sumQ (map (\a -> holding w a m) members)
      owedTo = Q.sumQ (map _.principal (filter (\b -> b.currency == m && inC b.lender && not (inC b.borrower)) bonds))
      owedBy = Q.sumQ (map _.principal (filter (\b -> b.currency == m && inC b.borrower && not (inC b.lender)) bonds))
    in
      Q.mul (valueIn w m) (Q.add held (Q.sub owedTo owedBy))

-- | A seat's indicators for a round: what it owed on its agreements over its revenue from goods, and the margin of
-- | each of its every-round criteria in its worst round so far.
type SeatIndicators = { round :: Int, actor :: ActorId, debtService :: Maybe Q, margins :: Array Q }

seatIndicators :: PremiseComposition -> StoredRun -> Array SeatIndicators
seatIndicators gb r = concatMap round r.rounds
  where
  earnings = earningsOf r
  objectives = evaluateObjectives gb r.rounds
  everyRound actor = case Map.lookup actor gb.explicitActors >>= _.objective of
    Just o -> map (\c -> c.over == EveryRound) o.criteria
    Nothing -> []
  margins actor k = case filter (\o -> o.explicitActor == actor) objectives of
    [ o ] -> mapMaybe identity (zipWith (\isEvery c -> if isEvery then minimum (take k (zipWith Q.sub c.values c.bound)) else Nothing) (everyRound actor) o.criteria)
    _ -> []
  round x =
    map
      ( \actor ->
          let
            owed = Q.sumQ (map _.owed (filter (\l -> l.from == Held actor && isAgreement l.cause) (select legs x.record.events)))
            revenue = Q.sumQ (map _.revenue (filter (\e -> e.round == x.record.round && e.actor == actor) earnings))
          in
            { round: x.record.round, actor, debtService: if owed == Q.zero then Just Q.zero else Q.divMaybe owed revenue, margins: margins actor x.record.round }
      )
      (sort (fromFoldable (Map.keys gb.explicitActors)))
  isAgreement = case _ of
    OfAgreement _ -> true
    _ -> false

isAggregate :: WorldState -> ActorId -> Boolean
isAggregate w a = case Map.lookup a w.actors of
  Just x -> case x.kind of
    Aggregate _ -> true
    _ -> false
  Nothing -> false

-- | The goods of a world: every Resource an Exchange trades that is not a currency.
goodsOf :: WorldState -> Array ResourceId
goodsOf w = filter (\g -> not (g `elem` currenciesOf w)) (sort (nub (map _.base (values w.exchanges))))

-- | What one unit of a Transformation delivers of a Resource.
perUnit :: WorldState -> ResourceId -> TransformationId -> Q
perUnit w g t = maybe Q.zero (\x -> Q.sumQ (map _.perUnit (filter (\e -> e.resource == g) x.effects))) (Map.lookup t w.transformations)

countryColumns :: Array Column
countryColumns =
  [ column "Net external position" "The currencies the country holds other than its own, and what the rest of the world owes it less what it owes the rest of the world on open agreements, valued in the numeraire after the round. Its own currency is what its home Exchanges quote."
  , column "Domestic absorption" "For each good the country made in the round, the share its own households bought: low when its output depends on demand abroad."
  ]

seatColumns :: Array Column
seatColumns =
  [ column "Debt service" "What the seat owed on its agreements in the round, over its revenue from goods: above 1, it cannot pay from its sales."
  , column "Viability margin" "For each criterion the seat must meet every round, its measure less its bound in its worst round so far: below 0, it has failed."
  ]

columns :: Array Column
columns = countryColumns <> seatColumns

-- | The view: country indicators and seat indicators, by round.
indicators :: PremiseComposition -> StoredRun -> { countries :: Table, seats :: Table }
indicators gb r =
  { countries:
      { title: "Country indicators"
      , columns: names ([ col.round, countryColumn ] <> countryColumns)
      , rows: map (\c -> [ show c.round, show c.country, quantity c.netExternal, absorptionCell c.absorption ]) (countryIndicators r)
      }
  , seats:
      { title: "Seat indicators"
      , columns: names ([ col.round, col.explicitActor ] <> seatColumns)
      , rows: map (\s -> [ show s.round, show s.actor, maybe "n/a" ratio s.debtService, marginsCell s.margins ]) (seatIndicators gb r)
      }
  }
  where
  countryColumn = column "Country" "A group of Actors: an Actor and those that count as it for Rules. Everything outside it, the Exchanges' reserves included, is abroad."
  absorptionCell xs = listed (map (\a -> show a.good <> " " <> maybe "n/a" ratio a.share) xs)
  marginsCell xs = listed (map quantity xs)
  listed xs = if null xs then "none" else joinWith ", " xs
