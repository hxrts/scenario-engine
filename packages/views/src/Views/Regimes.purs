-- | The Regimes view (docs/07 §9.2; docs/05 §3): each country's reproduction regime, round by round, in its three
-- | dimensions. Generic: a country is an Actor group; its capability is what it holds of Resources in the capacity and
-- | competence categories and what its Transformations run on them; its demand is what its Aggregates (households)
-- | receive and buy, and what its Actors sell at home and abroad; its finance is its open agreements. Every amount is
-- | valued in the numeraire.
module Views.Regimes
  ( Regime
  , regimes
  , capabilityColumns
  , demandColumns
  , financeColumns
  , columns
  ) where

import Prelude

import Core.Kernel.Types (Endpoint(..))
import Core.Map (values)

import Core.Num as Q
import Data.Array (all, any, concatMap, elem, filter, fromFoldable, last, nub, null, sort, zip)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Laws.External (bondsOf, groupOf)
import Laws.Query (holding)
import Laws.Slice (exchangeFills, legs, select, transformationFills)
import Laws.Types (ActorId, ActorKind(..), DrawSource(..), ResourceId)
import Laws.World (Cause(..), Mechanism(..), WorldState)
import Schema.Composition (capacityCategory, competenceCategory)
import Views.Common (Column, StoredRun, Table, col, column, names, quantity, worlds)
import Views.Standings (valueIn)

type Regime = { country :: String, capability :: Table, demand :: Table, finance :: Table }

-- | Every country with households, each its regime's three tables.
regimes :: StoredRun -> Array Regime
regimes r = map regimeOf countries
  where
  steps = zip (worlds r) r.rounds
  final = maybe r.manifest.initial _.world (last r.rounds)
  countries = sort (nub (map (groupOf final) (filter (isAggregate final) (fromFoldable (Map.keys final.actors)))))
  regimeOf c =
    { country: show c
    , capability: { title: "Productive capability, " <> show c, columns: names ([ col.round ] <> capabilityColumns), rows: concatMap (capabilityRows c) steps }
    , demand: { title: "Effective demand, " <> show c, columns: names ([ col.round ] <> demandColumns), rows: map (demandRow c) steps }
    , finance: { title: "Financial capacity, " <> show c, columns: names ([ col.round ] <> financeColumns), rows: map (financeRow c) steps }
    }
  inside w c a = groupOf w a == c
  members w c = filter (inside w c) (fromFoldable (Map.keys w.actors))
  -- per capacity Resource the country holds: the competence its Transformations draw beside it, and the units run
  capabilityRows c (Tuple _ x) =
    let
      w = x.world
      held res = Q.sumQ (map (\a -> holding w a res) (members w c))
      ofCategory k = filter (\res -> (Map.lookup res w.resources >>= _.category) == Just k) (fromFoldable (Map.keys w.resources))
      drawing res = filter (\t -> any (\d -> d.resource == res && d.from == Executor) t.draws) (fromFoldable (values w.transformations))
      run res = Q.sumQ (map _.quantity (filter (\f -> inside w c f.by && f.transformation `elem` map _.id (drawing res)) (select transformationFills x.record.events)))
      skillOf res = filter (\s -> any (\t -> any (\d -> d.resource == s) t.draws) (drawing res)) (ofCategory competenceCategory)
    in
      map
        ( \res ->
            [ show x.record.round
            , show res
            , quantity (held res)
            , quantity (Q.sumQ (map held (skillOf res)))
            , quantity (run res)
            , quantity (max Q.zero (Q.sub (held res) (run res)))
            ]
        )
        (filter (\res -> held res > Q.zero) (ofCategory capacityCategory))
  demandRow c (Tuple before x) =
    let
      w = x.world
      households = filter (isAggregate w) (members w c)
      toHouseholds = filter
        ( \l -> case l.to of
            Held a -> a `elem` households && not (fromInside w c l.from && isHousehold households l.from)
            _ -> false
        )
        (select legs x.record.events)
      income = Q.sumQ (map (\l -> Q.mul l.moved (valueIn before l.resource)) (filter (\l -> incomeCause l.cause) toHouseholds))
      goodsFills = filter (\f -> maybe false (\m -> isGood before m.base) (Map.lookup f.exchange before.exchanges)) (select exchangeFills x.record.events)
      worth f = maybe Q.zero (\m -> Q.mul (Q.mul f.price (Q.abs f.quantity)) (valueIn before m.quote)) (Map.lookup f.exchange before.exchanges)
      spent = Q.sumQ (map worth (filter (\f -> f.actor `elem` households && f.quantity > Q.zero) goodsFills))
      homeOnly m = not (null m.access) && all (inside w c) m.access
      sales p = Q.sumQ (map worth (filter (\f -> inside w c f.actor && f.quantity < Q.zero && maybe false p (Map.lookup f.exchange before.exchanges)) goodsFills))
    in
      [ show x.record.round, quantity income, quantity spent, quantity (Q.sub income spent), quantity (sales homeOnly), quantity (sales (not <<< homeOnly)) ]
  financeRow c (Tuple before x) =
    let
      w = x.world
      bonds = bondsOf w
      worth b = Q.mul b.principal (valueIn w b.currency)
      owedTo = Q.sumQ (map worth (filter (\b -> inside w c b.lender) bonds))
      owes = Q.sumQ (map worth (filter (\b -> inside w c b.borrower) bonds))
      paid = Q.sumQ (map (\l -> Q.mul l.moved (valueIn before l.resource)) (filter (\l -> isAgreement l.cause && fromInside w c l.from) (select legs x.record.events)))
    in
      [ show x.record.round, quantity owedTo, quantity owes, quantity paid ]
  fromInside w c = case _ of
    Held a -> inside w c a
    _ -> false
  isHousehold hs = case _ of
    Held a -> a `elem` hs
    _ -> false
  incomeCause = case _ of
    OfRule _ -> true
    OfIntent { mechanism: Direct } -> true
    OfAgreement _ -> true
    _ -> false
  isAgreement = case _ of
    OfAgreement _ -> true
    _ -> false

isAggregate :: WorldState -> ActorId -> Boolean
isAggregate w a = case Map.lookup a w.actors of
  Just x -> case x.kind of
    Aggregate _ -> true
    _ -> false
  Nothing -> false

-- | A Resource an Exchange trades that no Exchange quotes, and that is not the numeraire: a good.
isGood :: WorldState -> ResourceId -> Boolean
isGood w res = res /= w.numeraire && not (any (\m -> m.quote == res) (values w.exchanges))

capabilityColumns :: Array Column
capabilityColumns =
  [ column "Capacity resource" "A Resource in the capacity category the country's Actors hold."
  , column "Held" "Units of the capacity the country holds after the round."
  , column "Competence" "Units of the competence the Transformations drawing on this capacity draw beside it: capacity runs only as far as competence."
  , column "Run" "Units of the Transformations drawing on this capacity the country ran in the round."
  , column "Idle" "Capacity held less units run: what stood idle, still costing its upkeep."
  ]

demandColumns :: Array Column
demandColumns =
  [ column "Household income" "What the country's households received in the round from outside themselves: wages and levies paid to them, transfers and dividends, and repayments of what they lent, valued in the numeraire."
  , column "Household spending" "What they paid for goods they bought, valued alike."
  , column "Household saving" "Income less spending."
  , column "Sales at home" "What the country's Actors were paid for goods sold on Exchanges open to its own Actors alone, valued alike."
  , column "Sales abroad" "What they were paid for goods sold on every other Exchange, valued alike."
  ]

financeColumns :: Array Column
financeColumns =
  [ column "Owed to it" "The principal of open agreements whose lender is in the country, valued in the numeraire after the round."
  , column "It owes" "The principal of open agreements whose borrower is in the country, valued alike."
  , column "Debt service paid" "What the country's Actors paid on their agreements in the round, valued alike."
  ]

columns :: Array Column
columns = capabilityColumns <> demandColumns <> financeColumns
