-- | One Actor's balanceSheet (docs/07_simulator.md §9): what it has done, what it has available, and what it can do,
-- | scoped to that Actor. It names another Actor only where this one was party to a Transaction with it. Pure.
module Views.BalanceSheet
  ( BalanceSheet
  , BalanceSheetLink
  , balanceSheetLinks
  , linksOf
  , balanceSheetAt
  ) where

import Prelude

import Core.Draw (tokenFor)
import Core.Num (Kind(..))
import Core.Kernel.Consume (partiesOf)
import Core.Exchange (exchangePrice)
import Core.Map as CoreMap
import Data.Array (concatMap, elem, filter, find, last, mapMaybe, nub)
import Data.Either (hush)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Laws.Slice (event, select)
import Laws.Types (ActorId, Actor, ActorKind(..), Capability(..), Intent, IntentF(..), RuleChangeF(..), idText)
import Laws.World (WorldState, renderReason)
import Core.Kernel.Types (Party(..))
import Views.Actor (actorHistory)
import Views.Common (Chart, StoredRun, Table, quantity, series, unitOf, worlds)

type BalanceSheet =
  { actor :: String
  , kind :: String
  , done :: Table
  , holdings :: Table
  , chart :: Chart
  , netWorth :: Chart
  , owed :: Table
  , capabilities :: Table
  , exchanges :: Table
  , transformations :: Table
  , prices :: Chart
  , objective :: Table
  }

type BalanceSheetLink = { actor :: String, kind :: String, path :: String }

seedOf :: StoredRun -> Int
seedOf r = fromMaybe 0 (Int.fromString r.playerSeed)

tokenOf :: StoredRun -> ActorId -> String
tokenOf r a = tokenFor (seedOf r) (idText a)

kindOf :: Actor -> String
kindOf a = case a.kind of
  PlayableActor _ -> "Playable Actor"
  NonPlayableActor _ -> "Non-playable Actor"
  _ -> "Aggregate"

-- | Every Actor's balanceSheet path, for the admin page and for handing out at the start of a game.
balanceSheetLinks :: StoredRun -> Array BalanceSheetLink
balanceSheetLinks r = linksOf r.id (seedOf r) r.manifest.initial.actors

-- | The balanceSheet paths of a Run's Actors, from its id, player seed and Actors.
linksOf :: String -> Int -> Map.Map ActorId Actor -> Array BalanceSheetLink
linksOf id seed actors = map link (CoreMap.values actors)
  where
  link a = { actor: idText a.id, kind: kindOf a, path: "/balancesheet/" <> id <> "/" <> tokenFor seed (idText a.id) }

-- | The balanceSheet a token opens; nothing when it opens none. `objective` is the Run's objectives table.
balanceSheetAt :: StoredRun -> Table -> String -> Maybe BalanceSheet
balanceSheetAt r objectives token = do
  a <- find (\x -> tokenOf r x.id == token) (CoreMap.values r.manifest.initial.actors)
  history <- hush (actorHistory r a.id)
  now <- last (worlds r)
  held <- Map.lookup a.id now.actors
  pure
    { actor: idText a.id
    , kind: kindOf a
    , done: doneTable r a.id
    , holdings: history.table
    , chart: history.chart
    , netWorth: history.netWorth
    , owed: owedTable now a.id
    , capabilities: { title: "What it may do", columns: [ "Capability" ], rows: map (\c -> [ capabilityText c ]) held.capabilities }
    , exchanges: exchangeTable now a.id held
    , transformations: transformationTable now held
    , prices: priceChart r a.id
    , objective: objectives { rows = filter (\row -> row `startsWith` idText a.id) objectives.rows }
    }

startsWith :: Array String -> String -> Boolean
startsWith row x = case row of
  [] -> false
  _ -> x `elem` row

capabilityText :: Capability -> String
capabilityText = case _ of
  MayTrade m -> "Trade on " <> idText m
  MayTransfer r -> "Transfer " <> idText r
  MayTransform t -> "Run " <> idText t
  MayEnact k -> "Enact " <> idText k

-- | One row per Transaction the Actor proposed or was asked to sign, by round, with what became of it.
doneTable :: StoredRun -> ActorId -> Table
doneTable r a =
  { title: "What it has done, by round"
  , columns: [ "Round", "Role", "Action", "With", "Outcome" ]
  , rows: concatMap round r.rounds
  }
  where
  round x = mapMaybe (row x) x.record.inputs
  row x input
    | input.candidateTransaction.author == a || a `elem` input.candidateTransaction.signers =
        let
          c = input.candidateTransaction
          others = filter (_ /= a) (nub ([ c.author ] <> c.signers))
        in
          Just
            [ show x.record.round
            , if c.author == a then "Proposed" else "Asked to sign"
            , joinWith "; " (map intentText c.intents)
            , joinWith ", " (map idText others)
            , outcome x.record.events c.id
            ]
    | otherwise = Nothing
  outcome events id =
    case find (\e -> e.candidateTransaction == id) (select (event @"Rejected") events) of
      Just e -> "Rejected: " <> renderReason e.reason
      Nothing
        | id `elem` map _.candidateTransaction (select (event @"Lapsed") events) -> "Lapsed"
        | otherwise -> "Went through"

intentText :: Intent -> String
intentText = case _ of
  Trade o -> "Trade " <> quantity o.quantity <> " on " <> idText o.exchange
  Route o -> "Route " <> quantity o.quantity <> " of " <> idText o.pair.base
  Transfer o -> "Send " <> quantity o.quantity <> " " <> idText o.resource <> " to " <> idText o.to
  Transform o -> "Run " <> idText o.transformation <> " x " <> quantity o.quantity
  ChangeRule (Enact _) -> "Enact a Rule"
  ChangeRule (Repeal id) -> "Repeal " <> idText id
  Enter e -> "Enter the agreement " <> e.template

-- | What it owes or is owed in pending Transactions.
owedTable :: WorldState -> ActorId -> Table
owedTable w a =
  { title: "What it owes or is owed"
  , columns: [ "Due round", "Obligation", "Owed by" ]
  , rows: concatMap one w.pending
  }
  where
  one p =
    let
      x = unwrap p
    in
      if a `elem` partiesOf x.transaction then map (rowOf x.due) x.transaction.obligations else []
  rowOf due o = [ show due, o.id, party o.obligor ]
  party = case _ of
    Named e -> idText e
    Bound p -> p

exchangeTable :: WorldState -> ActorId -> Actor -> Table
exchangeTable w a held =
  { title: "Exchanges it can use"
  , columns: [ "Exchange", "Buys", "With", "Price" ]
  , rows: mapMaybe row (CoreMap.values w.exchanges)
  }
  where
  row m
    | a `elem` m.access && MayTrade m.id `elem` held.capabilities = Just [ idText m.id, idText m.base, idText m.quote, quantity (exchangePrice m) ]
    | otherwise = Nothing

transformationTable :: WorldState -> Actor -> Table
transformationTable w held =
  { title: "Transformations it can run"
  , columns: [ "Transformation", "Smallest order", "Largest order" ]
  , rows: mapMaybe row (CoreMap.values w.transformations)
  }
  where
  row t
    | MayTransform t.id `elem` held.capabilities = Just [ idText t.id, quantity t.minimum, maybe "" quantity t.maximum ]
    | otherwise = Nothing

priceChart :: StoredRun -> ActorId -> Chart
priceChart r a =
  { title: "Prices on its Exchanges"
  , unit: unitOf r r.manifest.initial.numeraire
  , kind: Price
  , series: series r \w -> mapMaybe (\m -> if a `elem` m.access then Just (Tuple (idText m.id) (exchangePrice m)) else Nothing) (CoreMap.values w.exchanges)
  }

