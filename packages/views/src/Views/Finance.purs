-- | The Finance view (docs/07_simulator.md §9.3; docs/05_pacific.md §7): the bonds open after each round, and each
-- | country's external accounts. A country is a group of Actors (an Actor with no group is its own); everything
-- | outside it, the Exchanges' reserves included, is abroad. Generic: it reads the world's agreements, Exchanges and
-- | settled Flows, and names nothing of a composition's.
module Views.Finance
  ( finance
  , externalAccounts
  , columns
  , Earnings
  , earningsOf
  ) where

import Prelude

import Core.Kernel.Types (Endpoint(..))

import Core.Num (Q)
import Core.Num as Q
import Data.Array (concatMap, elem, filter, mapMaybe, nub, sort, zip)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import Laws.Slice (exchangeFills, legs, select)
import Laws.Types (ActorId)
import Laws.External (ExternalLine, bondsOf, currenciesOf, externalLines)
import Laws.World (Cause(..))
import Views.Common (Column, StoredRun, Table, col, column, names, quantity, ratio, worlds)
import Views.Standings (valueIn)

-- | Every country's external accounts, by round and currency.
externalAccounts :: StoredRun -> Array ExternalLine
externalAccounts r = concatMap (\(Tuple before x) -> externalLines before x) (zip (worlds r) r.rounds)

countryColumn :: Column
countryColumn = column "Country" "A group of Actors: an Actor and those that count as it for Rules. Everything outside it, the Exchanges' reserves included, is abroad."

currencyColumn :: Column
currencyColumn = column "Currency" "The currency a bond or an account is in: the numeraire, or a Resource an Exchange quotes."

bondColumns :: Array Column
bondColumns =
  [ column "Template" "The bond's agreement template."
  , column "Lender" "The Actor owed: it Transferred the principal."
  , column "Borrower" "The Actor that owes, and repays the next round."
  , currencyColumn
  , column "Principal" "What was lent, or carried from a bond not repaid in full."
  , column "Rate" "The interest a round, over the principal."
  ]

externalColumns :: Array Column
externalColumns =
  [ countryColumn
  , currencyColumn
  , column "Opening" "What the country held of the currency and was owed in it by the rest of the world, less what it owed, before the round: bonds at their principal."
  , column "Goods" "Paid to it for goods it sold abroad, less paid for goods it bought from abroad."
  , column "Currency trades" "Bought on the currency Exchanges, less sold."
  , column "Levies" "Levies received from abroad, less levies paid abroad."
  , column "Interest" "Interest received from abroad, less interest paid: lending across the border plus the change in its bonds."
  , column "Closing" "The position after the round."
  , column "Residual" "Closing less opening less the four flows: what crossed the border as something else. Zero when every flow is accounted."
  ]

-- | What a producer kept of a round's sales, every amount valued in the numeraire at the round's opening prices: its
-- | revenue from goods sold, less the levies and wages it paid (every Flow a Rule charged it), less its debt service
-- | (what it paid on its agreements).
type Earnings = { round :: Int, actor :: ActorId, revenue :: Q, charges :: Q, debtService :: Q, retained :: Q }

-- | The earnings of every Actor that sold on the Exchanges, by round.
earningsOf :: StoredRun -> Array Earnings
earningsOf r = concatMap round (zip (worlds r) r.rounds)
  where
  round (Tuple before x) =
    let
      value m = valueIn before m
      isCurrency res = res `elem` currenciesOf before
      paidBy a l = case l.from of
        Held b -> b == a
        _ -> false
      ls = select legs x.record.events
      seats = filter (\a -> Map.member a r.manifest.initial.actors) (sort (nub (map _.actor (select exchangeFills x.record.events))))
    in
      map
        ( \a ->
            let
              revenue = Q.sumQ
                ( mapMaybe
                    ( \f -> case Map.lookup f.exchange before.exchanges of
                        Just m | f.actor == a && f.quantity < Q.zero && not (isCurrency m.base) -> Just (Q.mul (Q.mul (Q.neg f.quantity) f.price) (value m.quote))
                        _ -> Nothing
                    )
                    (select exchangeFills x.record.events)
                )
              charged p = Q.sumQ (map (\l -> Q.mul l.moved (value l.resource)) (filter (\l -> paidBy a l && p l.cause) ls))
              charges = charged \c -> case c of
                OfRule _ -> true
                _ -> false
              debtService = charged \c -> case c of
                OfAgreement _ -> true
                _ -> false
            in
              { round: x.record.round, actor: a, revenue, charges, debtService, retained: Q.sub revenue (Q.add charges debtService) }
        )
        seats

earningsColumns :: Array Column
earningsColumns =
  [ column "Seller" "An Actor that sold on the Exchanges in the round."
  , column "Revenue" "What it was paid for goods it sold, valued in the numeraire at the round's opening prices."
  , column "Levies and wages" "What every Rule charged it: wages, upkeep, tariffs, taxes and fees, valued alike."
  , column "Debt service" "What it paid on its agreements: principal and interest, valued alike."
  , column "Retained" "Revenue less levies and wages, less debt service: what the round's sales left it."
  ]

-- | The columns this module defines.
columns :: Array Column
columns = bondColumns <> externalColumns <> currentColumns <> earningsColumns

currentColumns :: Array Column
currentColumns =
  [ column "Current account" "Goods, levies and interest across the border, every currency valued in the numeraire at the round's closing prices."
  , column "Financial account" "Currency trades across the border, valued alike: what the country moved between currencies."
  , column "Valuation" "The change in its opening position from the currencies' values moving over the round."
  , column "Change in position" "Its position in the numeraire after the round, less before: the three columns' sum."
  ]

-- | The view: the bonds open after each round, and the external accounts, with the current account in the numeraire.
finance :: StoredRun -> { bonds :: Table, external :: Table, current :: Table, earnings :: Table }
finance r =
  { bonds:
      { title: "Open bonds"
      , columns: names ([ col.round ] <> bondColumns)
      , rows: concatMap (\x -> map (\b -> [ show x.record.round, b.template, show b.lender, show b.borrower, show b.currency, quantity b.principal, ratio b.rate ]) (bondsOf x.world)) r.rounds
      }
  , external:
      { title: "External accounts"
      , columns: names ([ col.round ] <> externalColumns)
      , rows: map (\l -> [ show l.round, show l.country, show l.currency, quantity l.opening, quantity l.goods, quantity l.exchange, quantity l.levies, quantity l.interest, quantity l.closing, quantity l.residual ]) lines
      }
  , earnings:
      { title: "Earnings"
      , columns: names ([ col.round ] <> earningsColumns)
      , rows: map (\e -> [ show e.round, show e.actor, quantity e.revenue, quantity e.charges, quantity e.debtService, quantity e.retained ]) (earningsOf r)
      }
  , current:
      { title: "Current account"
      , columns: names ([ col.round, countryColumn ] <> currentColumns)
      , rows: concatMap currentRows (zip (worlds r) r.rounds)
      }
  }
  where
  lines = externalAccounts r
  currentRows (Tuple before x) =
    let
      w = x.world
      mine c = filter (\l -> l.round == x.record.round && l.country == c) lines
      value m = valueIn w m
      start m = valueIn before m
      countries = nub (map _.country (filter (\l -> l.round == x.record.round) lines))
    in
      map
        ( \c ->
            let
              ls = mine c
              ca = Q.sumQ (map (\l -> Q.mul (value l.currency) (Q.sumQ [ l.goods, l.levies, l.interest ])) ls)
              fa = Q.sumQ (map (\l -> Q.mul (value l.currency) l.exchange) ls)
              valuation = Q.sumQ (map (\l -> Q.mul l.opening (Q.sub (value l.currency) (start l.currency))) ls)
              change = Q.sub (Q.sumQ (map (\l -> Q.mul (value l.currency) l.closing) ls)) (Q.sumQ (map (\l -> Q.mul (start l.currency) l.opening) ls))
            in
              [ show x.record.round, show c, quantity ca, quantity fa, quantity valuation, quantity change ]
        )
        countries
