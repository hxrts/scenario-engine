-- | The Finance view (docs/07_simulator.md §9.2; docs/05_pacific.md §9.3): the bonds open after each round, and each
-- | country's external accounts. A country is a group of Actors (an Actor with no group is its own); everything
-- | outside it, the Exchanges' reserves included, is abroad. Generic: it reads the world's agreements, Exchanges and
-- | settled Flows, and names nothing of a composition's.
module Views.Finance
  ( finance
  , Bond
  , bondsOf
  , ExternalLine
  , externalAccounts
  , currenciesOf
  , columns
  ) where

import Prelude

import Core.Kernel.Types (Account(..), Amount(..), Effect(..), Endpoint(..), Party(..))
import Core.Map (values)
import Core.Num (Q)
import Core.Num as Q
import Data.Array (concatMap, elem, filter, fromFoldable, head, mapMaybe, nub, sort, zip)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Laws.Query (holding)
import Laws.Slice (legs, select)
import Laws.Types (ActorId, ResourceId)
import Laws.World (Cause(..), Mechanism(..), PendingTransaction(..), WorldState)
import Views.Common (Column, StoredRun, Table, col, column, names, quantity, ratio, worlds)
import Views.Standings (valueIn)

-- | An open bond: a pending agreement whose one obligation moves a currency from its borrower to its lender, at a
-- | rate over its principal.
type Bond = { template :: String, lender :: ActorId, borrower :: ActorId, currency :: ResourceId, principal :: Q, rate :: Q }

-- | The bonds open in a World State: every pending agreement of one obligation moving `ratio × principal` of a
-- | Resource from one named Actor to another.
bondsOf :: WorldState -> Array Bond
bondsOf w = mapMaybe one w.pending
  where
  one (PendingTransaction pt) = case pt.transaction.cause, head pt.transaction.obligations of
    Just (OfAgreement { template }), Just o | [ _ ] <- pt.transaction.obligations -> case o.effect of
      Move { from: HeldBy (Named borrower), to: HeldBy (Named lender), resource, owed: Times { ratio: grow, amount: Exactly principal } } ->
        Just { template, lender, borrower, currency: resource, principal, rate: Q.sub grow Q.one }
      _ -> Nothing
    _, _ -> Nothing

-- | The currencies of a world: its numeraire and every Resource an Exchange quotes.
currenciesOf :: WorldState -> Array ResourceId
currenciesOf w = sort (nub ([ w.numeraire ] <> map _.quote (values w.exchanges)))

-- | One country's account in one currency for one round. Its position is what the country holds of the currency
-- | and is owed in it by the rest of the world, less what it owes the rest of the world: bonds at their principal.
-- | It changes by the round's flows across the border: payment for goods, currency bought or sold on the currency
-- | Exchanges, levies, and lending (principal Transferred and bonds repaid); `interest` is lending plus the change
-- | in the bonds, so a loan made is not income and a bond repaid with interest is. Whatever crossed the border as
-- | something else is the residual, which is zero when every flow is accounted.
type ExternalLine =
  { round :: Int
  , country :: ActorId
  , currency :: ResourceId
  , opening :: Q
  , goods :: Q
  , exchange :: Q
  , levies :: Q
  , lending :: Q
  , interest :: Q
  , closing :: Q
  , residual :: Q
  }

-- | Every country's external accounts, by round and currency.
externalAccounts :: StoredRun -> Array ExternalLine
externalAccounts r = concatMap round (zip (worlds r) r.rounds)
  where
  round (Tuple before x) =
    let
      w = x.world
      countries = sort (nub (map (groupOf w) (fromFoldable (Map.keys w.actors))))
    in
      do
        c <- countries
        m <- currenciesOf w
        let
          inC a = groupOf w a == c
          inside = case _ of
            Held a -> inC a
            _ -> false
          crossing = filter (\l -> l.resource == m && inside l.from /= inside l.to) (select legs x.record.events)
          signed l = if inside l.to then l.moved else Q.neg l.moved
          sumOf p = Q.sumQ (map signed (filter p crossing))
          isCurrency res = res `elem` currenciesOf w
          goods = sumOf \l -> case l.cause of
            OfMatch { exchange } -> maybe' (\e -> not (isCurrency e.base)) (Map.lookup exchange w.exchanges)
            _ -> false
          traded = sumOf \l -> case l.cause of
            OfMatch { exchange: e } -> maybe' (\v -> isCurrency v.base) (Map.lookup e w.exchanges)
            _ -> false
          levies = sumOf \l -> case l.cause of
            OfRule _ -> true
            _ -> false
          lending = sumOf \l -> case l.cause of
            OfAgreement _ -> true
            OfIntent { mechanism: Direct } -> true
            _ -> false
          opening = position before inC m
          closing = position w inC m
          claims ww = Q.sub (owedTo ww inC m) (owedBy ww inC m)
          interest = Q.add lending (Q.sub (claims w) (claims before))
        pure
          { round: x.record.round
          , country: c
          , currency: m
          , opening
          , goods
          , exchange: traded
          , levies
          , lending
          , interest
          , closing
          , residual: Q.sub (Q.sub closing opening) (Q.sumQ [ goods, traded, levies, interest ])
          }
  maybe' f = fromMaybe false <<< map f

-- | The group an Actor counts as, for Rules and here: its country.
groupOf :: WorldState -> ActorId -> ActorId
groupOf w a = fromMaybe a (Map.lookup a w.groups)

-- | What a country holds of a currency and is owed in it, less what it owes, the rest of the world.
position :: WorldState -> (ActorId -> Boolean) -> ResourceId -> Q
position w inC m = Q.add (Q.sumQ (map (\a -> holding w a m) (filter inC (fromFoldable (Map.keys w.actors))))) (Q.sub (owedTo w inC m) (owedBy w inC m))

-- | The principal of the bonds in a currency the rest of the world owes the country, and the country owes it.
owedTo :: WorldState -> (ActorId -> Boolean) -> ResourceId -> Q
owedTo w inC m = Q.sumQ (map _.principal (filter (\b -> b.currency == m && inC b.lender && not (inC b.borrower)) (bondsOf w)))

owedBy :: WorldState -> (ActorId -> Boolean) -> ResourceId -> Q
owedBy w inC m = Q.sumQ (map _.principal (filter (\b -> b.currency == m && inC b.borrower && not (inC b.lender)) (bondsOf w)))

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

-- | The columns this module defines.
columns :: Array Column
columns = bondColumns <> externalColumns <> currentColumns

currentColumns :: Array Column
currentColumns =
  [ column "Current account" "Goods, levies and interest across the border, every currency valued in the numeraire at the round's closing prices."
  , column "Financial account" "Currency trades across the border, valued alike: what the country moved between currencies."
  , column "Valuation" "The change in its opening position from the currencies' values moving over the round."
  , column "Change in position" "Its position in the numeraire after the round, less before: the three columns' sum."
  ]

-- | The view: the bonds open after each round, and the external accounts, with the current account in the numeraire.
finance :: StoredRun -> { bonds :: Table, external :: Table, current :: Table }
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
