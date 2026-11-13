-- | External accounts (docs/02 §9, INV-38; docs/05 §4.6): the bonds open in a World, and each country's account in
-- | each currency for a round, which balances when every flow across its border is goods, a currency trade, a levy
-- | or interest. Generic: it reads the world's agreements, Exchanges, groups and settled Flows, and names nothing of a
-- | composition's. The invariant checks it every round, and the Finance view shows it.
module Laws.External
  ( Bond
  , bondsOf
  , currenciesOf
  , ExternalLine
  , externalLines
  , groupOf
  ) where

import Prelude

import Core.Kernel.Types (Account(..), Amount(..), Effect(..), Endpoint(..), Party(..))
import Core.Map (values)
import Core.Num (Q)
import Core.Num as Q
import Data.Array (elem, filter, fromFoldable, mapMaybe, nub, sort)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Laws.Query (holding)
import Laws.Slice (legs, select)
import Laws.Types (ActorId, ResourceId)
import Laws.World (Cause(..), Mechanism(..), PendingTransaction(..), RoundResult, WorldState, WorldTemplate(..))

-- | An open bond: a pending agreement whose one obligation moves a currency from its borrower to its lender, at a
-- | rate over its principal.
type Bond = { template :: String, lender :: ActorId, borrower :: ActorId, currency :: ResourceId, principal :: Q, rate :: Q }

-- | The bonds open in a World State: every pending agreement of one obligation moving `ratio × principal` of a
-- | Resource from one named Actor to another; and every one still in its term, which owes nothing yet and spawns such
-- | an agreement for its principal, at the rate that agreement's template sets.
bondsOf :: WorldState -> Array Bond
bondsOf w = mapMaybe one w.pending
  where
  one (PendingTransaction pt) = case pt.transaction.cause, pt.transaction.obligations of
    Just (OfAgreement { template }), [ o ] -> case o.effect, o.next.performed of
      Move { from: HeldBy (Named borrower), to: HeldBy (Named lender), resource, owed: Times { ratio: grow, amount: Exactly principal } }, _ ->
        Just { template, lender, borrower, currency: resource, principal, rate: Q.sub grow Q.one }
      Move { from: HeldBy (Named borrower), to: HeldBy (Named lender), resource, owed: Exactly z }, [ s ] | z == Q.zero ->
        case Map.lookup "principal" s.args, rateOf s.template of
          Just (Exactly principal), Just rate -> Just { template, lender, borrower, currency: resource, principal, rate }
          _, _ -> Nothing
      _, _ -> Nothing
    _, _ -> Nothing
  -- the rate a template's one obligation charges over its principal
  rateOf t = case Map.lookup t w.templates of
    Just (WorldTemplate tt) -> case tt.transaction.obligations of
      [ o ] -> case o.effect of
        Move { owed: Times { ratio } } -> Just (Q.sub ratio Q.one)
        _ -> Nothing
      _ -> Nothing
    Nothing -> Nothing

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

-- | Every country's external accounts for one round, by currency, from the world the round was played from and its
-- | result. A country is a group of Actors (an Actor with no group is its own); everything outside it, the Exchanges'
-- | reserves included, is abroad.
externalLines :: WorldState -> RoundResult -> Array ExternalLine
externalLines before x =
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
  where
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

