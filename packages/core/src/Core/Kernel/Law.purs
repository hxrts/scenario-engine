-- | Laws (docs/02_type_system.md): rules over stocks and flows. A time law on a stock emits one leg per
-- | holder, written with the holder as a bound party.
module Core.Kernel.Law
  ( holderParam
  , forHolder
  ) where

import Prelude

import Core.Kernel.Types (Account(..), Amount(..), Coupling, Effect(..), Law, Leg, Operation(..), Party(..), Short(..), noNext)
import Core.Types (EntityId)
import Data.Maybe (Maybe(..))

-- | The name a stock law's leg gives the holder it applies to.
holderParam :: String
holderParam = "holder"

-- | What a stock law does to one holder, as a coupling. A law that emits no leg has none.
forHolder :: forall cap law query reason cause. EntityId -> Law cap query reason cause -> Maybe (Coupling cap law query reason cause)
forHolder holder law = case law.operation of
  Emit l -> Just
    { obligations: [ { id: "law", obligor: Named holder, effect: Move (leg l), when: law.guard, onShort: Cap, next: noNext } ]
    , holds: []
    , solver: Nothing
    , ration: { tier: 1, whole: false, scaleGroup: Nothing }
    , phase: law.phase
    , cause: Just law.cause
    , consented: []
    }
  _ -> Nothing
  where
  party = case _ of
    Bound p | p == holderParam -> Named holder
    other -> other
  account = case _ of
    HeldBy p -> HeldBy (party p)
    other -> other

  leg :: Leg query -> Leg query
  leg l = l { from = account l.from, to = account l.to, owed = amount l.owed }

  amount :: Amount query -> Amount query
  amount = case _ of
    Stock s -> Stock s { at = account s.at }
    Times t -> Times t { amount = amount t.amount }
    Plus p -> Plus { left: amount p.left, right: amount p.right }
    Minus p -> Minus { left: amount p.left, right: amount p.right }
    Least p -> Least { left: amount p.left, right: amount p.right }
    ShareOf s -> ShareOf s { limit = amount s.limit, among = map amount s.among }
    other -> other
