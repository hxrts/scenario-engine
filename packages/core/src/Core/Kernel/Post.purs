-- | Posting (docs/02_type_system.md): the one place a settlement leg is written to the ledger. A leg
-- | honours what its obligation says a shortfall does, and reports what moved.
module Core.Kernel.Post
  ( Account(..)
  , Ledger
  , Opening
  , Outcome(..)
  , Posted
  , accountOf
  , adjust
  , available
  , ledgerByAccount
  , post
  ) where

import Prelude

import Core.Kernel.Types (Short(..))
import Core.Map (entries, sumInto)
import Core.Num (Q)
import Core.Num as Q
import Core.Query (lookupQ)
import Core.Types (EntityId, MarketId, ResourceId)
import Core.World (Endpoint(..), Flow)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))

data Account = Ent EntityId | Mkt MarketId

derive instance Eq Account
derive instance Ord Account

-- | Net changes to every account for the round, applied together at commit.
type Ledger = Map (Tuple Account ResourceId) Q

-- | What an account held when the round opened.
type Opening = Account -> ResourceId -> Q

data Outcome = PostedWhole | PostedShort

derive instance Eq Outcome
instance Show Outcome where
  show PostedWhole = "PostedWhole"
  show PostedShort = "PostedShort"

type Posted = { flow :: Flow, outcome :: Outcome, ledger :: Ledger }

-- | The ledger grouped by Account, in one pass: each Account's changes, keyed by Resource.
ledgerByAccount :: Ledger -> Map Account (Map ResourceId Q)
ledgerByAccount ledger = Map.fromFoldableWith Map.union (map (\(Tuple (Tuple a r) q) -> Tuple a (Map.singleton r q)) (entries ledger))

adjust :: Account -> ResourceId -> Q -> Ledger -> Ledger
adjust a r = sumInto (Tuple a r)

-- | The account behind an endpoint. The economy's boundary has none.
accountOf :: Endpoint -> Maybe Account
accountOf = case _ of
  Held e -> Just (Ent e)
  InMarket m -> Just (Mkt m)
  Source -> Nothing
  Sink -> Nothing

-- | What an account can pay after the settlement so far, never below zero.
available :: Opening -> Ledger -> Account -> ResourceId -> Q
available opening ledger a r = max Q.zero (Q.add (opening a r) (lookupQ (Tuple a r) ledger))

-- | Post one leg. `Cap` moves what is held; `Reject` moves nothing when the whole cannot move. The
-- | boundary can always pay. A leg that moves nothing still writes its accounts: a stock law reads the
-- | ledger's keys to find what an Entity came to hold this round.
post :: Opening -> Short -> { from :: Endpoint, to :: Endpoint, resource :: ResourceId, owed :: Q } -> Ledger -> Posted
post opening short l ledger =
  { flow: { from: l.from, to: l.to, resource: l.resource, owed: l.owed, moved }
  , outcome: if moved == l.owed then PostedWhole else PostedShort
  , ledger: credit (debit ledger)
  }
  where
  held = maybe l.owed (\a -> available opening ledger a l.resource) (accountOf l.from)
  moved = case short of
    Cap -> min l.owed held
    Reject -> if held >= l.owed then l.owed else Q.zero
  debit = maybe identity (\a -> adjust a l.resource (Q.neg moved)) (accountOf l.from)
  credit = maybe identity (\a -> adjust a l.resource moved) (accountOf l.to)
