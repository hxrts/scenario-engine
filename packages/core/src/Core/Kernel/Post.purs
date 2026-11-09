-- | Posting (docs/02_kernel.md): the one place a settlement settled Flow is written to the ledger. A settled Flow
-- | honours what its obligation says a shortfall does, and reports what moved.
module Core.Kernel.Post
  ( Account(..)
  , Ledger
  , Opening
  , Outcome(..)
  , Posted
  , accountOf
  , crossesBoundary
  , adjust
  , available
  , ledgerByAccount
  , post
  ) where

import Prelude

import Core.Kernel.Types (Short(..), Endpoint(..), Flow)
import Core.Map (entries, sumInto, lookupQ)
import Core.Num (Q)
import Core.Num as Q
import Core.Types (ActorId, ExchangeId, ResourceId)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))

data Account = Ent ActorId | Mkt ExchangeId

derive instance Eq Account
derive instance Ord Account
derive instance Generic Account _
instance Show Account where
  show = genericShow

-- | Net changes to every account for the round, applied together at commit.
type Ledger = Map (Tuple Account ResourceId) Q

-- | What an account held when the round opened.
type Opening = Account -> ResourceId -> Q

data Outcome = PostedWhole | PostedShort

derive instance Eq Outcome
derive instance Generic Outcome _
instance Show Outcome where
  show = genericShow

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
  InExchange m -> Just (Mkt m)
  Source -> Nothing
  Sink -> Nothing

-- | Whether a settled Flow enters or leaves the economy, rather than moving between holders.
crossesBoundary :: Flow -> Boolean
crossesBoundary l = l.from == Source || l.to == Sink

-- | What an account can pay after the settlement so far, never below zero.
available :: Opening -> Ledger -> Account -> ResourceId -> Q
available opening ledger a r = max Q.zero (Q.add (opening a r) (lookupQ (Tuple a r) ledger))

-- | Post one settled Flow. `Cap` moves what is held; `Reject` moves nothing when the whole cannot move. The
-- | boundary can always pay, and so can a reserve: clearing bounds a batch's net, which a reserve can
-- | pass on the way there, and commit fails the round on a negative closing reserve. A settled Flow that moves nothing still writes its accounts: a stock law reads the
-- | ledger's keys to find what an Actor came to hold this round.
post :: Opening -> Short -> { from :: Endpoint, to :: Endpoint, resource :: ResourceId, owed :: Q } -> Ledger -> Posted
post opening short l ledger =
  { flow: { from: l.from, to: l.to, resource: l.resource, owed: l.owed, moved }
  , outcome: if moved == l.owed then PostedWhole else PostedShort
  , ledger: credit (debit ledger)
  }
  where
  held = case l.from of
    InExchange _ -> l.owed
    other -> maybe l.owed (\a -> available opening ledger a l.resource) (accountOf other)
  moved = case short of
    Cap -> min l.owed held
    Reject -> if held >= l.owed then l.owed else Q.zero
  debit = maybe identity (\a -> adjust a l.resource (Q.neg moved)) (accountOf l.from)
  credit = maybe identity (\a -> adjust a l.resource moved) (accountOf l.to)
