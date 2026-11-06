-- | Engine phase 7 (docs/02_type_system.md §8.1 step 7) and the round driver. `resolveRound` runs
-- | phases 1–7 in order: prepare, screen, fund, settle, circulate, then estimate and commit. Commit
-- | applies the Settlement's ledger to every Entity and Market at once and fails the round if any
-- | Account would go negative. That check is an invariant assertion: settlement and escrow should
-- | make it unreachable, and a failure rejects the round rather than recording a corrupt state.
module Core.Engine.Commit
  ( EngineError(..)
  , resolveRound
  ) where

import Prelude

import Core.Engine.Circulate (circulate)
import Core.Engine.Fund (fund)
import Core.Engine.Prepare (prepareRound)
import Core.Engine.Screen (duplicatedIds, screen)
import Core.Engine.Settle (Account(..), Settlement, settle)
import Core.Num (Q)
import Core.Num as Q
import Core.Represent as Represent
import Core.Types (BehaviorDomain, Entity, EntityId, Market, MarketId, Ratio, ResourceId, SignedProposal)
import Core.World (Event, RoundResult, WorldState)
import Data.Array (concatMap, find, foldl, last, sortWith)
import Data.Either (Either(..), either)
import Data.Int (fromString) as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

data EngineError
  = UnknownReference String
  | NegativeBalance EntityId ResourceId -- an invariant failure: settlement should make this unreachable
  | NegativeReserve MarketId ResourceId -- likewise, for Market reserves

derive instance Eq EngineError
instance Show EngineError where
  show (UnknownReference r) = "unknown reference: " <> r
  show (NegativeBalance e r) = "negative balance: " <> show e <> " " <> show r
  show (NegativeReserve m r) = "negative reserve: " <> show m <> " " <> show r

-- | Resolve one round. The World State is prepared first (Phase 1), so the caller passes it in
-- | either prepared or not; the result is the same.
resolveRound :: WorldState -> Array SignedProposal -> Either EngineError RoundResult
resolveRound w0 inputs0 =
  let
    w = prepareRound w0
    inputs = sortWith submissionOrder inputs0
    screened = map (screen w (duplicatedIds inputs)) inputs
    items = concatMap (either (const []) pure) screened
    screenEvents = concatMap (either identity _.outcome) screened
    funded = fund w items
    -- estimates read the validated proposed quantities, before funding scales them (§7.4)
    estimates = Represent.estimate w (map _.proposal items)
    settled = circulate w (settle w funded.items)
  in
    commit w inputs (screenEvents <> funded.events <> estimates.events) estimates.factors settled

-- | The canonical order of Proposals: by author, then by the numeric index of the author's
-- | submission (the last segment of the id, so index 10 follows index 2), then by id.
submissionOrder :: SignedProposal -> Tuple String (Tuple Int String)
submissionOrder { proposal: p } = Tuple (show p.author) (Tuple index (show p.id))
  where
  index = fromMaybe 0 (Int.fromString (fromMaybe "" (last (split (Pattern "/") (show p.id)))))

commit :: WorldState -> Array SignedProposal -> Array Event -> Map EntityId (Map BehaviorDomain Ratio) -> Settlement -> Either EngineError RoundResult
commit w inputs screenEvents factors st = do
  entities <- traverse (applyEntity st) w.entities
  marketList <- traverse (applyMarket st) (Map.toUnfoldable w.markets :: Array _)
  let
    events = screenEvents <> st.events
    world = w
      { round = w.round + 1
      , entities = entities
      , markets = Map.fromFoldable marketList
      , deliveries = w.deliveries <> st.deliveries
      , scheduled = Map.union st.scheduled w.scheduled
      , factors = factors
      , events = w.events <> events
      }
  pure { world, record: { round: w.round, inputs, events } }

-- | An Entity with the Settlement's net change to its holdings applied, if none goes negative.
applyEntity :: Settlement -> Entity -> Either EngineError Entity
applyEntity st e = (\accounts -> e { accounts = accounts })
  <$> checkNonNegative (Ent e.id) (applyHoldings (Ent e.id) st e.accounts)

-- | A Market with the Settlement's net change to its reserves and its new position applied, if no
-- | reserve goes negative.
applyMarket :: Settlement -> Tuple MarketId Market -> Either EngineError (Tuple MarketId Market)
applyMarket st (Tuple id m) = (\reserves -> Tuple id m { reserves = reserves, position = fromMaybe m.position (Map.lookup id st.positions) })
  <$> checkNonNegative (Mkt id) (applyHoldings (Mkt id) st m.reserves)

-- | The holdings of one Account after the Settlement's ledger entries for it.
applyHoldings :: Account -> Settlement -> Map ResourceId Q -> Map ResourceId Q
applyHoldings acct st hs = foldl (\h (Tuple (Tuple a r) q) -> if a == acct then Map.insertWith Q.add r q h else h) hs (Map.toUnfoldable st.ledger :: Array _)

-- | Settlement caps every payment at what an account holds and escrow funds the rest, so no
-- | account should go negative: the first negative holding fails the round.
checkNonNegative :: Account -> Map ResourceId Q -> Either EngineError (Map ResourceId Q)
checkNonNegative acct hs = case find (\(Tuple _ q) -> q < Q.zero) (Map.toUnfoldable hs :: Array _) of
  Just (Tuple r _) -> Left
    ( case acct of
        Ent e -> NegativeBalance e r
        Mkt m -> NegativeReserve m r
    )
  Nothing -> Right hs
