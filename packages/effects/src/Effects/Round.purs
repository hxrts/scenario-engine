-- | The round runner and game loop (docs/02_type_system.md §8.3). Written against effects only:
-- | it uses the signatures in `Effects.Signatures` and the policy environment in
-- | `Effects.Environment`; handlers choose the context it runs in.
module Effects.Round
  ( runRound
  , runGame
  ) where

import Prelude

import Core.Engine (EngineError, observe, prepareRound, resolveRound)
import Core.Query (isAggregate)
import Core.Types (Entity, EntityId, Proposal, SignedProposal, proposalId)
import Core.World (RoundResult, View, WorldState)
import Data.Array (concat, elem, filter, fromFoldable, mapWithIndex)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Effects.Environment (InWorld, envFor, homeMarket, policyOf)
import Effects.Signatures (RECORD, RecordF(..), SEAT_INPUT, SeatInputF(..))
import Policy.Interpret (Draft, Env)
import Policy.Interpret as Policy
import Run (Run, lift)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

-- | One round: prepare and observe (pure), Propose and Sign for every Policy at once,
-- | then `resolveRound` (pure), recorded.
runRound :: forall r. InWorld -> WorldState -> Run (SEAT_INPUT + RECORD + r) (Either EngineError RoundResult)
runRound inWorld w0 = do
  let
    w = prepareRound w0
    entities = fromFoldable (Map.values w.entities)
    view e = observe w e
  drafts <- for entities \e -> map (toProposal w e.id) <<< mapWithIndex tuple' <$> proposeFor e (view e)
  let
    proposals = concat drafts
    toSign e = filter (\p -> e.id `elem` p.signers && p.author /= e.id && not (isAggregate e)) proposals
  signatures <- for entities \e -> do
    ids <- if toSign e == [] then pure [] else signFor e ((view e) { toSign = toSign e })
    pure { entity: e.id, ids }
  let
    signed :: Array SignedProposal
    signed = map (\p -> { proposal: p, signatures: map _.entity (filter (\s -> p.id `elem` s.ids) signatures) }) proposals
    result = resolveRound w signed
  for_ result \r -> lift _record (Record r unit)
  pure result
  where
  proposeFor e v = case policyOf inWorld e of
    Just program -> pure (Policy.propose program (env e v))
    Nothing -> lift _seatInput (Propose e.id v identity)
  signFor e v = case policyOf inWorld e of
    Just program -> pure (Policy.sign program (env e v))
    Nothing -> lift _seatInput (Sign e.id v identity)

  env :: Entity -> View -> Env
  env e = envFor { params: inWorld.params, home: homeMarket w0, seed: w0.seed, source: "policy" } e.id

type Tuple' = { index :: Int, draft :: Draft }

tuple' :: Int -> Draft -> Tuple'
tuple' index draft = { index, draft }

toProposal :: WorldState -> EntityId -> Tuple' -> Proposal
toProposal w author { index, draft } =
  { id: proposalId w.round author index, author, intents: draft.intents, signers: draft.signers, procedure: draft.procedure }

-- | Run `n` rounds from `w`, stopping at the first engine failure.
runGame :: forall r. InWorld -> Int -> WorldState -> Run (SEAT_INPUT + RECORD + r) (Either EngineError WorldState)
runGame inWorld n w
  | n <= 0 = pure (Right w)
  | otherwise = runRound inWorld w >>= case _ of
      Left e -> pure (Left e)
      Right r -> runGame inWorld (n - 1) r.world

_seatInput :: Proxy "seatInput"
_seatInput = Proxy

_record :: Proxy "record"
_record = Proxy
