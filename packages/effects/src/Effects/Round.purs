-- | The round runner and game loop (docs/02_type_system.md §8.3). Written against effects only:
-- | it uses the signatures in `Effects.Signatures` and the in-world Policies in `Policy.InWorld`;
-- | handlers choose the context it runs in. The Proposal rules it applies are in `Core.Proposal`.
module Effects.Round
  ( runRound
  , runGame
  ) where

import Prelude

import Core.Map (values)

import Core.Engine.Commit (EngineError, resolveRound)
import Core.Engine.Prepare (observe, prepareRound)
import Core.Proposal (assembleSigned, proposalsOf, signableBy)
import Schema.Joint (jointProposalsOf, numberJoint)
import Core.Types (Entity)
import Core.World (RoundResult, View, WorldState)
import Data.Array (concat, filter, mapMaybe, null)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..), isNothing)
import Data.Tuple (Tuple(..))
import Data.Traversable (for)
import Effects.Signatures (INTERPRET, InterpretF(..), NARRATE, NarrateF(..), RECORD, RecordF(..), SEAT_INPUT, SeatInputF(..), _interpret, _narrate, _record, _seatInput)
import Policy.InWorld (policyOf)
import Core.Strategy (Strategy, InWorld)
import Policy.Interpret (Env, envFor)
import Policy.Interpret as Policy
import Run (Run, lift)
import Type.Row (type (+))

-- | One round: prepare and observe (pure). Seats speak; the round's transcripts are read in one
-- | joint pass. Then Propose and Sign for every Policy and seat, `resolveRound` (pure), recorded with
-- | its transcripts and narrated. A seat that says nothing, and a round where nobody speaks, make no
-- | joint call.
runRound :: forall r. InWorld -> WorldState -> Run (SEAT_INPUT + INTERPRET + RECORD + NARRATE + r) (Either EngineError RoundResult)
runRound inWorld w0 = do
  let
    w = prepareRound w0
    entities = values w.entities
    view e = observe w e
  spoken <- for (filter (\e -> isNothing (policyOf inWorld e)) entities) \e -> do
    said <- lift _seatInput (Speak e.id (view e) identity)
    pure (map (\text -> { seat: e.id, text }) said)
  let transcripts = mapMaybe identity spoken
  joint <-
    if null transcripts then pure []
    else map jointProposalsOf (lift _interpret (Interpret w.round transcripts identity))
  drafted <- for entities \e -> proposalsOf w.round e.id <$> proposeFor e (view e)
  let
    own = Map.fromFoldableWith (+) (map (\p -> Tuple p.author 1) (concat drafted))
    jointNumbered = numberJoint w.round own joint
    proposals = concat drafted <> map _.proposal jointNumbered
  signatures <- for entities \e -> do
    let signable = signableBy e proposals
    -- asked even with nothing to sign: a replay answers from the record, the author's signature included
    ids <- signFor e ((view e) { toSign = signable })
    pure { entity: e.id, ids }
  let
    result = resolveRound w (assembleSigned proposals signatures jointNumbered)
  for_ result \r -> do
    lift _record (Record r transcripts unit)
    void (lift _narrate (Narrate r identity))
  pure result
  where
  proposeFor = decide Policy.propose \e v -> lift _seatInput (Propose e.id v identity)
  signFor = decide Policy.sign \e v -> lift _seatInput (Sign e.id v identity)

  -- an in-world Entity answers through its Policy, a seat through SEAT_INPUT
  decide :: forall a. (Strategy -> Env -> Array a) -> (Entity -> View -> Run _ (Array a)) -> Entity -> View -> Run _ (Array a)
  decide policy ask e v = case policyOf inWorld e of
    Just strategy -> pure (policy strategy (env e v))
    Nothing -> ask e v

  env :: Entity -> View -> Env
  env e = envFor { params: inWorld.params, seed: w0.seed, source: "policy" } e.id

-- | Run `n` rounds from `w`, stopping at the first engine failure.
runGame :: forall r. InWorld -> Int -> WorldState -> Run (SEAT_INPUT + INTERPRET + RECORD + NARRATE + r) (Either EngineError WorldState)
runGame inWorld n w
  | n <= 0 = pure (Right w)
  | otherwise = runRound inWorld w >>= case _ of
      Left e -> pure (Left e)
      Right r -> runGame inWorld (n - 1) r.world
