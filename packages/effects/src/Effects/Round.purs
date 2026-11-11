-- | The round runner and game loop (docs/02_kernel.md §8.3). Written against effects only:
-- | it uses the signatures in `Effects.Signatures` and the in-world Strategies in `Strategy.InWorld`;
-- | handlers choose the context it runs in. The candidate Transaction rules it applies are in `Laws.candidate Transaction`.
module Effects.Round
  ( runRound
  , runGame
  ) where

import Prelude

import Core.Map (values)

import Laws.Engine.Commit (EngineError, resolveRound)
import Laws.Engine.Prepare (observe, prepareRound)
import Laws.CandidateTransaction (assembleSigned, candidateTransactionsOf, signableBy)
import Schema.Joint (jointCandidateTransactionsOf, numberJoint)
import Laws.Types (Actor)
import Laws.World (RoundResult, View, WorldState)
import Data.Array (concat, concatMap, filter, mapMaybe, null)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..), isNothing)
import Data.Tuple (Tuple(..))
import Data.Traversable (for)
import Effects.Signatures (INTERPRET, InterpretF(..), RECORD, RecordF(..), SEAT_INPUT, SeatInputF(..), _interpret, _record, _seatInput)
import Strategy.InWorld (strategyOf)
import Laws.StrategyRules (StrategyRules, InWorld)
import Strategy.Interpret (Env, envFor)
import Strategy.Interpret as Strategy
import Run (Run, lift)
import Type.Row (type (+))

-- | One round: prepare and observe (pure). Seats speak; the round's transcripts are read in one
-- | joint pass. Then Propose and Sign for every Strategy and seat, `resolveRound` (pure), recorded with
-- | its transcripts. A seat that says nothing, and a round where nobody speaks, make no
-- | joint call.
runRound :: forall r. InWorld -> WorldState -> Run (SEAT_INPUT + INTERPRET + RECORD + r) (Either EngineError RoundResult)
runRound inWorld w0 = do
  let
    w = prepareRound w0
    actors = values w.actors
    view e = observe w e
  spoken <- for (filter (\e -> isNothing (strategyOf inWorld e)) actors) \e -> do
    said <- lift _seatInput (Speak e.id (view e) identity)
    pure (map (\text -> { seat: e.id, text }) said)
  let transcripts = mapMaybe identity spoken
  joint <-
    if null transcripts then pure []
    else map jointCandidateTransactionsOf (lift _interpret (Interpret w.round transcripts identity))
  -- a seat proposes knowing what its interpreted speech already put forward, so a routine beside it can
  -- account for that (`Planned`)
  let spokenBy e = concatMap (\j -> if j.author == e.id then j.draft.intents else []) joint
  drafted <- for actors \e -> candidateTransactionsOf w.round e.id <$> proposeFor e ((view e) { proposed = spokenBy e })
  let
    own = Map.fromFoldableWith (+) (map (\p -> Tuple p.author 1) (concat drafted))
    jointNumbered = numberJoint w.round own joint
    candidateTransactions = concat drafted <> map _.candidateTransaction jointNumbered
  signatures <- for actors \e -> do
    let signable = signableBy e candidateTransactions
    -- asked even with nothing to sign: a replay answers from the record, the author's signature included
    ids <- signFor e ((view e) { toSign = signable })
    pure { actor: e.id, ids }
  let
    result = resolveRound w (assembleSigned candidateTransactions signatures jointNumbered)
  for_ result \r -> do
    lift _record (Record r transcripts unit)
  pure result
  where
  proposeFor = decide Strategy.propose \e v -> lift _seatInput (Propose e.id v identity)
  signFor = decide Strategy.sign \e v -> lift _seatInput (Sign e.id v identity)

  -- an in-world Actor answers through its Strategy, a seat through SEAT_INPUT
  decide :: forall a. (StrategyRules -> Env -> Array a) -> (Actor -> View -> Run _ (Array a)) -> Actor -> View -> Run _ (Array a)
  decide strategy ask e v = case strategyOf inWorld e of
    Just strategyRules -> pure (strategy strategyRules (env e v))
    Nothing -> ask e v

  env :: Actor -> View -> Env
  env e = envFor { params: inWorld.params, seed: w0.seed, source: "strategy" } e.id

-- | Run `n` rounds from `w`, stopping at the first engine failure.
runGame :: forall r. InWorld -> Int -> WorldState -> Run (SEAT_INPUT + INTERPRET + RECORD + r) (Either EngineError WorldState)
runGame inWorld n w
  | n <= 0 = pure (Right w)
  | otherwise = runRound inWorld w >>= case _ of
      Left e -> pure (Left e)
      Right r -> runGame inWorld (n - 1) r.world
