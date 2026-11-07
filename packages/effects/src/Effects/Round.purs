-- | The round runner and game loop (docs/02_type_system.md §8.3). Written against effects only:
-- | it uses the signatures in `Effects.Signatures` and the policy environment in
-- | `Effects.Environment`; handlers choose the context it runs in.
module Effects.Round
  ( runRound
  , runGame
  ) where

import Prelude

import Core.Engine.Commit (EngineError, resolveRound)
import Core.Engine.Prepare (observe, prepareRound)
import Core.Query (isAggregate)
import Core.Types (Entity, EntityId, Proposal, SignedProposal, proposalId)
import Core.World (RoundResult, View, WorldState)
import Data.Array (concat, concatMap, elem, filter, fromFoldable, mapWithIndex, null)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Traversable (for, mapAccumL)
import Effects.Environment (InWorld, envFor, homeMarket, policyOf)
import Effects.Signatures (INTERPRET, InterpretF(..), JointProposal, RECORD, RecordF(..), SEAT_INPUT, SeatInputF(..))
import Policy.Interpret (Draft, Env)
import Policy.Interpret as Policy
import Run (Run, lift)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

-- | One round: prepare and observe (pure). Seats speak; the round's transcripts are read in one
-- | joint pass. Then Propose and Sign for every Policy and seat, `resolveRound` (pure), recorded.
-- | A seat that says nothing, and a round where nobody speaks, make no joint call.
runRound :: forall r. InWorld -> WorldState -> Run (SEAT_INPUT + INTERPRET + RECORD + r) (Either EngineError RoundResult)
runRound inWorld w0 = do
  let
    w = prepareRound w0
    entities = fromFoldable (Map.values w.entities)
    view e = observe w e
  spoken <- for (filter (\e -> isNothing (policyOf inWorld e)) entities) \e -> do
    text <- lift _seatInput (Speak e.id (view e) identity)
    pure { seat: e.id, text }
  let transcripts = filter (\t -> t.text /= "") spoken
  joint <-
    if null transcripts then pure []
    else lift _interpret (Interpret w.round transcripts identity) >>= case _ of
      Right js -> pure js
      Left _ -> pure []
  let jointNumbered = numbered w joint
  drafted <- for entities \e -> map (toProposal w e.id) <<< mapWithIndex tuple' <$> proposeFor e (view e)
  let
    proposals = concat drafted <> map _.proposal jointNumbered
    toSign e = filter (\p -> e.id `elem` p.signers && p.author /= e.id && not (isAggregate e)) proposals
  signatures <- for entities \e -> do
    ids <- if toSign e == [] then pure [] else signFor e ((view e) { toSign = toSign e })
    pure { entity: e.id, ids }
  let
    signed :: Array SignedProposal
    signed = map (\p -> { proposal: p, signatures: map _.entity (filter (\s -> p.id `elem` s.ids) signatures) <> jointSigned p.id }) proposals
    jointSigned pid = concatMap _.signed (filter (\j -> j.proposal.id == pid) jointNumbered)
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

-- | The joint Proposals with their index among their author's, as a seat's own Proposals are
-- | numbered, so that replay assigns the same ids.
numbered :: WorldState -> Array JointProposal -> Array { proposal :: Proposal, signed :: Array EntityId }
numbered w joint = (mapAccumL step Map.empty joint).value
  where
  step seen j =
    let
      n = fromMaybe 0 (Map.lookup j.author seen)
    in
      { accum: Map.insert j.author (n + 1) seen
      , value: { proposal: toProposal w j.author { index: n, draft: j.draft }, signed: j.signed }
      }

type Tuple' = { index :: Int, draft :: Draft }

tuple' :: Int -> Draft -> Tuple'
tuple' index draft = { index, draft }

toProposal :: WorldState -> EntityId -> Tuple' -> Proposal
toProposal w author { index, draft } =
  { id: proposalId w.round author index, author, intents: draft.intents, signers: draft.signers, procedure: draft.procedure }

-- | Run `n` rounds from `w`, stopping at the first engine failure.
runGame :: forall r. InWorld -> Int -> WorldState -> Run (SEAT_INPUT + INTERPRET + RECORD + r) (Either EngineError WorldState)
runGame inWorld n w
  | n <= 0 = pure (Right w)
  | otherwise = runRound inWorld w >>= case _ of
      Left e -> pure (Left e)
      Right r -> runGame inWorld (n - 1) r.world

_seatInput :: Proxy "seatInput"
_seatInput = Proxy

_interpret :: Proxy "interpret"
_interpret = Proxy

_record :: Proxy "record"
_record = Proxy
