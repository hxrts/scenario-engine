-- | Effect signatures and the round runner (docs/02_type_system.md §8.3, §8.4).
-- | The runner is written against effects only; handlers choose the context.
module Effects
  ( SEAT_INPUT
  , SeatInputF(..)
  , RECORD
  , RecordF(..)
  , INTERPRET
  , InterpretF(..)
  , Interpretation(..)
  , NARRATE
  , NarrateF(..)
  , CLOCK
  , ClockF(..)
  , Deadline(..)
  , InWorld
  , inWorldOf
  , homeMarket
  , runRound
  , runGame
  ) where

import Prelude

import Core.Draw (draw)
import Core.Engine (EngineError, observe, prepareRound, resolveRound)
import Core.Num (Q)
import Core.Types (Controller(..), Entity, EntityId, EntityKind(..), Id(..), MarketId, PolicyId, Proposal, ProposalId, SignedProposal, proposalId)
import Core.World (RoundResult, View, WorldState)
import Data.Array (concat, elem, filter, find, fromFoldable, head, mapWithIndex)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Traversable (for, traverse)
import Foreign.Object (Object)
import Policy.Interpret (Draft, Env)
import Policy.Interpret as Policy
import Policy.Program (Program, decodeProgram)
import Board.Gameboard (Gameboard)
import Data.Argonaut.Decode (JsonDecodeError)
import Data.TraversableWithIndex (traverseWithIndex)
import Board.Decode (toMap)
import Run (Run, lift)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

-- | A seat's Proposals in Propose and its signatures in Sign.
data SeatInputF a
  = Propose EntityId View (Array Draft -> a)
  | Sign EntityId View (Array ProposalId -> a)

derive instance Functor SeatInputF

type SEAT_INPUT r = (seatInput :: SeatInputF | r)

-- | Append a round's result to the run's records.
data RecordF a = Record RoundResult a

derive instance Functor RecordF

type RECORD r = (record :: RecordF | r)

-- | Transcript to candidate Intents (live play). The interpreter proposes; the player confirms.
data Interpretation
  = Understood (Array Draft) (Array ProposalId) -- Proposals to publish, and Proposals to sign
  | Clarify String -- a materially ambiguous action, returned as a question
  | Inadmissible String -- claimed resources or authority the Actor lacks

data InterpretF a = Interpret EntityId View String (Interpretation -> a)

derive instance Functor InterpretF

type INTERPRET r = (interpret :: InterpretF | r)

-- | Resolved events to the front page and balance-sheet text; narration adds no facts.
data NarrateF a = Narrate RoundResult (String -> a)

derive instance Functor NarrateF

type NARRATE r = (narrate :: NarrateF | r)

-- | Session deadlines (live play only): wait until the propose or sign deadline passes.
data Deadline = ProposeDeadline | SignDeadline

data ClockF a = Await Deadline a

derive instance Functor ClockF

type CLOCK r = (clock :: ClockF | r)

-- | In-world Policies (NPC and Aggregate) and the scenario parameters they read.
type InWorld = { programs :: Map PolicyId Program, params :: Object Q }

-- | Decode the Gameboard's Policy catalog.
inWorldOf :: Gameboard -> Either JsonDecodeError InWorld
inWorldOf gb = do
  programs <- traverseWithIndex (const decodeProgram) gb.policies
  pure { programs: toMap programs, params: gb.parameters }

homeMarket :: WorldState -> EntityId -> MarketId
homeMarket w e = case find (\m -> e `elem` m.access) markets of
  Just m -> m.id
  Nothing -> fromMaybe (Id "") (map _.id (head markets))
  where
  markets = fromFoldable (Map.values w.markets)

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
  _ <- traverse (\r -> lift _record (Record r unit)) (either' result)
  pure result
  where
  either' = case _ of
    Right r -> [ r ]
    Left _ -> []
  proposeFor e v = case policyOf e of
    Just program -> pure (Policy.propose program (env e v))
    Nothing -> lift _seatInput (Propose e.id v identity)
  signFor e v = case policyOf e of
    Just program -> pure (Policy.sign program (env e v))
    Nothing -> lift _seatInput (Sign e.id v identity)

  policyOf :: Entity -> Maybe Program
  policyOf e = case e.kind of
    Actor (NPC p) -> Map.lookup p inWorld.programs
    Aggregate a -> Map.lookup a.policy inWorld.programs
    Actor (Seat _) -> Nothing

  env :: Entity -> View -> Env
  env e v =
    { view: v
    , params: inWorld.params
    , home: homeMarket w0 e.id
    , random: \i -> draw w0.seed { round: w0.round, source: "policy", entity: unwrap e.id, category: "choice", index: i }
    }
  isAggregate e = case e.kind of
    Aggregate _ -> true
    _ -> false

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
