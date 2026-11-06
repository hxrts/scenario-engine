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
  , checkHomes
  , envFor
  , runRound
  , runGame
  ) where

import Prelude

import Core.Draw (Seed, draw)
import Core.Engine (EngineError, observe, prepareRound, resolveRound)
import Core.Num (Q)
import Core.Types (Controller(..), Entity, EntityId, EntityKind(..), MarketId, PolicyId, Proposal, ProposalId, RoundIndex, SignedProposal, proposalId)
import Core.World (RoundResult, View, WorldState)
import Data.Array (concat, elem, filter, find, fromFoldable, mapWithIndex)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Newtype (unwrap)
import Data.Traversable (for)
import Foreign.Object (Object)
import Policy.Interpret (Draft, Env)
import Policy.Interpret as Policy
import Policy.Program (Program, decodeProgram)
import Board.Gameboard (Gameboard)
import Data.Argonaut.Decode (JsonDecodeError)
import Data.TraversableWithIndex (traverseWithIndex)
import Board.Decode (toMap)
import Run (Run, lift)
import Data.Foldable (for_)
import Core.Query (isAggregate)
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

derive instance Eq Deadline

instance Show Deadline where
  show ProposeDeadline = "propose"
  show SignDeadline = "sign"

data ClockF a = Await RoundIndex Deadline a

derive instance Functor ClockF

type CLOCK r = (clock :: ClockF | r)

-- | In-world Policies (NPC and Aggregate) and the scenario parameters they read.
type InWorld = { programs :: Map PolicyId Program, params :: Object Q }

-- | Decode the Gameboard's Policy catalog.
inWorldOf :: Gameboard -> Either JsonDecodeError InWorld
inWorldOf gb = do
  programs <- traverseWithIndex (const decodeProgram) gb.policies
  pure { programs: toMap programs, params: gb.parameters }

-- | The policy environment of an Entity at a View. Draws are keyed by the source (the
-- | in-world stream or a seat's stream), the round, and the Entity, so streams never share draws.
envFor :: { params :: Object Q, home :: EntityId -> Maybe MarketId, seed :: Seed, source :: String } -> EntityId -> View -> Env
envFor c e v =
  { view: v
  , params: c.params
  , home: c.home e
  , random: \i -> draw c.seed { round: v.round, source: c.source, entity: unwrap e, category: "choice", index: i }
  }

-- | An Entity's home market: the first Market (by id) it has access to, or Nothing when it has none.
homeMarket :: WorldState -> EntityId -> Maybe MarketId
homeMarket w e = map _.id (find (\m -> e `elem` m.access) (fromFoldable (Map.values w.markets)))

-- | Every Entity that acts (a seat, or an Entity whose Policy runs) needs a home market, because
-- | `$home` names it. An acting Entity with none is a startup error, named here, before any round.
checkHomes :: InWorld -> WorldState -> Either String Unit
checkHomes inWorld w = case find (\e -> acts e && isNothing (homeMarket w e.id)) (fromFoldable (Map.values w.entities)) of
  Just e -> Left ("entity " <> show e.id <> " acts but has no accessible market, so it has no home market")
  Nothing -> Right unit
  where
  acts e = isSeat e || isJust (policyOf inWorld e)
  isSeat e = case e.kind of
    Actor (Seat _) -> true
    _ -> false

-- | The Program an Entity runs in its Policy step, if it is driven by one. Seats are driven by their
-- | seat source instead.
policyOf :: InWorld -> Entity -> Maybe Program
policyOf inWorld e = case e.kind of
  Actor (NPC p) -> Map.lookup p inWorld.programs
  Aggregate a -> Map.lookup a.policy inWorld.programs
  Actor (Seat _) -> Nothing

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
