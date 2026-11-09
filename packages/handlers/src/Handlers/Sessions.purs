-- | Live sessions hosted by the server (docs/04_run.md §11, docs/06_simulator.md §8): the `SESSIONS` interpreter.
-- | Each session plays through the same live handler as the console. A human seat's text arrives by
-- | `Submit`, into a mailbox the round reads from.
module Handlers.Sessions
  ( SessionHost
  , Hosted
  , Slot
  , newSessionHost
  , interpretSessions
  ) where

import Prelude

import Data.Array (filter)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Canceler, launchAff_, makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effects.Signatures (LiveF(..), LiveRequest, SessionState, SessionsF(..), SubmitResult(..))
import Handlers.Llm.Backend (Backend)
import Handlers.Session (interpretLive)
import Handlers.Store (Metric, StoreBackend)
import Laws.Types (ActorId, RoundIndex)
import Laws.World (RoundResult)
import Schema.Run (RunStatus(..))

type Slot = Either String (String -> Effect Unit) -- a text waiting to be read, or a reader waiting for one

type Hosted =
  { humans :: Array ActorId
  , window :: Int
  , current :: Ref RoundIndex
  , mail :: Ref (Map (Tuple ActorId RoundIndex) Slot)
  , submitted :: Ref (Set.Set (Tuple ActorId RoundIndex))
  , status :: Ref RunStatus
  }

type SessionHost =
  { sessions :: Ref (Map String Hosted)
  , store :: StoreBackend
  , metricsOf :: RoundResult -> Array Metric
  , llm :: Backend
  }

newSessionHost :: { store :: StoreBackend, metricsOf :: RoundResult -> Array Metric, llm :: Backend } -> Effect SessionHost
newSessionHost d = (\sessions -> { sessions, store: d.store, metricsOf: d.metricsOf, llm: d.llm }) <$> Ref.new Map.empty

interpretSessions :: SessionHost -> SessionsF ~> Effect
interpretSessions host = case _ of
  Host { request, humans } next -> do
    current <- Ref.new request.game.world.round
    mail <- Ref.new Map.empty
    submitted <- Ref.new Set.empty
    status <- Ref.new Running
    let hosted = { humans, window: request.window, current, mail, submitted, status }
    Ref.modify_ (Map.insert request.id hosted) host.sessions
    launchAff_ (play host hosted request)
    pure next
  State id k -> do
    found <- Map.lookup id <$> Ref.read host.sessions
    k <$> traverse stateOf found
  Submit s k -> do
    found <- Map.lookup s.id <$> Ref.read host.sessions
    k <$> case found of
      Nothing -> pure NoSuchSession
      Just hosted -> submit hosted s

play :: SessionHost -> Hosted -> LiveRequest -> Aff Unit
play host hosted request = do
  out <- interpretLive
    { store: host.store, metricsOf: host.metricsOf, input: reader hosted, say: const (pure unit), llm: host.llm }
    (PlayLive request identity)
  liftEffect $ Ref.write
    ( case out of
        Left _ -> Failed
        Right _ -> Completed
    )
    hosted.status

-- | What a human seat says in a round, when it arrives.
reader :: Hosted -> ActorId -> RoundIndex -> String -> Aff String
reader hosted seat round _ = do
  liftEffect (Ref.write round hosted.current)
  makeAff \done -> do
    let key = Tuple seat round
    Map.lookup key <$> Ref.read hosted.mail >>= case _ of
      Just (Left text) -> done (Right text)
      _ -> Ref.modify_ (Map.insert key (Right (done <<< Right))) hosted.mail
    pure (nonCanceler :: Canceler)

submit :: Hosted -> { id :: String, seat :: ActorId, round :: RoundIndex, text :: String } -> Effect SubmitResult
submit hosted s = do
  status <- Ref.read hosted.status
  current <- Ref.read hosted.current
  taken <- Set.member key <$> Ref.read hosted.submitted
  if status /= Running then pure SessionOver
  else if not (s.seat `elem'` hosted.humans) then pure NotASeat
  else if s.round /= current then pure (NotThisRound current)
  else if taken then pure AlreadySubmitted
  else do
    Ref.modify_ (Set.insert key) hosted.submitted
    Map.lookup key <$> Ref.read hosted.mail >>= case _ of
      Just (Right wake) -> wake s.text
      _ -> Ref.modify_ (Map.insert key (Left s.text)) hosted.mail
    pure Accepted
  where
  key = Tuple s.seat s.round
  elem' x xs = filter (_ == x) xs /= []

stateOf :: Hosted -> Effect SessionState
stateOf hosted = do
  round <- Ref.read hosted.current
  status <- Ref.read hosted.status
  taken <- Ref.read hosted.submitted
  let done e = Set.member (Tuple e round) taken
  pure { round, status, window: hosted.window, submitted: filter done hosted.humans, awaiting: filter (not <<< done) hosted.humans }
