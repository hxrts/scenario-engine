-- | The one live run a host holds (docs/04_run.md §11, docs/07_simulator.md §8.2): the `SESSIONS` interpreter. The run
-- | plays through the same live handler as every live run. A human seat's text arrives by `Submit`, into a mailbox
-- | the round reads from; the lifecycle controls act on the run in place:
-- |
-- | - `Advance` closes the open round now: every human seat that has not spoken says nothing, and the round resolves.
-- | - `Pause` stops the propose window's clock, or restarts it; submissions are taken while paused.
-- | - `End` completes the run after the round being played.
-- | - `Clear` stops the run at once and records it as cleared, keeping the rounds it played.
-- |
-- | A finished, failed or cleared run stays readable as the host's last run until another starts.
module Handlers.Sessions
  ( SessionHost
  , Hosted
  , Slot
  , newSessionHost
  , interpretSessions
  , isLive
  ) where

import Prelude

import Data.Array (filter)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Canceler, Fiber, error, killFiber, launchAff, launchAff_, makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effects.Signatures (ControlResult(..), LiveF(..), LiveRequest, SessionState, SessionsF(..), SubmitResult(..))
import Effects.Store (setStatus)
import Handlers.Llm.Backend (Backend)
import Handlers.Session (interpretLive)
import Handlers.Store (Metric, StoreBackend, runStore)
import Laws.Types (ActorId, RoundIndex)
import Laws.World (RoundResult)
import Schema.Run (RunStatus(..))

type Slot = Either String (String -> Effect Unit) -- a text waiting to be read, or a reader waiting for one

type Hosted =
  { id :: String
  , tag :: String
  , humans :: Array ActorId
  , window :: Int
  , current :: Ref RoundIndex
  , mail :: Ref (Map (Tuple ActorId RoundIndex) Slot)
  , submitted :: Ref (Set.Set (Tuple ActorId RoundIndex))
  , entered :: Ref (Map (Tuple ActorId RoundIndex) String) -- who typed a seat's text, when not the seat
  , closed :: Ref (Set.Set RoundIndex) -- rounds advanced: every seat not yet heard says nothing
  , status :: Ref RunStatus
  , paused :: Ref Boolean
  , ending :: Ref Boolean
  , fiber :: Ref (Maybe (Fiber Unit))
  }

type SessionHost =
  { live :: Ref (Maybe Hosted)
  , store :: StoreBackend
  , metricsOf :: RoundResult -> Array Metric
  , llm :: Maybe String -> Backend
  , say :: String -> Effect Unit
  }

newSessionHost :: { store :: StoreBackend, metricsOf :: RoundResult -> Array Metric, llm :: Maybe String -> Backend, say :: String -> Effect Unit } -> Effect SessionHost
newSessionHost d = (\live -> { live, store: d.store, metricsOf: d.metricsOf, llm: d.llm, say: d.say }) <$> Ref.new Nothing

-- | Whether the host's run is live: running, paused included.
isLive :: SessionHost -> Effect Boolean
isLive host = Ref.read host.live >>= case _ of
  Nothing -> pure false
  Just h -> (_ == Running) <$> Ref.read h.status

interpretSessions :: SessionHost -> SessionsF ~> Effect
interpretSessions host = case _ of
  Host { request, humans, tag } next -> do
    current <- Ref.new request.game.world.round
    mail <- Ref.new Map.empty
    submitted <- Ref.new Set.empty
    entered <- Ref.new Map.empty
    closed <- Ref.new Set.empty
    status <- Ref.new Running
    paused <- Ref.new false
    ending <- Ref.new false
    fiber <- Ref.new Nothing
    let hosted = { id: request.id, tag, humans, window: request.window, current, mail, submitted, entered, closed, status, paused, ending, fiber }
    Ref.write (Just hosted) host.live
    f <- launchAff (play host hosted request)
    Ref.write (Just f) fiber
    pure next
  State k -> do
    found <- Ref.read host.live
    k <$> traverse stateOf found
  Submit s k -> withLive (pure (k NoSuchSession)) \hosted -> k <$> submit hosted s
  Advance k -> control k \hosted -> advance hosted
  Pause on k -> control k \hosted -> Ref.write on hosted.paused
  End k -> control k \hosted -> Ref.write true hosted.ending
  Clear k -> Ref.read host.live >>= case _ of
    Nothing -> pure (k Nothing)
    Just hosted -> do
      status <- Ref.read hosted.status
      if status /= Running then pure (k Nothing)
      else do
        Ref.write Cleared hosted.status
        running <- Ref.read hosted.fiber
        for_ running \f -> launchAff_ (killFiber (error "cleared") f)
        runStore host.store (setStatus hosted.id Cleared (Just "cleared by the facilitator"))
        pure (k (Just hosted.id))
  where
  withLive :: forall a. Effect a -> (Hosted -> Effect a) -> Effect a
  withLive none f = Ref.read host.live >>= maybe' none f

  control :: forall a. (ControlResult -> a) -> (Hosted -> Effect Unit) -> Effect a
  control k f = withLive (pure (k NoLiveRun)) \hosted -> do
    status <- Ref.read hosted.status
    if status /= Running then pure (k LiveRunOver) else f hosted $> k Controlled

  maybe' :: forall a b. Effect b -> (a -> Effect b) -> Maybe a -> Effect b
  maybe' none f = case _ of
    Nothing -> none
    Just a -> f a

play :: SessionHost -> Hosted -> LiveRequest -> Aff Unit
play host hosted request = do
  out <- interpretLive
    { store: host.store
    , metricsOf: host.metricsOf
    , input: reader hosted
    , say: host.say
    , llm: host.llm
    , paused: Ref.read hosted.paused
    , ending: Ref.read hosted.ending
    , enteredBy: \seat round -> Map.lookup (Tuple seat round) <$> Ref.read hosted.entered
    }
    (PlayLive request identity)
  liftEffect do
    status <- Ref.read hosted.status
    -- a cleared run keeps its status; otherwise the run's outcome decides it
    when (status == Running) $ Ref.write
      ( case out of
          Left _ -> Failed
          Right _ -> Completed
      )
      hosted.status

-- | What a human seat says in a round, when it arrives, or nothing once the round has been advanced.
reader :: Hosted -> ActorId -> RoundIndex -> String -> Aff String
reader hosted seat round _ = do
  liftEffect (Ref.write round hosted.current)
  makeAff \done -> do
    let key = Tuple seat round
    advanced <- Set.member round <$> Ref.read hosted.closed
    if advanced then done (Right "")
    else Map.lookup key <$> Ref.read hosted.mail >>= case _ of
      Just (Left text) -> done (Right text)
      _ -> Ref.modify_ (Map.insert key (Right (done <<< Right))) hosted.mail
    pure (nonCanceler :: Canceler)

-- | Close the open round: every human seat not yet heard says nothing, whether its reader is waiting or not.
advance :: Hosted -> Effect Unit
advance hosted = do
  round <- Ref.read hosted.current
  Ref.modify_ (Set.insert round) hosted.closed
  for_ hosted.humans \seat -> do
    let key = Tuple seat round
    Map.lookup key <$> Ref.read hosted.mail >>= case _ of
      Just (Right wake) -> Ref.modify_ (Map.insert key (Left "")) hosted.mail *> wake ""
      Just (Left _) -> pure unit
      Nothing -> Ref.modify_ (Map.insert key (Left "")) hosted.mail

submit :: Hosted -> { seat :: ActorId, round :: RoundIndex, text :: String, enteredBy :: Maybe String } -> Effect SubmitResult
submit hosted s = do
  status <- Ref.read hosted.status
  current <- Ref.read hosted.current
  taken <- Set.member key <$> Ref.read hosted.submitted
  advanced <- Set.member s.round <$> Ref.read hosted.closed
  if status /= Running then pure SessionOver
  else if not (s.seat `elem'` hosted.humans) then pure NotASeat
  else if s.round /= current || advanced then pure (NotThisRound current)
  else if taken then pure AlreadySubmitted
  else do
    Ref.modify_ (Set.insert key) hosted.submitted
    for_ s.enteredBy \by -> Ref.modify_ (Map.insert key by) hosted.entered
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
  paused <- Ref.read hosted.paused
  ending <- Ref.read hosted.ending
  taken <- Ref.read hosted.submitted
  let done e = Set.member (Tuple e round) taken
  pure { id: hosted.id, tag: hosted.tag, round, status, paused, ending, window: hosted.window, submitted: filter done hosted.humans, awaiting: filter (not <<< done) hosted.humans }
