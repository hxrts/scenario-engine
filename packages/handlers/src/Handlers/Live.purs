-- | The Live handler (docs/04_instantiated_game.md §8, §11): runs `runRound` unchanged in Aff.
-- | Human seats type text, read through INTERPRET, within the propose and sign windows of CLOCK;
-- | AI-agent seats answer SEAT_INPUT directly; any other seat plays its bound program.
module Handlers.Live (Live, Seat(..), Prompt, runLive, confirmed) where

import Prelude

import Core.Types (EntityId, ProposalId)
import Core.World (RoundResult, View)
import Data.Array (length)
import Data.String (toLower, trim)
import Effect.Aff (Aff)
import Effects.Signatures (ClockF(..), Deadline(..), InterpretF(..), Interpretation(..), RECORD, RecordF(..), SEAT_INPUT, SeatInputF(..))
import Handlers.Seats (SeatSource)
import Policy.Interpret (Draft)
import Run (Run, case_, on, run)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

data Seat = Human | Agent | Program

-- | A message to one human seat: an open window (`question` empty), or a question to answer.
type Prompt = { seat :: EntityId, round :: Int, window :: Deadline, question :: String }

type Live =
  { seat :: EntityId -> Seat
  , ask :: Prompt -> Aff String -- the seat's next utterance
  , notify :: EntityId -> String -> Aff Unit -- a message to the seat that needs no answer
  , interpret :: InterpretF ~> Aff
  , clock :: ClockF ~> Aff
  , agents :: { propose :: EntityId -> View -> Aff (Array Draft), sign :: EntityId -> View -> Aff (Array ProposalId) }
  , programs :: SeatSource
  , record :: RoundResult -> Aff Unit -- persist, then narrate
  }

-- | A confirmation is `y` or `yes`, in any letter case, after trimming; anything else declines.
confirmed :: String -> Boolean
confirmed answer = toLower (trim answer) == "y" || toLower (trim answer) == "yes"

runLive :: forall a. Live -> Run (SEAT_INPUT + RECORD + ()) a -> Aff a
runLive live = run (case_ # on (Proxy :: _ "seatInput") seat # on (Proxy :: _ "record") record)
  where
  record (Record r next) = live.record r *> pure next
  seat = case _ of
    Propose e v k -> k <$> case live.seat e of
      Human -> human e v ProposeDeadline \i -> case i of
        Understood ds _ -> ds
        _ -> []
      Agent -> live.agents.propose e v
      Program -> pure (live.programs.propose e v)
    Sign e v k -> k <$> case live.seat e of
      Human -> human e v SignDeadline \i -> case i of
        Understood _ ids -> ids
        _ -> []
      Agent -> live.agents.sign e v
      Program -> pure (live.programs.sign e v)

  -- Interpret what the seat says; ask back a clarification at most twice and confirm a
  -- candidate before the window closes. An inadmissible or unconfirmed action is dropped, and
  -- the seat is told that it was dropped.
  human :: forall b. EntityId -> View -> Deadline -> (Interpretation -> Array b) -> Aff (Array b)
  human e v window pick = do
    said <- live.ask { seat: e, round: v.round, window, question: "" }
    out <- go 2 said
    live.clock (Await v.round window unit)
    pure out
    where
    prompt question = live.ask { seat: e, round: v.round, window, question }
    notice why = live.notify e ("Round " <> show v.round <> " " <> show window <> " window: " <> why) $> []
    go n said = live.interpret (Interpret e v said identity) >>= case _ of
      Clarify q | n > 0 -> prompt q >>= go (n - 1)
      Clarify _ -> notice "the action stayed ambiguous, so nothing was submitted"
      Inadmissible why -> prompt ("Flagged as inadmissible: " <> why) $> []
      i@(Understood _ _) -> do
        let candidate = pick i
        if length candidate == 0 then pure []
        else do
          yes <- prompt ("Confirm " <> show (length candidate) <> " item(s)? (y/n)")
          if confirmed yes then pure candidate
          else notice (show (length candidate) <> " item(s) not confirmed, so nothing was submitted")
