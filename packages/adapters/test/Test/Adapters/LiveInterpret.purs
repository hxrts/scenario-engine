-- | The one-off, billed check of the joint pass against the real `pi` backend (docs/06 §14). It
-- | makes exactly one model call over a fixed set of transcripts, and checks that the reply
-- | decodes and that the two transcripts that describe one joint Proposal reconcile into one Proposal with
-- | both seats signing. It is not part of any suite (`spago test -p`, `just test`, `just check`,
-- | `just all`, `just dashboard-test`). Run it with `just live-interpret`, which needs
-- | ALLOW_BILLED_LIVE=1; this module refuses to run without it.
module Test.Adapters.LiveInterpret (main) where

import Prelude

import Adapters.Digest (sha256)
import Adapters.Pi (pi)
import Control.Monad.Error.Class (throwError)
import Schema.Joint (Transcript)
import Rules.Types (Id(..))
import Rules.World (WorldState)
import Data.Array (elem, filter, length)
import Data.Foldable (for_)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error, message)

import Handlers.Llm.Joint (interpreter)
import Handlers.Llm.Backend (backend, defaultModel)
import Node.Process (exit', lookupEnv)
import Testkit.Meridian (presetSeats)

-- | The fixed transcripts: the US and Germany describe one trade, and Japan says something unrelated.
transcripts :: Array Transcript
transcripts =
  [ { seat: Id "US", text: "I will sell Germany ten units of energy this round for fifty units of money." }
  , { seat: Id "DE", text: "Agreed. I will buy ten units of energy from the US for fifty units of money." }
  , { seat: Id "JP", text: "I am keeping my money at home this round." }
  ]

world :: Aff WorldState
world = do
  g <- presetSeats [] "Passive" Nothing
  pure g.world

-- | One joint pass, then the checks. A failed check throws; the caller reports and exits 1.
check :: Aff Unit
check = do
  w <- world
  let llm = backend { model: defaultModel, call: pi { model: defaultModel, rawDir: "work/live-interpret" }, digest: sha256 }
  out <- interpreter llm w 1 transcripts
  js <- case out of
    Right js -> pure js
    Left why -> throwError (error ("the reply did not decode: " <> why))
  liftEffect $ log ("model " <> defaultModel <> ": " <> show (length js) <> " Proposal(s)")
  for_ js \j -> liftEffect (log ("  author " <> show j.author <> ", signed " <> show j.signed <> ", intents " <> show (length j.draft.intents)))
  let both = filter (\j -> elem (Id "US") j.signed && elem (Id "DE") j.signed) js
  when (length both /= 1) $
    throwError (error ("the US and German transcripts must reconcile into one Proposal signed by both; got " <> show (length both)))
  liftEffect $ log "ok: the reply decodes and the matching transcripts reconcile into one Proposal"

main :: Effect Unit
main = do
  allowed <- lookupEnv "ALLOW_BILLED_LIVE"
  case allowed of
    Just "1" -> launchAff_ do
      out <- attempt check
      liftEffect $ case out of
        Right _ -> exit' 0
        Left e -> log ("live interpretation check failed: " <> message e) *> exit' 1
    _ -> do
      log "refused: this makes one billed model call. Set ALLOW_BILLED_LIVE=1 (or run `just live-interpret`)."
      exit' 2
