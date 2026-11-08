-- | Smart constructors for the Run Store's writes (`STORE` in `Effects.Signatures`). Callers write
-- | `createRun id ...` inside a `Run (STORE + r)` program; a handler interprets it. `finish` is the one
-- | program built from them: the run lifecycle's last step. The reads are in `Effects.Observe`.
module Effects.Store
  ( createRun
  , setStatus
  , failRun
  , finish
  , failRunning
  , setSpec
  , setArchived
  , deleteRun
  ) where

import Prelude

import Core.Run (RunStatus(..))
import Data.Either (Either, either)
import Data.Maybe (Maybe(..))
import Core.Run (Spec)
import Effects.Signatures (NewRun, STORE, StoreF(..), _store)
import Run (Run, lift)
import Type.Row (type (+))

createRun :: forall r. NewRun -> Run (STORE + r) Unit
createRun n = lift _store (CreateRun n identity)

-- | Set a run's status and its failure reason (`Nothing` clears the reason).
setStatus :: forall r. String -> RunStatus -> Maybe String -> Run (STORE + r) Unit
setStatus id status reason = lift _store (SetStatus id status reason identity)

-- | Mark a run failed, with why.
failRun :: forall r. String -> String -> Run (STORE + r) Unit
failRun id reason = setStatus id Failed (Just reason)

-- | Mark a finished run completed, or failed with the error as its reason.
finish :: forall e a r. Show e => String -> Either e a -> Run (STORE + r) Unit
finish id = either (failRun id <<< show) (const (setStatus id Completed Nothing))

-- | Fail every run still marked running that a process of this role created (its leftovers, since a
-- | process of another role, such as a live session on the same store, may still be running its own),
-- | with the reason. A run with no recorded owner predates the owner column and counts as left over.
-- | Returns how many.
failRunning :: forall r. { role :: String, reason :: String } -> Run (STORE + r) Int
failRunning r = lift _store (FailRunning r identity)

setSpec :: forall r. String -> Spec -> Run (STORE + r) Unit
setSpec id spec = lift _store (SetSpec id spec identity)

setArchived :: forall r. String -> Boolean -> Run (STORE + r) Unit
setArchived id archived = lift _store (SetArchived id archived identity)

-- | Remove a run with its rounds and metrics; returns how many runs were removed (0 or 1).
deleteRun :: forall r. String -> Run (STORE + r) Int
deleteRun id = lift _store (DeleteRun id identity)
