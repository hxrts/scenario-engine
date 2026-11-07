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

import Data.Either (Either, either)
import Effects.Signatures (NewRun, STORE, Spec, StoreF(..))
import Run (Run, lift)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

store :: Proxy "store"
store = Proxy

createRun :: forall r. NewRun -> Run (STORE + r) Unit
createRun n = lift store (CreateRun n identity)

setStatus :: forall r. String -> String -> Run (STORE + r) Unit
setStatus id status = lift store (SetStatus id status identity)

failRun :: forall r. String -> String -> Run (STORE + r) Unit
failRun id reason = lift store (FailRun id reason identity)

-- | Mark a finished run completed, or failed with the error as its reason.
finish :: forall e a r. Show e => String -> Either e a -> Run (STORE + r) Unit
finish id = either (failRun id <<< show) (const (setStatus id "completed"))

-- | Fail every run still marked running; returns how many.
failRunning :: forall r. String -> Run (STORE + r) Int
failRunning reason = lift store (FailRunning reason identity)

setSpec :: forall r. String -> Spec -> Run (STORE + r) Unit
setSpec id spec = lift store (SetSpec id spec identity)

setArchived :: forall r. String -> Boolean -> Run (STORE + r) Unit
setArchived id archived = lift store (SetArchived id archived identity)

-- | Remove a run with its rounds and metrics; returns how many runs were removed (0 or 1).
deleteRun :: forall r. String -> Run (STORE + r) Int
deleteRun id = lift store (DeleteRun id identity)
