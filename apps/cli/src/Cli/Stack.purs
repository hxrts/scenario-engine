-- | The CLI's handler stacks, closed over the real devices.
module Cli.Stack (runSystem, runSession, runInterpret, runLaunches) where

import Prelude

import Adapters.Clock (nowIso)
import Adapters.Console (closeInput, readLine, writeLine)
import Adapters.Digest (sha256)
import Adapters.Filesystem (fileDevice, lineSink)
import Adapters.Pi (pi)
import Adapters.Process (processAlive, processIdentity)
import Adapters.Random (drawOsSeed)
import Adapters.SQLite (open)
import Cli.Session (Options, SESSION, evidenceDir)
import Laws.World (WorldState)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effects.Launch (LAUNCHES)
import Effects.Signatures (CONSOLE, ENTROPY, FILES, INTERPRET, InterpretF(..), STORE, _play, _clock, _console, _entropy, _files, _interpret, _live, _observe, _process, _store)
import Handlers.Llm.Backend (Backend, backend, logging)
import Handlers.Llm.Joint (interpreter)
import Handlers.Play (interpretPlay)
import Handlers.Session (interpretLive)
import Handlers.SqlStore (openStore, sqlStore)
import Handlers.Store (interpretObserve, interpretStore)
import Handlers.System (ConsoleDevice, SystemDevices, interpretClock, interpretConsole, interpretEntropy, interpretFiles, interpretProcess)
import Run (Run, case_, on, run)
import Type.Row (type (+))
import Views.Metrics (roundMetrics)

system :: SystemDevices
system = { now: nowIso, drawSeed: drawOsSeed, identity: processIdentity }

console :: ConsoleDevice
console = { readLine, writeLine, closeInput }

-- | Programs over files, the terminal, and entropy.
runSystem :: Run (FILES + CONSOLE + ENTROPY + ()) ~> Aff
runSystem = run (case_ # on _files (interpretFiles fileDevice) # on _console (interpretConsole console) # on _entropy (interpretEntropy system))

-- | Programs over one joint pass of a backend, against a world.
runInterpret :: Backend -> WorldState -> Run (INTERPRET + ()) ~> Aff
runInterpret llm w = run (case_ # on _interpret \(Interpret round transcripts k) -> k <$> interpreter llm w round transcripts)

-- | Programs that launch and store runs, over the store in the file named.
runLaunches :: forall a. String -> Run (LAUNCHES + STORE + CONSOLE + ()) a -> Aff a
runLaunches path program = do
  db <- liftEffect (openStore open path)
  let store = sqlStore db processAlive
  run
    ( case_
        # on _observe (liftEffect <<< interpretObserve store)
        # on _store (liftEffect <<< interpretStore store)
        # on _play (liftEffect <<< interpretPlay store roundMetrics)
        # on _clock (interpretClock system)
        # on _entropy (interpretEntropy system)
        # on _process (interpretProcess system)
        # on _console (interpretConsole console)
    )
    program

-- | A session's program over its store, its model, and its evidence files. Opened only once the game is known.
runSession :: forall a. Options -> Run (SESSION + ()) a -> Aff a
runSession o program = do
  let dir = evidenceDir o
  evidence <- liftEffect (lineSink (dir <> "/calls.jsonl"))
  turn <- liftEffect (lineSink (dir <> "/turns.jsonl"))
  db <- liftEffect (openStore open o.db)
  let
    llm = logging evidence turn (backend { model: o.model, call: pi { model: o.model, rawDir: dir <> "/raw" }, digest: sha256 })
    store = sqlStore db processAlive
  run
    ( case_
        # on _observe (liftEffect <<< interpretObserve store)
        # on _store (liftEffect <<< interpretStore store)
        # on _live (interpretLive { store, metricsOf: roundMetrics, input: \_ _ prompt -> readLine prompt, say: writeLine, llm })
        # on _clock (interpretClock system)
        # on _entropy (interpretEntropy system)
        # on _process (interpretProcess system)
        # on _console (interpretConsole console)
    )
    program
