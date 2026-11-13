-- | The server's handler stack: every row its programs ask for, closed over devices. A test passes fixed
-- | devices (`implementOn`); `implement` passes the real ones.
module Server.Stack (Devices, ServerCache, newServerCache, openDatabase, localStore, systemDevices, implement, newHost, implementOn, runServer) where

import Views.Compare (CompareView)
import Prelude

import Data.Maybe (fromMaybe)

import Adapters.Clock (nowIso)
import Adapters.Process (processAlive, processIdentity)
import Adapters.Random (drawOsSeed)
import Adapters.SQLite (open)
import Effect (Effect)
import Effects.Signatures (_cache, _clock, _entropy, _observe, _play, _process, _sessions, _store)
import Handlers.Sessions (SessionHost, interpretSessions, newSessionHost)
import Handlers.Llm.Backend (backend, defaultModel)
import Adapters.Pi (pi)
import Adapters.Digest (sha256)
import Handlers.Api.Route (Implementation)
import Handlers.Cache (Cache, interpretCache, newCache)
import Handlers.Play (interpretPlay)
import Handlers.Sql (SqlDevice)
import Handlers.SqlStore (openStore, sqlStore)
import Handlers.Store (StoreBackend, interpretObserve, interpretStore)
import Handlers.System (SystemDevices, interpretClock, interpretEntropy, interpretProcess)
import Run (Run, case_, on, run)
import Server.Implementation (answer)
import Server.Rows (SERVER)
import Server.Compositions (Compositions)
import Views.Common (StoredRun)
import Views.Metrics (roundMetrics)

type ServerCache = Cache StoredRun CompareView

type Devices = { system :: SystemDevices, store :: StoreBackend, cache :: ServerCache, sessions :: SessionHost }

newServerCache :: Effect ServerCache
newServerCache = newCache

openDatabase :: String -> Effect SqlDevice
openDatabase = openStore open

localStore :: SqlDevice -> StoreBackend
localStore d = sqlStore d processAlive

systemDevices :: SystemDevices
systemDevices = { now: nowIso, drawSeed: drawOsSeed, identity: processIdentity }

runServer :: Devices -> Run (SERVER ()) ~> Effect
runServer d = run
  ( case_
      # on _observe (interpretObserve d.store)
      # on _store (interpretStore d.store)
      # on _cache (interpretCache d.cache)
      # on _play (interpretPlay d.store roundMetrics)
      # on _clock (interpretClock d.system)
      # on _entropy (interpretEntropy d.system)
      # on _process (interpretProcess d.system)
      # on _sessions (interpretSessions d.sessions)
  )

implementOn :: Devices -> Compositions -> Implementation
implementOn d compositions = runServer d <<< answer compositions

implement :: Compositions -> StoreBackend -> ServerCache -> SessionHost -> Implementation
implement compositions store cache sessions = implementOn { system: systemDevices, store, cache, sessions } compositions

-- | The session host of the real server: its seats and joint pass run through the `pi` model process.
newHost :: StoreBackend -> Effect SessionHost
newHost store = newSessionHost { store, metricsOf: roundMetrics, say: const (pure unit), llm: \m -> let model = fromMaybe defaultModel m in backend { model, call: pi { model, rawDir: "runs/server/raw" }, digest: sha256 } }
