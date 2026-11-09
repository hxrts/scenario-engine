-- | The server's handler stack: every row its programs ask for, closed over devices. A test passes fixed
-- | devices (`implementOn`); `implement` passes the real ones.
module Server.Stack (Devices, ServerCache, newServerCache, openDatabase, localStore, systemDevices, implement, implementOn, runServer) where

import Prelude

import Adapters.Clock (nowIso)
import Adapters.Process (processAlive, processIdentity)
import Adapters.Random (drawOsSeed)
import Adapters.SQLite (open)
import Effect (Effect)
import Effects.Signatures (_cache, _clock, _entropy, _observe, _play, _process, _store)
import Handlers.Api.Route (Implementation)
import Handlers.Cache (Cache, interpretCache, newCache)
import Handlers.Play (interpretPlay)
import Handlers.Sql (SqlDevice)
import Handlers.SqlStore (openStore, sqlStore)
import Handlers.Store (StoreBackend, interpretObserve, interpretStore)
import Handlers.System (SystemDevices, interpretClock, interpretEntropy, interpretProcess)
import Run (Run, case_, on, run)
import Server.Errors (Answer)
import Server.Implementation (answer)
import Server.Rows (SERVER)
import Server.Premises (Premises)
import Views.Common (StoredRun)
import Views.Metrics (roundMetrics)

type ServerCache = Cache StoredRun Answer

type Devices = { system :: SystemDevices, store :: StoreBackend, cache :: ServerCache }

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
  )

implementOn :: Devices -> Premises -> Implementation
implementOn d premises = runServer d <<< answer premises

implement :: Premises -> StoreBackend -> ServerCache -> Implementation
implement premises store cache = implementOn { system: systemDevices, store, cache } premises
