-- | A real SQLite store for a test: a device with the schema made, and the store over it.
module Testkit.Store (open, storeOver, playStored) where

import Adapters.Process (processAlive)
import Adapters.SQLite as SQLite
import Core.Draw (Seed)
import Effect (Effect)
import Effects.Signatures (NewRun, PlayF(..), PlayOutcome, PlayRequest(..))
import Game.Types (Game)
import Handlers.Play (interpretPlay)
import Views.Metrics (roundMetrics)
import Prelude (identity)
import Handlers.Sql (SqlDevice)
import Handlers.SqlStore (openStore, sqlStore)
import Handlers.Store (StoreBackend)

open :: String -> Effect SqlDevice
open = openStore SQLite.open

storeOver :: SqlDevice -> StoreBackend
storeOver d = sqlStore d processAlive

-- | A simulated game stored through `PLAY`.
playStored :: StoreBackend -> Seed -> Game -> NewRun -> Effect PlayOutcome
playStored store seed game run = interpretPlay store roundMetrics (Play (Simulated { seed, game, run }) identity)
