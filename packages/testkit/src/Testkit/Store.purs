-- | A real SQLite store for a test: a device with the schema made, and the store over it.
module Testkit.Store (open, storeOver) where

import Adapters.Process (processAlive)
import Adapters.SQLite as SQLite
import Effect (Effect)
import Handlers.Sql (SqlDevice)
import Handlers.SqlStore (openStore, sqlStore)
import Handlers.Store (StoreBackend)

open :: String -> Effect SqlDevice
open = openStore SQLite.open

storeOver :: SqlDevice -> StoreBackend
storeOver d = sqlStore d processAlive
