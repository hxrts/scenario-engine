-- | The SQLite device: open a connection and run one statement, one query, or one dbDbTransaction on it.
-- | It knows no table; the schema, the statements and the decoding are the handler's (`Handlers.SqlStore`).
module Adapters.SQLite (open, configuredBusyTimeout) where

import Prelude

import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Nullable (Nullable, null)
import Effect (Effect)
import Foreign (Foreign, unsafeToForeign)
import Handlers.Sql (SqlDevice, SqlValue(..))

foreign import data Db :: Type

-- | The busy timeout every connection is configured with, in milliseconds.
foreign import configuredBusyTimeout :: Int

foreign import openImpl :: String -> Effect Db
foreign import closeImpl :: Db -> Effect Unit
foreign import scriptImpl :: Fn2 Db String (Effect Unit)
foreign import executeImpl :: Fn3 Db String (Array Foreign) (Effect Int)
foreign import queryImpl :: forall row. Fn3 Db String (Array Foreign) (Effect (Array (Record row)))
foreign import insertImpl :: forall row. Fn3 Db String (Record row) (Effect Unit)
foreign import dbDbTransactionImpl :: forall a. Fn2 Db (Effect a) (Effect a)

-- | Open a connection, creating the file if it is new. Other connections' locks are waited for.
open :: String -> Effect SqlDevice
open path = device <$> openImpl path

device :: Db -> SqlDevice
device db =
  { script: runFn2 scriptImpl db
  , execute: \sql params -> runFn3 executeImpl db sql (map parameter params)
  , query: \sql params -> runFn3 queryImpl db sql (map parameter params)
  , insert: runFn3 insertImpl db
  , dbDbTransaction: runFn2 dbDbTransactionImpl db
  , close: closeImpl db
  }

parameter :: SqlValue -> Foreign
parameter = case _ of
  SqlText s -> unsafeToForeign s
  SqlInt n -> unsafeToForeign n
  SqlNull -> unsafeToForeign (null :: Nullable Int)
