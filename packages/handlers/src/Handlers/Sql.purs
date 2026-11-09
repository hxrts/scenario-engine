-- | What the SQL store handler (`Handlers.SqlStore`) needs of a database device. An adapter implements it.
module Handlers.Sql (SqlValue(..), SqlDevice) where

import Prelude

import Effect (Effect)

-- | A statement's positional parameter.
data SqlValue = SqlText String | SqlInt Int | SqlNull

-- | One connection: a script, one statement (how many rows it changed), one query, one row inserted under
-- | its field names, one transaction (rolled back if the body throws), and closing it.
type SqlDevice =
  { script :: String -> Effect Unit
  , execute :: String -> Array SqlValue -> Effect Int
  , query :: forall row. String -> Array SqlValue -> Effect (Array (Record row))
  , insert :: forall row. String -> Record row -> Effect Unit
  , transaction :: forall a. Effect a -> Effect a
  , close :: Effect Unit
  }
