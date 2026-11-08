-- | Command-line flags shared by the apps (docs/06_simulator.md §1): `--name value` pairs.
-- | A flag that is not allowed, has no value, or is given twice is a usage error; nothing is ignored.
module Flags
  ( flagNames
  , parseFlags
  , flagOr
  , intFlag
  , optionalInt
  ) where

import Prelude

import Data.Array (elem, uncons)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Foreign.Object (Object)
import Foreign.Object as Object

-- | The `--name value` flags of an argument list. A flag that is not allowed, has no value, or is
-- | given twice is a usage error: nothing is silently ignored.
-- | The flags each command takes: the one list. `scripts/validate.sh` reads it through the CLI's `flags` command.
flagNames :: { run :: Array String, session :: Array String, serve :: Array String }
flagNames =
  { run: [ "--preset", "--rounds", "--seed" ]
  , session: [ "--humans", "--model", "--rounds", "--window", "--id", "--db", "--seed" ]
  , serve: [ "--db", "--port", "--static" ]
  }

parseFlags :: Array String -> Array String -> Either String (Object String)
parseFlags allowed args = go args Object.empty
  where
  go xs acc = case uncons xs of
    Nothing -> Right acc
    Just { head: k, tail: rest }
      | not (elem k allowed) -> Left ("unknown option " <> k)
      | Object.member k acc -> Left ("duplicate option " <> k)
      | otherwise -> case uncons rest of
          Nothing -> Left ("missing value for " <> k)
          Just { head: v, tail: more } -> go more (Object.insert k v acc)

-- | A flag's value, or the default when it was not given.
flagOr :: String -> String -> Object String -> String
flagOr name d flags = fromMaybe d (Object.lookup name flags)

-- | A whole-number flag, with its default when absent. `accept` is the value rule and `rule` says it
-- | in the error: `--port must be a port number, got x`.
intFlag :: String -> Int -> (Int -> Boolean) -> String -> Object String -> Either String Int
intFlag name d accept rule flags = fromMaybe d <$> optionalInt name accept rule flags

-- | A whole-number flag that may be absent.
optionalInt :: String -> (Int -> Boolean) -> String -> Object String -> Either String (Maybe Int)
optionalInt name accept rule flags = case Object.lookup name flags of
  Nothing -> Right Nothing
  Just s -> case fromString s of
    Just n | accept n -> Right (Just n)
    _ -> Left (name <> " must be " <> rule <> ", got " <> s)
