-- | Command-line flags shared by the apps (docs/06_simulator.md §1): `--name value` pairs.
-- | A flag that is not allowed, has no value, or is given twice is a usage error; nothing is ignored.
module Flags
  ( Flag
  , textFlag
  , intFlagOf
  , optionalIntFlag
  , checkedFlag
  , parseWith
  , usageOf
  , ServeFlags
  , serveFlags
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
import Data.String (joinWith)
import Foreign.Object (Object)
import Foreign.Object as Object

-- | The `--name value` flags of an argument list. A flag that is not allowed, has no value, or is
-- | given twice is a usage error: nothing is silently ignored.
-- | One description of a flag: its name and placeholder for the usage line, and how its value is read. Flags combine
-- | applicatively, so a command's allow-list, parser, and usage line are one value.
newtype Flag a = Flag
  { specs :: Array { name :: String, placeholder :: String }
  , read :: Object String -> Either String a
  }

instance Functor Flag where
  map f (Flag x) = Flag x { read = map f <<< x.read }

instance Apply Flag where
  apply (Flag f) (Flag x) = Flag { specs: f.specs <> x.specs, read: \o -> f.read o <*> x.read o }

instance Applicative Flag where
  pure a = Flag { specs: [], read: \_ -> Right a }

-- | A text flag with its default.
textFlag :: String -> String -> String -> Flag String
textFlag name placeholder d = Flag { specs: [ { name, placeholder } ], read: Right <<< flagOr name d }

-- | A whole-number flag with its default: `accept` is the value rule, `rule` says it in the error.
intFlagOf :: String -> String -> Int -> (Int -> Boolean) -> String -> Flag Int
intFlagOf name placeholder d accept rule = Flag { specs: [ { name, placeholder } ], read: intFlag name d accept rule }

-- | A whole-number flag that may be absent.
optionalIntFlag :: String -> String -> (Int -> Boolean) -> String -> Flag (Maybe Int)
optionalIntFlag name placeholder accept rule = Flag { specs: [ { name, placeholder } ], read: optionalInt name accept rule }

-- | A flag whose value must also pass a check, such as a run id's rule.
checkedFlag :: forall a b. (a -> Either String b) -> Flag a -> Flag b
checkedFlag check (Flag x) = Flag x { read = x.read >=> check }

-- | The arguments read through a command's flags: every flag's allow-list check, then every value.
parseWith :: forall a. Flag a -> Array String -> Either String a
parseWith (Flag x) args = parseFlags (map _.name x.specs) args >>= x.read

-- | A command's flags as a usage line: `[--rounds <n>] [--seed <n>]`.
usageOf :: forall a. Flag a -> String
usageOf (Flag x) = joinWith " " (map (\s -> "[" <> s.name <> " <" <> s.placeholder <> ">]") x.specs)

-- | The server's flags. The default port is also `apps/dashboard/ports.mjs`'s API_PORT.
type ServeFlags = { db :: String, port :: Int, static :: String }

serveFlags :: Flag ServeFlags
serveFlags = { db: _, port: _, static: _ }
  <$> textFlag "--db" "file" "runs.db"
  <*> intFlagOf "--port" "1-65535" 8787 (\p -> p > 0 && p < 65536) "a port number"
  <*> textFlag "--static" "dir" "apps/dashboard/dist"

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
