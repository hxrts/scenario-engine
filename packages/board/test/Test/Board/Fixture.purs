-- | Loads a fixture from packages/scenarios/fixtures.
module Test.Board.Fixture (Fixture, load, run) where

import Prelude

import Board.Decode (Round, decodeWorld)
import Core.Engine (EngineError, resolveRound)
import Core.World (RoundResult, WorldState)
import Data.Argonaut.Decode (printJsonDecodeError)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

type Fixture = { name :: String, world :: WorldState, rounds :: Array Round }

load :: String -> Effect (Either String Fixture)
load name = do
  text <- readTextFile UTF8 ("packages/scenarios/fixtures/" <> name <> ".json")
  pure (jsonParser text >>= (lmap printJsonDecodeError <<< decodeWorld))

run :: WorldState -> Round -> Either EngineError RoundResult
run w r = resolveRound w r.inputs
