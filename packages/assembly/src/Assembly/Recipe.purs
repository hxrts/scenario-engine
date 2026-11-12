-- | A run's recipe (docs/07_simulator.md §9.1): the preset, player seed, round count and overrides it
-- | is played from, and the stored specification that records them. `encodeSpec` writes that
-- | specification, and `decodeSpec` reads it back.
module Assembly.Recipe
  ( Recipe
  , encodeSpec
  , decodeSpec
  ) where

import Prelude

import Core.Draw (Seed)
import Schema.Run (Spec)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Parser (jsonParser)

import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)

import Assembly.Override (Override, decodeOverrides, encodeOverrides)
import Core.Shape (Format(..), encodeWith)
import Schema.Cast (Cast, castShape)

-- | What a new run is played from: its preset, player seed (Nothing draws one), round count
-- | (Nothing is the configuration's), and overrides. It is what a run's stored spec records.
type Recipe = { preset :: String, playerSeed :: Maybe Int, rounds :: Maybe Int, overrides :: Array Override }

-- | The stored specification of a run: its composition, the player seed it was resolved to, its recipe,
-- | the run it was derived from, if any, and, for a live session, its model and human seats.
encodeSpec :: { gameId :: String, composition :: String, premises :: String, playerSeed :: Seed, preset :: Maybe String, rounds :: Maybe Int, overrides :: Array Override, model :: Maybe String, humans :: Maybe String, cast :: Cast } -> Spec
encodeSpec r =
  { gameId: Just r.gameId
  , composition: Just r.composition
  , premises: Just r.premises
  , preset: r.preset
  , playerSeed: Just (unwrap r.playerSeed)
  , overrides: Just (stringify (encodeOverrides r.overrides))
  , rounds: r.rounds
  , model: r.model
  , humans: r.humans
  , cast: Just (stringify (encodeWith Snapshot castShape r.cast))
  , runClass: Nothing
  , cacheKey: Nothing
  , tag: Nothing
  , metadata: Nothing
  }

-- | The recipe a stored specification was played with. A live session has no preset and so no recipe.
decodeSpec :: Spec -> Either String Recipe
decodeSpec s = do
  preset <- note "the stored spec has no preset" s.preset
  playerSeed <- note "the stored spec has no player seed" s.playerSeed
  overrides <- case s.overrides of
    Nothing -> Right []
    Just text -> jsonParser text >>= decodeOverrides
  pure { preset, playerSeed: Just playerSeed, rounds: s.rounds, overrides }
