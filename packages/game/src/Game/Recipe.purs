-- | A run's recipe (docs/06_simulator.md §9.1): the preset, player seed, round count and overrides it
-- | is played from, and the stored specification that records them. `encodeSpec` writes that
-- | specification, `decodeSpec` reads it back, and `deriveRecipe` applies a derive request to a
-- | source's recipe. The server keeps the HTTP status of each failure.
module Game.Recipe
  ( Recipe
  , Derivation
  , encodeSpec
  , decodeSpec
  , deriveRecipe
  ) where

import Prelude

import Core.Draw (Seed)
import Schema.Run (DeriveKind(..), Spec, specKey)
import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (filter, null)
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Newtype (unwrap)
import Data.String (joinWith)
import Game.Override (Override, decodeOverrides, encodeOverrides)

-- | What a new run is played from: its preset, player seed (Nothing draws one), round count
-- | (Nothing is the configuration's), and overrides. It is what a run's stored spec records.
type Recipe = { preset :: String, playerSeed :: Maybe Int, rounds :: Maybe Int, overrides :: Array Override }

-- | The run a derived run came from, and how it was derived.
type Derivation = { source :: String, kind :: DeriveKind }

-- | The stored specification of a run: its Gameboard, the player seed it was resolved to, its recipe,
-- | the run it was derived from, if any, and, for a live session, its model and human seats.
encodeSpec :: { gameId :: String, playerSeed :: Seed, preset :: Maybe String, rounds :: Maybe Int, overrides :: Array Override, derivation :: Maybe Derivation, model :: Maybe String, humans :: Maybe String } -> Spec
encodeSpec r =
  { gameId: Just r.gameId
  , preset: r.preset
  , playerSeed: Just (unwrap r.playerSeed)
  , overrides: Just (stringify (encodeOverrides r.overrides))
  , rounds: r.rounds
  , source: map _.source r.derivation
  , kind: map _.kind r.derivation
  , model: r.model
  , humans: r.humans
  }

-- | The recipe a derived run is played from, given the request's fields (`Effects.Api.DeriveRequest`;
-- | the request's kind is parsed before this is called). Absent fields keep the source's values.
deriveRecipe
  :: forall r
   . DeriveKind
  -> { preset :: Maybe String, playerSeed :: Maybe (Maybe Int), rounds :: Maybe Int, overrides :: Maybe Json | r }
  -> Recipe
  -> Either String Recipe
deriveRecipe kind body base = case kind of
  Duplicate -> unchanged [ preset, seed, rounds, overrides ] *> pure base
  Rerun -> unchanged [ preset, seed, rounds, overrides ] *> pure base
  Variant -> do
    unchanged [ overrides ]
    pure
      { preset: fromMaybe base.preset body.preset
      , playerSeed: fromMaybe base.playerSeed body.playerSeed
      , rounds: maybe base.rounds Just body.rounds
      , overrides: base.overrides
      }
  Counterfactual -> do
    unchanged [ preset, seed, rounds ]
    given <- case body.overrides of
      Nothing -> Left "a counterfactual needs overrides"
      Just j -> decodeOverrides j
    if null given then Left "a counterfactual needs at least one override" else pure unit
    pure base { overrides = base.overrides <> given }
  where
  preset = { name: specKey.preset, given: isJust body.preset }
  seed = { name: specKey.playerSeed, given: isJust body.playerSeed }
  rounds = { name: specKey.rounds, given: isJust body.rounds }
  overrides = { name: specKey.overrides, given: isJust body.overrides }

  -- a field the kind does not take is an error, not a silent no-op
  unchanged :: Array { name :: String, given :: Boolean } -> Either String Unit
  unchanged fields = case filter _.given fields of
    [] -> pure unit
    xs -> Left (show kind <> " does not take " <> joinWith ", " (map _.name xs) <> "; only a variant or counterfactual changes the spec")

-- | The recipe a stored specification was played with. A live session has no preset and so no recipe.
decodeSpec :: Spec -> Either String Recipe
decodeSpec s = do
  preset <- note "the stored spec has no preset" s.preset
  playerSeed <- note "the stored spec has no player seed" s.playerSeed
  overrides <- case s.overrides of
    Nothing -> Right []
    Just text -> jsonParser text >>= decodeOverrides
  pure { preset, playerSeed: Just playerSeed, rounds: s.rounds, overrides }
