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
import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (filter, null)
import Data.Either (Either(..), note)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Newtype (unwrap)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Game.Override (Override, decodeOverrides, encodeOverrides)

-- | A stored specification: every value is a string.
type Spec = Object String

-- | What a new run is played from: its preset, player seed (Nothing draws one), round count
-- | (Nothing is the configuration's), and overrides. It is what a run's stored spec records.
type Recipe = { preset :: String, playerSeed :: Maybe Int, rounds :: Maybe Int, overrides :: Array Override }

-- | The run a derived run came from, and how it was derived (duplicate, rerun, variant, counterfactual).
type Derivation = { source :: String, kind :: String }

-- | The stored specification of a run: its Gameboard, the player seed it was resolved to, its recipe,
-- | and the run it was derived from, if any. Every value is a string.
encodeSpec :: { gameId :: String, playerSeed :: Seed, preset :: String, rounds :: Maybe Int, overrides :: Array Override, derivation :: Maybe Derivation } -> Spec
encodeSpec r = Object.fromFoldable $
  [ Tuple "gameId" r.gameId
  , Tuple "preset" r.preset
  , Tuple "playerSeed" (show (unwrap r.playerSeed))
  , Tuple "overrides" (stringify (encodeOverrides r.overrides))
  ] <> maybe [] (\n -> [ Tuple "rounds" (show n) ]) r.rounds <> maybe [] (\d -> [ Tuple "source" d.source, Tuple "kind" d.kind ]) r.derivation

-- | The recipe a derived run is played from, given the request's fields (`Effects.Api.DeriveRequest`;
-- | `kind` is one of the four, checked before this is called). Absent fields keep the source's values.
deriveRecipe
  :: forall r
   . { kind :: String, preset :: Maybe String, playerSeed :: Maybe (Maybe Int), rounds :: Maybe Int, overrides :: Maybe Json | r }
  -> Recipe
  -> Either String Recipe
deriveRecipe body base = case body.kind of
  "duplicate" -> unchanged [ preset, seed, rounds, overrides ] *> pure base
  "rerun" -> unchanged [ preset, seed, rounds, overrides ] *> pure base
  "variant" -> do
    unchanged [ overrides ]
    pure
      { preset: fromMaybe base.preset body.preset
      , playerSeed: fromMaybe base.playerSeed body.playerSeed
      , rounds: maybe base.rounds Just body.rounds
      , overrides: base.overrides
      }
  _ -> do
    unchanged [ preset, seed, rounds ]
    given <- case body.overrides of
      Nothing -> Left "a counterfactual needs overrides"
      Just j -> decodeOverrides j
    if null given then Left "a counterfactual needs at least one override" else pure unit
    pure base { overrides = base.overrides <> given }
  where
  preset = { name: "preset", given: isJust body.preset }
  seed = { name: "playerSeed", given: isJust body.playerSeed }
  rounds = { name: "rounds", given: isJust body.rounds }
  overrides = { name: "overrides", given: isJust body.overrides }

  -- a field the kind does not take is an error, not a silent no-op
  unchanged :: Array { name :: String, given :: Boolean } -> Either String Unit
  unchanged fields = case filter _.given fields of
    [] -> pure unit
    xs -> Left (body.kind <> " does not take " <> joinWith ", " (map _.name xs) <> "; only a variant or counterfactual changes the spec")

-- | The recipe a stored specification was played with.
decodeSpec :: Object String -> Either String Recipe
decodeSpec s = do
  preset <- note "the stored spec has no preset" (Object.lookup "preset" s)
  playerSeed <- note "the stored spec has no player seed" (Object.lookup "playerSeed" s >>= Int.fromString)
  overrides <- case Object.lookup "overrides" s of
    Nothing -> Right []
    Just text -> jsonParser text >>= decodeOverrides
  -- rows written before the key was omitted hold "default" for a configured round count
  rounds <- case Object.lookup "rounds" s of
    Nothing -> Right Nothing
    Just "default" -> Right Nothing
    Just text -> Just <$> note ("the stored spec's rounds is not an integer: " <> text) (Int.fromString text)
  pure { preset, playerSeed: Just playerSeed, rounds, overrides }
