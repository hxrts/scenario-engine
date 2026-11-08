-- | A run's recipe (docs/06_simulator.md §9.1): the preset, player seed, round count and overrides it
-- | is played from, and the stored specification that records them. `encodeSpec` writes that
-- | specification, `decodeSpec` reads it back, and `deriveRecipe` applies a derive request to a
-- | source's recipe. The server keeps the HTTP status of each failure.
module Game.Recipe
  ( Recipe
  , Derivation
  , specKeys
  , specRounds
  , encodeSpec
  , decodeSpec
  , deriveRecipe
  ) where

import Prelude

import Core.Draw (Seed)
import Core.Run (DeriveKind(..), deriveKindName)
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

-- | The run a derived run came from, and how it was derived.
type Derivation = { source :: String, kind :: DeriveKind }

-- | The keys of a stored specification.
specKeys
  :: { gameId :: String, preset :: String, playerSeed :: String, rounds :: String, overrides :: String, source :: String, kind :: String, model :: String, humans :: String }
specKeys =
  { gameId: "gameId"
  , preset: "preset"
  , playerSeed: "playerSeed"
  , rounds: "rounds"
  , overrides: "overrides"
  , source: "source"
  , kind: "kind"
  , model: "model"
  , humans: "humans"
  }

-- | The round count a stored specification records, if it is a number.
specRounds :: Spec -> Maybe Int
specRounds s = Object.lookup specKeys.rounds s >>= Int.fromString

-- | The stored specification of a run: its Gameboard, the player seed it was resolved to, its recipe,
-- | the run it was derived from, if any, and, for a live session, its model and human seats. Every
-- | value is a string.
encodeSpec :: { gameId :: String, playerSeed :: Seed, preset :: String, rounds :: Maybe Int, overrides :: Array Override, derivation :: Maybe Derivation, model :: Maybe String, humans :: Maybe String } -> Spec
encodeSpec r = Object.fromFoldable $
  [ Tuple specKeys.gameId r.gameId
  , Tuple specKeys.preset r.preset
  , Tuple specKeys.playerSeed (show (unwrap r.playerSeed))
  , Tuple specKeys.rounds (maybe "default" show r.rounds)
  , Tuple specKeys.overrides (stringify (encodeOverrides r.overrides))
  ] <> maybe [] (\d -> [ Tuple specKeys.source d.source, Tuple specKeys.kind (deriveKindName d.kind) ]) r.derivation
    <> maybe [] (pure <<< Tuple specKeys.model) r.model
    <> maybe [] (pure <<< Tuple specKeys.humans) r.humans

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
  preset = { name: specKeys.preset, given: isJust body.preset }
  seed = { name: specKeys.playerSeed, given: isJust body.playerSeed }
  rounds = { name: specKeys.rounds, given: isJust body.rounds }
  overrides = { name: specKeys.overrides, given: isJust body.overrides }

  -- a field the kind does not take is an error, not a silent no-op
  unchanged :: Array { name :: String, given :: Boolean } -> Either String Unit
  unchanged fields = case filter _.given fields of
    [] -> pure unit
    xs -> Left (deriveKindName kind <> " does not take " <> joinWith ", " (map _.name xs) <> "; only a variant or counterfactual changes the spec")

-- | The recipe a stored specification was played with.
decodeSpec :: Object String -> Either String Recipe
decodeSpec s = do
  preset <- note "the stored spec has no preset" (Object.lookup specKeys.preset s)
  playerSeed <- note "the stored spec has no player seed" (Object.lookup specKeys.playerSeed s >>= Int.fromString)
  overrides <- case Object.lookup specKeys.overrides s of
    Nothing -> Right []
    Just text -> jsonParser text >>= decodeOverrides
  pure { preset, playerSeed: Just playerSeed, rounds: specRounds s, overrides }
