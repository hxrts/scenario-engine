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
import Data.Argonaut.Core (Json, isNull, stringify, toString)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (find, null)
import Data.Either (Either(..), hush, note)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
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
  , Tuple "rounds" (maybe "default" show r.rounds)
  , Tuple "overrides" (stringify (encodeOverrides r.overrides))
  ] <> maybe [] (\d -> [ Tuple "source" d.source, Tuple "kind" d.kind ]) r.derivation

-- | The recipe a derived run is played from, given the kind and the body.
deriveRecipe :: String -> Object Json -> Recipe -> Either String Recipe
deriveRecipe kind body base = case kind of
  "duplicate" -> unchanged [ "preset", "playerSeed", "rounds", "overrides" ] *> pure base
  "rerun" -> unchanged [ "preset", "playerSeed", "rounds", "overrides" ] *> pure base
  "variant" -> do
    unchanged [ "overrides" ]
    preset <- maybe (pure base.preset) (\j -> note "preset must be a string" (toString j)) (Object.lookup "preset" body)
    playerSeed <- case Object.lookup "playerSeed" body of
      Nothing -> pure base.playerSeed
      Just j | isNull j -> pure Nothing
      Just j -> Just <$> intField "playerSeed" j
    rounds <- case Object.lookup "rounds" body of
      Nothing -> pure base.rounds
      Just j | isNull j -> pure base.rounds
      Just j -> Just <$> intField "rounds" j
    pure { preset, playerSeed, rounds, overrides: base.overrides }
  _ -> do
    unchanged [ "preset", "playerSeed", "rounds" ]
    given <- case Object.lookup "overrides" body of
      Nothing -> Left "a counterfactual needs overrides"
      Just j -> decodeOverrides j
    if null given then Left "a counterfactual needs at least one override" else pure unit
    pure base { overrides = base.overrides <> given }
  where
  -- a field the kind does not take is an error, not a silent no-op
  unchanged :: Array String -> Either String Unit
  unchanged fields = case find (\f -> Object.member f body) fields of
    Just f -> Left (kind <> " does not take " <> f <> "; only a variant or counterfactual changes the spec")
    Nothing -> pure unit

  intField :: String -> Json -> Either String Int
  intField name j = note (name <> " must be an integer") (decodeIntJson j)

-- | An integer from a JSON number, or Nothing.
decodeIntJson :: Json -> Maybe Int
decodeIntJson j = hush (decodeJson j)

-- | The recipe a stored specification was played with.
decodeSpec :: Object String -> Either String Recipe
decodeSpec s = do
  preset <- note "the stored spec has no preset" (Object.lookup "preset" s)
  playerSeed <- note "the stored spec has no player seed" (Object.lookup "playerSeed" s >>= Int.fromString)
  overrides <- case Object.lookup "overrides" s of
    Nothing -> Right []
    Just text -> jsonParser text >>= decodeOverrides
  pure { preset, playerSeed: Just playerSeed, rounds: Object.lookup "rounds" s >>= Int.fromString, overrides }
