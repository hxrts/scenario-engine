-- | A composition the live run's start form offers (`GET /api/premise`, docs/07_simulator.md §9.2): the shape the
-- | server replies with and the dashboard reads. Pure.
module Views.Composition
  ( RoundRange
  , CompositionInfo
  ) where

-- | The supported round counts, and the count a launch without `rounds` plays.
type RoundRange = { min :: Int, max :: Int, default :: Int }

-- | A composition: its name and version, its Premises in order, its round range, the game seed it fixes, the default run name
-- | (the next run id), and the presets every seat may play, in the composition's order.
type CompositionInfo =
  { name :: String
  , version :: String
  , premises :: Array String
  , rounds :: RoundRange
  , gameSeed :: Int
  , defaultName :: String
  , presets :: Array String
  , defaultPreset :: String -- the default preset
  , castable :: Array String -- the Actors a cast may name, in id order, and the seats among them
  , seats :: Array String
  }
