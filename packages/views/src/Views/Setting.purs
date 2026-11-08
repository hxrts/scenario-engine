-- | The setting a launch form runs (`GET /api/setting`, docs/06_simulator.md §9.1): the shape the server
-- | replies with and the dashboard reads. Pure.
module Views.Setting
  ( RoundRange
  , Setting
  ) where

-- | The supported round counts, and the count a launch without `rounds` plays.
type RoundRange = { min :: Int, max :: Int, default :: Int }

-- | The setting: its name and version, its round range, the game seed it fixes, the default run name
-- | (the next run id), and the presets every seat may play, in the setting's order.
type Setting =
  { name :: String
  , version :: String
  , rounds :: RoundRange
  , gameSeed :: Int
  , defaultName :: String
  , presets :: Array String
  }
