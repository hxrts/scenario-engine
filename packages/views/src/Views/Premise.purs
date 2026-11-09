-- | The premise a launch form runs (`GET /api/premise`, docs/06_simulator.md §9.1): the shape the server
-- | replies with and the dashboard reads. Pure.
module Views.Premise
  ( RoundRange
  , Premise
  ) where

-- | The supported round counts, and the count a launch without `rounds` plays.
type RoundRange = { min :: Int, max :: Int, default :: Int }

-- | The premise: its name and version, its round range, the game seed it fixes, the default run name
-- | (the next run id), and the presets every seat may play, in the premise's order.
type Premise =
  { name :: String
  , version :: String
  , rounds :: RoundRange
  , gameSeed :: Int
  , defaultName :: String
  , presets :: Array String
  }
