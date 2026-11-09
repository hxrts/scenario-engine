-- | The composition root of `just _settings`'s compile step (`Cli.Compile`). An invalid setting exits 1.
module Compile (main) where

import Prelude

import Adapters.Exit (fail)
import Cli.Compile (compileAll, compileOne)
import Cli.Stack (runSystem)
import Data.Array (drop)
import Data.Either (either)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Node.Process (argv)

main :: Effect Unit
main = do
  args <- drop 2 <$> argv
  launchAff_ $ either (liftEffect <<< fail) pure =<< runSystem case args of
    [ "--only", id ] -> compileOne id
    _ -> compileAll
