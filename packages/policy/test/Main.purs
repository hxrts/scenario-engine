module Test.Policy.Main where

import Prelude

import Core.Num as Q
import Core.Types (Controller(..), EntityKind(..), Id(..), Intent(..))
import Core.World (Event(..), View)
import Data.Argonaut.Core (Json, toObject)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Foreign.Object as Object
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Policy.Interpret (propose)
import Policy.Program (decodeProgram)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

programs :: String -> Effect (Array (Tuple String Json))
programs path = do
  text <- readTextFile UTF8 path
  pure case jsonParser text of
    Right j -> case toObject j >>= \o -> fromMaybe (Just o) (map toObject (Object.lookup "policies" o)) of
      Just o -> Object.toUnfoldable o
      Nothing -> []
    Left _ -> []

view :: View
view =
  { round: 2
  , self: { id: Id "CN", accounts: Map.empty, capacities: Map.empty, capabilities: [], kind: Actor (NPC (Id "china-strategic-v0")) }
  , prices: Map.fromFoldable [ Tuple (Id "energy-asia") (Q.fromInt 2) ]
  , rules: []
  , events: [ Transformed { round: 1, by: Id "US", transformation: Id "major-military-action", quantity: Q.fromInt 12, paid: Q.zero } ]
  , factors: Map.empty
  , toSign: []
  }

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Decision language" do
    it "decodes every scenario Policy and seat preset" do
      ps <- liftEffect (programs "packages/scenarios/fractured-world/v0/policies.json")
      presets <- liftEffect (programs "packages/scenarios/presets.json")
      for_ (ps <> presets) \(Tuple name j) -> case decodeProgram j of
        Left e -> fail (name <> ": " <> show e)
        Right _ -> pure unit
    it "china-strategic-v0 procures after a major US action" do
      ps <- liftEffect (programs "packages/scenarios/fractured-world/v0/policies.json")
      case map decodeProgram (Object.lookup "china-strategic-v0" (Object.fromFoldable ps)) of
        Just (Right p) -> map _.intents (propose p { view, params: Object.empty, home: Id "energy-asia", random: const Q.zero })
          `shouldEqual` [ [ Transform { by: Id "CN", transformation: Id "defense-procurement-cn", quantity: Q.fromInt 4 } ] ]
        _ -> fail "missing china-strategic-v0"
    it "choose picks an option by the seeded draw" do
      presets <- liftEffect (programs "packages/scenarios/presets.json")
      case map decodeProgram (Object.lookup "RandomValid" (Object.fromFoldable presets)) of
        Just (Right p) -> do
          let at u = map _.intents (propose p { view: view { self = view.self { id = Id "SME" } }, params: Object.empty, home: Id "energy-asia", random: const u })
          at Q.zero `shouldEqual` []
          at (fromMaybe Q.zero (Q.parse "0.9")) `shouldEqual` [ [ Trade { market: Id "energy-asia", quantity: Q.fromInt (-5), limitPrice: Just (Q.fromInt 2) } ] ]
        _ -> fail "missing RandomValid"
