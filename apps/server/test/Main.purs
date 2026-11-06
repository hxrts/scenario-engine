module Test.Server.Main where

import Prelude

import Adapters.Store (open)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
import Effect (Effect)
import Effect.Class (liftEffect)
import Foreign.Object as Object
import Data.Tuple as Data.Tuple
import Scenarios (load, loadPresets)
import Server (route)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldSatisfy)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Read API" do
    it "launches a run, then serves its views and a comparison" do
      gb <- liftEffect (load "fractured-world/v0")
      ps <- liftEffect loadPresets
      case gb, ps of
        Right { gameboard, config }, Right presets -> do
          db <- liftEffect (open ":memory:")
          let
            api method path query body = liftEffect (route { gameboard, config, presets } db { method, path, query: Object.fromFoldable (map (\x -> Data.Tuple.Tuple x.k x.v) query), body })
            launch name seed = api "POST" "/api/runs" [] (stringify (encodeJson { name, preset: "Conservative", playerSeed: seed, gameSeed: (Nothing :: Maybe Int), rounds: Just 3 }))
          a <- launch "a" 1
          a.body `shouldEqual` "{\"id\":\"run-1\"}"
          _ <- launch "b" 2
          list <- api "GET" "/api/runs" [] ""
          list.body `shouldSatisfy` contains (Pattern "run-2")
          bal <- api "GET" "/api/runs/run-1/balances" [ { k: "round", v: "3" } ] ""
          bal.status `shouldEqual` 200
          cmp <- api "GET" "/api/compare" [ { k: "runs", v: "run-1,run-2" } ] ""
          cmp.body `shouldSatisfy` contains (Pattern "playerSeed")
          missing <- api "GET" "/api/runs/run-9/overview" [] ""
          missing.status `shouldEqual` 404
        _, _ -> fail "scenario"
