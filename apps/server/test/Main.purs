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
import Scenarios (loadContext)
import Server (route)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldSatisfy)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Read API" do
    it "launches a run, then serves its views and a comparison" do
      liftEffect (loadContext "fractured-world/v0") >>= case _ of
        Right ctx -> do
          db <- liftEffect (open ":memory:")
          let
            api method path query body = liftEffect (route ctx db { method, path, query: Object.fromFoldable (map (\x -> Data.Tuple.Tuple x.k x.v) query), body })
            launch name seed = api "POST" "/api/runs" [] (stringify (encodeJson { name, preset: "Conservative", playerSeed: seed, rounds: Just 3 }))
          a <- launch "a" 1
          a.body `shouldEqual` "{\"id\":\"run-1\"}"
          _ <- launch "b" 2
          list <- api "GET" "/api/runs" [] ""
          list.body `shouldSatisfy` contains (Pattern "run-2")
          bal <- api "GET" "/api/runs/run-1/balances" [ { k: "round", v: "3" } ] ""
          bal.status `shouldEqual` 200
          cmp <- api "GET" "/api/compare" [ { k: "runs", v: "run-1,run-2" } ] ""
          cmp.body `shouldSatisfy` contains (Pattern "playerSeed")
          -- the trajectory unit is the market's quote per base unit; an unknown market is a client error
          cmp.body `shouldSatisfy` contains (Pattern "\"unit\":\"M/E\"")
          noMarket <- api "GET" "/api/compare" [ { k: "runs", v: "run-1,run-2" }, { k: "market", v: "no-such-market" } ] ""
          noMarket.status `shouldEqual` 400
          missing <- api "GET" "/api/runs/run-9/overview" [] ""
          missing.status `shouldEqual` 404
          -- a round the run does not have is a client error, not the initial state
          outOfRange <- api "GET" "/api/runs/run-1/balances" [ { k: "round", v: "9" } ] ""
          outOfRange.status `shouldEqual` 400
          notANumber <- api "GET" "/api/runs/run-1/events" [ { k: "round", v: "two" } ] ""
          notANumber.status `shouldEqual` 400
          -- a comparison with no valid run is a bad request
          noneValid <- api "GET" "/api/compare" [ { k: "runs", v: "run-8,run-9" } ] ""
          noneValid.status `shouldEqual` 400
          -- the agency analysis names an unknown preset explicitly
          badPreset <- api "GET" "/api/agency" [ { k: "alternative", v: "NoSuchPreset" } ] ""
          badPreset.status `shouldEqual` 400
          badLaunch <- api "POST" "/api/runs" [] (stringify (encodeJson { name: "x", preset: "NoSuchPreset", playerSeed: 1, rounds: Just 2 }))
          badLaunch.status `shouldEqual` 400
          -- a client that still sends the removed gameSeed field is accepted, and the field is ignored
          oldClient <- api "POST" "/api/runs" [] "{\"name\":\"old\",\"preset\":\"Conservative\",\"playerSeed\":4,\"gameSeed\":7,\"rounds\":2}"
          oldClient.status `shouldEqual` 200
        Left e -> fail e
