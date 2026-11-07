module Test.Board.Main where

import Prelude

import Board.Gameboard.Instantiate (instantiate, mergeRequirements)
import Core.Num as Q
import Core.Types (Id(..))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Foreign.Object as Object
import Partial.Unsafe (unsafeCrashWith)
import Schema.Decode.Gameboard (decodeConfig, decodeGameboard)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Unsafe.Coerce (unsafeCoerce)

-- | Parse JSON text, crashing the test on a syntax error.
json :: String -> Json
json text = case jsonParser text of
  Left e -> unsafeCrashWith ("test JSON does not parse: " <> e)
  Right j -> j

-- | The smallest valid Gameboard document: no resources, markets, facilities, or Entities.
minimalGameboard :: String -> String
minimalGameboard roundRange =
  """{ "id": "test", "version": "v0", "numeraire": "M", "horizon": 5, "roundRange": """ <> roundRange
    <> """, "resources": {}, "markets": {}, "transformations": {}, "entities": {} }"""

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Requirement merge (docs/04 §4.2)" do
    -- an operating requirement of the US in money, with the given amount
    let req amount = { entity: Id "US", resource: Id "M", amount, penalty: Q.zero }
    it "two explicit amounts for one Entity and Resource are summed" do
      map _.amount (mergeRequirements [ req (Just (Q.fromInt 3)), req (Just (Q.fromInt 4)) ]) `shouldEqual` [ Just (Q.fromInt 7) ]
    it "an explicit amount and one equal to purchases both survive the merge, neither replaced by the other" do
      map _.amount (mergeRequirements [ req (Just (Q.fromInt 3)), req Nothing ]) `shouldEqual` [ Just (Q.fromInt 3), Nothing ]
    it "two requirements equal to purchases stay one" do
      map _.amount (mergeRequirements [ req Nothing, req Nothing ]) `shouldEqual` [ Nothing ]

  describe "Instantiation" do
    it "a decoded Gameboard's world carries no seed; instantiation sets it" do
      case decodeGameboard (json (minimalGameboard "[4, 8]")), decodeConfig (json """{ "seed": 9, "rounds": 4 }""") of
        Left e, _ -> fail (show e)
        _, Left e -> fail (show e)
        Right raw, Right config -> do
          -- the decoded world is a record with no seed field, not a seed placeholder
          Object.member "seed" (unsafeCoerce raw.world :: Object.Object Json) `shouldEqual` false
          case instantiate raw config of
            Left es -> fail (show es)
            Right initial -> show initial.world.seed `shouldEqual` "9"
