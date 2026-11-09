-- | Pins: the flag error texts of docs/06 §1 and the usage line format.
module Test.Flags.Main where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Flags (flagNamesOf, flagOr, intFlag, optionalInt, parseFlags, parseWith, serveFlags, textFlag, usageOf)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "parseFlags" do
    it "reads --name value pairs" do
      map Object.toUnfoldable (parseFlags [ "--a", "--b" ] [ "--a", "1", "--b", "2" ]) `shouldEqual` Right [ pair "--a" "1", pair "--b" "2" ]
    it "an unknown flag is an error" do
      (parseFlags [ "--a" ] [ "--z", "1" ] $> unit) `shouldEqual` Left "unknown option --z"
    it "a flag with no value is an error" do
      (parseFlags [ "--a" ] [ "--a" ] $> unit) `shouldEqual` Left "missing value for --a"
    it "a flag given twice is an error, not last-wins" do
      (parseFlags [ "--a" ] [ "--a", "1", "--a", "2" ] $> unit) `shouldEqual` Left "duplicate option --a"
  describe "flagOr" do
    it "reads the flag, or the default when absent" do
      let flags = Object.singleton "--a" "1"
      flagOr "--a" "x" flags `shouldEqual` "1"
      flagOr "--b" "x" flags `shouldEqual` "x"
  describe "intFlag" do
    it "reads a value the rule accepts, or the default when absent" do
      intFlag "--n" 5 (_ > 0) "positive" (Object.singleton "--n" "2") `shouldEqual` Right 2
      intFlag "--n" 5 (_ > 0) "positive" Object.empty `shouldEqual` Right 5
    it "names the flag, the rule and the value when it is rejected" do
      intFlag "--n" 5 (_ > 0) "positive" (Object.singleton "--n" "0") `shouldEqual` Left "--n must be positive, got 0"
      intFlag "--n" 5 (_ > 0) "positive" (Object.singleton "--n" "x") `shouldEqual` Left "--n must be positive, got x"
  describe "optionalInt" do
    it "is Nothing when absent, and the value when given" do
      optionalInt "--n" (const true) "a whole number" Object.empty `shouldEqual` Right Nothing
      optionalInt "--n" (const true) "a whole number" (Object.singleton "--n" "-3") `shouldEqual` Right (Just (-3))
  describe "Flag" do
    it "one value gives the allow-list, the parser, and the usage line" do
      flagNamesOf serveFlags `shouldEqual` [ "--db", "--port", "--static" ]
      usageOf serveFlags `shouldEqual` "[--db <file>] [--port <1-65535>] [--static <dir>]"
      parseWith ({ a: _, b: _ } <$> textFlag "--a" "x" "1" <*> textFlag "--b" "y" "2") [ "--b", "9" ] `shouldEqual` Right { a: "1", b: "9" }
      (parseWith (textFlag "--a" "x" "1") [ "--z", "1" ] $> unit) `shouldEqual` Left "unknown option --z"
  where
  pair k v = Tuple k v
