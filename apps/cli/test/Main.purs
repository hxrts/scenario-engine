module Test.Cli.Main where

import Prelude

import Core.Draw (Seed(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Handlers.Store (validRunId)
import Options (numberFrom, runOptions, sessionOptions)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Session run ids" do
    it "a plain slug names a run directory" do
      validRunId "session-1" `shouldEqual` true
      validRunId "Run_2" `shouldEqual` true
    it "anything that could leave runs/ or break the id is refused" do
      validRunId "" `shouldEqual` false
      validRunId "../etc" `shouldEqual` false
      validRunId "a/b" `shouldEqual` false
      validRunId "-leading" `shouldEqual` false
      validRunId "has space" `shouldEqual` false
  describe "run options" do
    it "defaults when no flag is given" do
      map _.rounds (runOptions Object.empty) `shouldEqual` Right Nothing
      map _.preset (runOptions Object.empty) `shouldEqual` Right "Passive"
      map _.seed (runOptions Object.empty) `shouldEqual` Right Nothing
    it "reads a negative seed: the seed is any whole number" do
      map _.seed (runOptions (Object.singleton "--seed" "-3")) `shouldEqual` Right (Just (Seed (-3)))
    it "rejects a seed that is not a whole number" do
      map _.rounds (runOptions (Object.singleton "--seed" "x")) `shouldEqual` Left "--seed must be a whole number, got x"
    it "rejects rounds below one" do
      map _.rounds (runOptions (Object.singleton "--rounds" "0")) `shouldEqual` Left "--rounds must be a whole number of at least 1, got 0"
  describe "numberFrom" do
    it "a value at the lowest bound is accepted; below it is an error" do
      numberFrom 0 "--window" 9 (Object.singleton "--window" "0") `shouldEqual` Right 0
      numberFrom 0 "--window" 9 (Object.singleton "--window" "-1") `shouldEqual` Left "--window must be a whole number of at least 0, got -1"
      numberFrom 0 "--window" 9 Object.empty `shouldEqual` Right 9
  describe "session options" do
    it "reads its flags and defaults the rest" do
      let o = sessionOptions "meridian" (Object.fromFoldable [ pair "--humans" "US,DE", pair "--window" "30" ])
      map _.premise o `shouldEqual` Right "meridian"
      map _.humans o `shouldEqual` Right "US,DE"
      map _.window o `shouldEqual` Right 30
      map _.id o `shouldEqual` Right "session-1"
    it "rejects an id that is not a valid run id" do
      map _.id (sessionOptions "meridian" (Object.singleton "--id" "../x")) `shouldEqual` Left "--id must be 1 to 64 letters, digits, '-' or '_', starting with a letter or digit, got ../x"
  where
  pair k v = Tuple k v
