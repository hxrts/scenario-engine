-- | Pins: the option error texts of docs/06 §6.2.
module Test.Cli.Main where

import Prelude

import Core.Draw (Seed(..))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Data.Map as Map
import Cli.Compile (compileAll, compileOne)
import Effects.Signatures (CONSOLE, FILES, _console, _files)
import Handlers.System (interpretConsole, interpretFiles)
import Run (Run, case_, on, run)
import Type.Row (type (+))
import Testkit.Devices (capturedConsole, memoryFiles)

import Effects.Store (validRunId)
import Options (runFlags, sessionFlags)
import Flags (parseWith)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

-- | A program over in-memory files and a captured terminal; what it left in each.
underTest :: forall a. Run (FILES + CONSOLE + ()) a -> Aff { value :: a, paths :: Array String, lines :: Array String }
underTest program = do
  files <- liftEffect (Ref.new Map.empty)
  lines <- liftEffect (Ref.new [])
  value <- run (case_ # on _files (interpretFiles (memoryFiles files)) # on _console (interpretConsole (capturedConsole lines))) program
  paths <- liftEffect (Array.fromFoldable <<< Map.keys <$> Ref.read files)
  said <- liftEffect (Ref.read lines)
  pure { value, paths, lines: said }

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "compile, under fixed devices" do
    it "writes the setting and the dashboard's columns, and says how many files" do
      out <- underTest compileAll
      out.value `shouldEqual` Right unit
      Array.elem "apps/dashboard/e2e/generated/columns.json" out.paths `shouldEqual` true
      Array.length out.lines `shouldEqual` 1
    it "an unknown setting writes nothing" do
      out <- underTest (compileOne "no-such")
      (out.value == Right unit) `shouldEqual` false
      out.paths `shouldEqual` []
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
      map _.rounds (parseWith runFlags []) `shouldEqual` Right Nothing
      map _.preset (parseWith runFlags []) `shouldEqual` Right "Passive"
      map _.seed (parseWith runFlags []) `shouldEqual` Right Nothing
    it "reads a negative seed: the seed is any whole number" do
      map _.seed (parseWith runFlags [ "--seed", "-3" ]) `shouldEqual` Right (Just (Seed (-3)))
    it "rejects a seed that is not a whole number" do
      map _.rounds (parseWith runFlags [ "--seed", "x" ]) `shouldEqual` Left "--seed must be a whole number, got x"
    it "rejects rounds below one" do
      map _.rounds (parseWith runFlags [ "--rounds", "0" ]) `shouldEqual` Left "--rounds must be a whole number of at least 1, got 0"
  describe "the window flag" do
    it "a value at the lowest bound is accepted; below it is an error" do
      map _.window (parseWith (sessionFlags "m") [ "--window", "0" ]) `shouldEqual` Right 0
      map _.window (parseWith (sessionFlags "m") [ "--window", "-1" ]) `shouldEqual` Left "--window must be a whole number of at least 0, got -1"
  describe "session options" do
    it "reads its flags and defaults the rest" do
      let o = parseWith (sessionFlags "meridian") [ "--humans", "US,DE", "--window", "30" ]
      map _.setting o `shouldEqual` Right "meridian"
      map _.humans o `shouldEqual` Right "US,DE"
      map _.window o `shouldEqual` Right 30
      map _.id o `shouldEqual` Right "session-1"
    it "rejects an id that is not a valid run id" do
      map _.id (parseWith (sessionFlags "meridian") [ "--id", "../x" ]) `shouldEqual` Left "--id must be 1 to 64 letters, digits, '-' or '_', starting with a letter or digit, got ../x"
