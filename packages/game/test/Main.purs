-- | The game package's tests: seat confinement (a ProposalBody carries no author, and the runner assigns
-- | the seat it asked), the seat presets' name check, and the overrides' JSON round trip.
module Test.Game.Main where

import Prelude

import Core.Num as Q
import Core.Proposal (proposalAt)
import Core.Types (Id(..))
import Data.Argonaut.Core (stringify)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Map as Map
import Effect (Effect)
import Foreign.Object as Object
import Game.Assemble (presetBindings)
import Game.Error (renderGameError)
import Game.Override (Override(..), decodeOverrides, encodeOverrides)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Seat confinement (docs/02 §8.4, work/4.md §7.2)" do
    it "a ProposalBody has no author: proposalAt assigns the seat that was asked" do
      let body = { intents: [], signers: [], procedure: Nothing }
      (proposalAt 2 (Id "US") 0 body).author `shouldEqual` Id "US"
      (proposalAt 2 (Id "DE") 0 body).author `shouldEqual` Id "DE"
      (proposalAt 2 (Id "DE") 1 body).author `shouldEqual` Id "DE"

  describe "Seat presets" do
    it "a preset that does not exist is an error, and no seat is bound silently" do
      case presetBindings Object.empty (const "Missing") [ Id "US" ] of
        Left e -> renderGameError e `shouldEqual` "unknown preset Missing"
        Right _ -> fail "expected an unknown-preset error"
      case presetBindings (Object.singleton "Passive" { rules: [], sign: [] }) (const "Passive") [ Id "US", Id "DE" ] of
        Left e -> fail (show e)
        Right bs -> Map.size bs `shouldEqual` 2

  describe "Overrides (docs/06 §3.1)" do
    it "encode then decode reproduces every override kind" do
      let
        overrides =
          [ GameSeed 3
          , Parameter "tariff" (Q.fromInt 2)
          , Implicit (Id "AID")
          ]
      case decodeOverrides (encodeOverrides overrides) of
        Left e -> fail e
        Right back -> stringify (encodeOverrides back) `shouldEqual` stringify (encodeOverrides overrides)
