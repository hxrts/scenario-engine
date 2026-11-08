-- | The game package's tests: seat confinement (a ProposalBody carries no author, and the runner assigns
-- | the seat it asked), the seat presets' name check, assembly's checks over the bindings and every
-- | bound Strategy, and the overrides' JSON round trip.
module Test.Game.Main where

import Prelude

import Core.Draw (Seed(..))
import Core.Num as Q
import Core.Proposal (proposalAt)
import Core.Types (Id(..), RefKind(..), proposalId)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Map as Map
import Effect (Effect)
import Foreign.Object as Object
import Data.Tuple (Tuple(..))
import Game.Bindings (scriptBindings)
import Game.Assemble (assemble, assemblePreset, configuredSeats, presetBindings, unknownSeats)
import Game.Error (GameError(..), renderGameError)
import Game.Override (Override(..), decodeOverrides, encodeOverrides)
import Game.Premise (Premise(..))
import Game.Recipe (decodeSpec)
import Game.Types (SeatBinding(..))
import Simulator.Simulate (simulate)
import Testkit.Meridian (premise)
import Testkit.Json (strategyOrThrow)
import Unsafe.Coerce (unsafeCoerce)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Seat confinement (docs/02 §8.4)" do
    it "a ProposalBody has no author: proposalAt assigns the seat that was asked" do
      let body = { intents: [], signers: [], procedure: Nothing }
      (proposalAt 2 (Id "US") 0 body).author `shouldEqual` Id "US"
      (proposalAt 2 (Id "DE") 0 body).author `shouldEqual` Id "DE"
      (proposalAt 2 (Id "DE") 1 body).author `shouldEqual` Id "DE"

  describe "Seat presets" do
    it "a human named for a live session must be a configured seat" do
      let config = { seats: Map.singleton (Id "US") "seat-1", npcs: Map.empty, representations: [], fallbacks: Map.empty, seed: Seed 1, rounds: 5, label: Nothing }
      unknownSeats [ Id "US", Id "USA" ] (unsafeCoerce config) `shouldEqual` [ Id "USA" ]
    it "a preset that does not exist is an error, and no seat is bound silently" do
      case presetBindings Object.empty (const "Missing") [ Id "US" ] of
        Left e -> renderGameError e `shouldEqual` "unknown preset Missing"
        Right _ -> fail "expected an unknown-preset error"
      case presetBindings (Object.singleton "Passive" { rules: [], sign: [] }) (const "Passive") [ Id "US", Id "DE" ] of
        Left e -> fail (show e)
        Right bs -> Map.size bs `shouldEqual` 2

  describe "Script bindings" do
    it "a seat's scripted Proposals are in submission order, not the order written" do
      let
        input i = { proposal: { id: proposalId 1 (Id "US") i, author: Id "US", intents: [], signers: [ Id (show i) ], procedure: Nothing }, signatures: [ Id "DE" ] }
      case Map.lookup (Id "US") (scriptBindings [ { round: 1, inputs: [ input 1, input 0 ] } ]) of
        Just (Script [ r ]) -> map _.signers r.propose `shouldEqual` [ [ Id "0" ], [ Id "1" ] ]
        _ -> fail "expected one scripted round"

  describe "Assembly (docs/06 §4.3)" do
    let
      assembled p bindings = map (const unit) (assemble (GameboardPremise p) bindings [])
      silent p = Map.fromFoldable (map (\e -> Tuple e (Script [])) (configuredSeats p.config))
    it "a binding for something that is not a configured seat is rejected" do
      p <- premise
      assembled p (Map.insert (Id "CN") (Script []) (silent p)) `shouldEqual` Left (NotASeat (Id "CN"))
      assembled p (Map.insert (Id "NOBODY") (Script []) (silent p)) `shouldEqual` Left (NotASeat (Id "NOBODY"))
    it "a configured seat with no binding is rejected" do
      p <- premise
      assembled p (Map.delete (Id "AID") (silent p)) `shouldEqual` Left (UnboundSeat (Id "AID"))
    it "an implicit seat is neither bound nor required: the preset game of a run with it implicit assembles" do
      p <- premise
      map _.rounds (assemblePreset p "Passive" [ Implicit (Id "AID") ] (Just 1)) `shouldEqual` Right 1
    it "a bound Strategy's names are checked: a misspelled parameter is an error, not a zero" do
      p <- premise
      let
        typo = strategyOrThrow """{"rules":[{"when":[{"gt":[{"param":"no-such-parameter"},"0"]}],"propose":[]}]}"""
        bindings = Map.insert (Id "US") (Strategy typo) (silent p)
      assembled p bindings `shouldEqual` Left (Unresolved "seat US" ParameterRef "no-such-parameter")
    it "a bound Strategy naming an unknown Market or Entity is an error" do
      p <- premise
      let
        market = strategyOrThrow """{"rules":[{"when":[],"propose":[{"trade":{"market":"energy-nowhere","quantity":"1"}}]}]}"""
        entity = strategyOrThrow """{"rules":[{"when":[{"restrictionInForce":{"by":"ATLANTIS"}}],"propose":[]}]}"""
      assembled p (Map.insert (Id "US") (Strategy market) (silent p)) `shouldEqual` Left (Unresolved "seat US" Market "energy-nowhere")
      assembled p (Map.insert (Id "US") (Strategy entity) (silent p)) `shouldEqual` Left (Unresolved "seat US" EntityRef "ATLANTIS")
    it "a literal premise takes no overrides" do
      p <- premise
      let world = (_.world) <$> assemble (GameboardPremise p) (silent p) []
      case world of
        Left e -> fail (show e)
        Right w -> map (const unit) (assemble (LiteralPremise { world: w, rounds: 1 }) Map.empty [ GameSeed 1 ]) `shouldEqual` Left LiteralOverrides
    it "an override naming an unknown Entity is an error" do
      p <- premise
      map (const unit) (assemblePreset p "Passive" [ Capacity (Id "NOBODY") (Id "energy") (Q.fromInt 1) ] Nothing) `shouldEqual` Left (Unresolved "override" EntityRef "NOBODY")
    it "an Agent or Human seat cannot be simulated" do
      p <- premise
      case assemble (GameboardPremise p) (Map.insert (Id "US") Agent (silent p)) [] of
        Left e -> fail (show e)
        Right g -> case simulate (Seed 1) g of
          Left e -> e `shouldEqual` NotSimulable (Id "US")
          Right _ -> fail "expected NotSimulable"

  describe "Overrides (docs/06 §3.1)" do
    it "encode then decode reproduces every override kind" do
      let
        overrides =
          [ GameSeed 3
          , Parameter "tariff" (Q.fromInt 2)
          , Capacity (Id "ENCO") (Id "energy") (Q.fromInt 7)
          , Shrinkage "sme-energy-sample" (fromMaybe Q.zero (Q.parse "0.1"))
          , Implicit (Id "AID")
          ]
      case decodeOverrides (encodeOverrides overrides) of
        Left e -> fail e
        Right back -> stringify (encodeOverrides back) `shouldEqual` stringify (encodeOverrides overrides)
    it "the stored form is byte-identical to specs written before the shape" do
      -- the literal is the output of the hand-written encoder this shape replaced
      let
        stored = """[{"type":"gameSeed","seed":3},{"type":"parameter","name":"tariff","value":"2.0000"},{"type":"capacity","entity":"ENCO","capacity":"energy","value":"7.0000"},{"type":"shrinkage","scheme":"sme-energy-sample","value":"1.0000"},{"type":"implicit","entity":"AID"}]"""
        overrides =
          [ GameSeed 3
          , Parameter "tariff" (Q.fromInt 2)
          , Capacity (Id "ENCO") (Id "energy") (Q.fromInt 7)
          , Shrinkage "sme-energy-sample" (Q.fromInt 1)
          , Implicit (Id "AID")
          ]
      stringify (encodeOverrides overrides) `shouldEqual` stored
      map (map (const unit)) (jsonParser stored >>= decodeOverrides) `shouldEqual` Right (map (const unit) overrides)
    it "a stored spec keeps reading: an old default round count, and a present non-integer fails" do
      let
        spec rounds = Object.fromFoldable ([ Tuple "preset" "Passive", Tuple "playerSeed" "1" ] <> rounds)
      map _.rounds (decodeSpec (spec [ Tuple "rounds" "default" ])) `shouldEqual` Right Nothing
      map _.rounds (decodeSpec (spec [])) `shouldEqual` Right Nothing
      map _.rounds (decodeSpec (spec [ Tuple "rounds" "3" ])) `shouldEqual` Right (Just 3)
      map _.rounds (decodeSpec (spec [ Tuple "rounds" "lots" ])) `shouldEqual` Left "the stored spec's rounds is not an integer: lots"
