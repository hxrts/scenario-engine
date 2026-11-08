-- | The game package's tests: seat confinement (a ProposalBody carries no author, and the runner assigns
-- | the seat it asked), the seat presets' name check, assembly's checks over the bindings and every
-- | bound Strategy, and the overrides' JSON round trip.
module Test.Game.Main where

import Schema.Build.Decision (passive)
import Schema.Build.Quantity (q)
import Prelude

import Core.Draw (Seed(..))
import Core.Num as Q
import Core.Proposal (proposalAt)
import Board.Error (ValidationError(..))
import Board.Gameboard.Validate (gameboardNames, unknownNames)
import Core.Types (Id(..), PolicyId, RefKind(..), proposalId)
import Data.Array (fromFoldable, head)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..), isLeft)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Map as Map
import Effect (Effect)
import Foreign.Object as Object
import Data.Tuple (Tuple(..))
import Game.Bindings (scriptBindings)
import Game.Assemble (assemble, assemblePreset, presetBindings, unknownSeats)
import Game.Error (GameError(..), renderGameError)
import Game.Override (Override(..), decodeOverrides, encodeOverrides)
import Game.Setting (Setting(..))
import Schema.Run (specShape)
import Core.Shape as Shape
import Data.Argonaut.Decode (printJsonDecodeError)
import Data.Bifunctor (bimap)
import Data.String (Pattern(..), contains)
import Game.Recipe (decodeSpec)
import Game.Types (SeatBinding(..))
import Simulator.Simulate (simulate)
import Game.Patch (setCapacity, setCurve)
import Testkit.Meridian (orThrow, setting, silent)
import Testkit.Json (strategyOrThrow)
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
      let config = { seats: Map.singleton (Id "US") "seat-1", npcs: Map.empty, representations: [], fallbacks: Map.empty, seed: Seed 1, rounds: 5 }
      unknownSeats [ Id "US", Id "USA" ] config `shouldEqual` [ Id "USA" ]
    it "a preset that does not exist is an error, and no seat is bound silently" do
      case presetBindings Object.empty (const "Missing") [ Id "US" ] of
        Left e -> renderGameError e `shouldEqual` "unknown preset Missing"
        Right _ -> fail "expected an unknown-preset error"
      bs <- orThrow (presetBindings (Object.singleton "Passive" passive) (const "Passive") [ Id "US", Id "DE" ])
      Map.size bs `shouldEqual` 2

  describe "Script bindings" do
    it "a seat's scripted Proposals are in submission order, not the order written" do
      let
        input i = { proposal: { id: proposalId 1 (Id "US") i, author: Id "US", intents: [], signers: [ Id (show i) ], procedure: Nothing }, signatures: [ Id "DE" ] }
      case Map.lookup (Id "US") (scriptBindings [ { round: 1, inputs: [ input 1, input 0 ] } ]) of
        Just (Script [ r ]) -> map _.signers r.propose `shouldEqual` [ [ Id "0" ], [ Id "1" ] ]
        _ -> fail "expected one scripted round"

  describe "Named patches of a setting" do
    it "a patch sets what it names, and fails on a name the setting does not have" do
      p <- setting
      map (\g -> map _.capacities (Map.lookup (Id "SA") g.world.entities)) (setCapacity (Id "SA") (Id "energy-production") (Q.fromInt 7) p.gameboard)
        `shouldEqual` Right (Just (Map.singleton (Id "energy-production") (Q.fromInt 7)))
      isLeft (setCapacity (Id "NOBODY") (Id "energy-production") (Q.fromInt 7) p.gameboard) `shouldEqual` true
      isLeft (setCurve (Id "no-market") { intercept: Q.fromInt 1, slope: Q.zero, domain: { lo: Q.zero, hi: Q.fromInt 1 } } p.gameboard) `shouldEqual` true
  describe "Assembly (docs/06 §4.3)" do
    let
      assembled p bindings = map (const unit) (assemble (GameboardSetting p) bindings [])
    it "a binding for something that is not a configured seat is rejected" do
      p <- setting
      assembled p (Map.insert (Id "CN") (Script []) (silent p)) `shouldEqual` Left (NotASeat (Id "CN"))
      assembled p (Map.insert (Id "NOBODY") (Script []) (silent p)) `shouldEqual` Left (NotASeat (Id "NOBODY"))
    it "a configured seat with no binding is rejected" do
      p <- setting
      assembled p (Map.delete (Id "AID") (silent p)) `shouldEqual` Left (UnboundSeat (Id "AID"))
    it "an implicit seat is neither bound nor required: the preset game of a run with it implicit assembles" do
      p <- setting
      map _.rounds (assemblePreset p "Passive" [ Implicit (Id "AID") ] (Just 1)) `shouldEqual` Right 1
    it "a bound Strategy's names are checked: a misspelled parameter is an error, not a zero" do
      p <- setting
      let
        typo = strategyOrThrow """{"rules":[{"when":[{"gt":[{"param":"no-such-parameter"},"0"]}],"propose":[]}]}"""
        bindings = Map.insert (Id "US") (Strategy typo) (silent p)
      assembled p bindings `shouldEqual` Left (Unresolved "seat US" ParameterRef "no-such-parameter")
    it "a bound Strategy naming an unknown Market or Entity is an error" do
      p <- setting
      let
        market = strategyOrThrow """{"rules":[{"when":[],"propose":[{"trade":{"market":"energy-nowhere","quantity":"1"}}]}]}"""
        entity = strategyOrThrow """{"rules":[{"when":[{"recorded":{"query":{"rulesInForce":{"governor":"ATLANTIS"}},"atLeast":"1"}}],"propose":[]}]}"""
      assembled p (Map.insert (Id "US") (Strategy market) (silent p)) `shouldEqual` Left (Unresolved "seat US" Market "energy-nowhere")
      assembled p (Map.insert (Id "US") (Strategy entity) (silent p)) `shouldEqual` Left (Unresolved "seat US" EntityRef "ATLANTIS")
    it "a literal setting takes no overrides" do
      p <- setting
      let world = (_.world) <$> assemble (GameboardSetting p) (silent p) []
      w <- orThrow (world)
      map (const unit) (assemble (LiteralSetting { world: w, rounds: 1 }) Map.empty [ GameSeed 1 ]) `shouldEqual` Left LiteralOverrides
    it "an override naming an unknown Entity is an error" do
      p <- setting
      map (const unit) (assemblePreset p "Passive" [ Capacity (Id "NOBODY") (Id "energy") (Q.fromInt 1) ] Nothing) `shouldEqual` Left (Unresolved "override" EntityRef "NOBODY")
    it "an override naming an unknown capacity, or making an unknown Entity implicit, is an error" do
      p <- setting
      map (const unit) (assemblePreset p "Passive" [ Capacity (Id "US") (Id "no-such-capacity") (Q.fromInt 1) ] Nothing) `shouldEqual` Left (Unresolved "override" CapacityName "no-such-capacity")
      map (const unit) (assemblePreset p "Passive" [ Implicit (Id "NOBODY") ] Nothing) `shouldEqual` Left (Unresolved "override" EntityRef "NOBODY")
    it "a recipe read back from a stored spec checks its overrides when it is played" do
      p <- setting
      case decodeOverrides =<< jsonParser """[{"type":"implicit","entity":"NOBODY"}]""" of
        Left e -> fail e
        Right overrides -> map (const unit) (assemblePreset p "Passive" overrides Nothing) `shouldEqual` Left (Unresolved "override" EntityRef "NOBODY")
    it "the name check knows every kind of reference" do
      p <- setting
      let
        scope = gameboardNames p.gameboard
        firstPolicy = fromMaybe "" (head (map show (fromFoldable (Map.keys p.gameboard.policies) :: Array PolicyId)))
      unknownNames scope [ Tuple PolicyRef "no-such-policy", Tuple RuleKindRef "no-such-kind" ]
        `shouldEqual` [ UnknownReference PolicyRef "no-such-policy", UnknownReference RuleKindRef "no-such-kind" ]
      unknownNames scope [ Tuple PolicyRef firstPolicy, Tuple RuleKindRef "tariff" ] `shouldEqual` []
    it "an Agent or Human seat cannot be simulated" do
      p <- setting
      g <- orThrow (assemble (GameboardSetting p) (Map.insert (Id "US") Agent (silent p)) [])
      case simulate (Seed 1) g of
        Left e -> e `shouldEqual` NotSimulable (Id "US")
        Right _ -> fail "expected NotSimulable"

  describe "Overrides (docs/06 §3.1)" do
    it "encode then decode reproduces every override kind" do
      let
        overrides =
          [ GameSeed 3
          , Parameter "tariff" (Q.fromInt 2)
          , Capacity (Id "ENCO") (Id "energy") (Q.fromInt 7)
          , Shrinkage "sme-energy-sample" (q "0.1")
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
    it "a stored spec reads an absent round count, a number, and fails on a present non-integer" do
      let
        -- the stored text, through the one shape; the format is pinned here (S8)
        spec rounds = bimap printJsonDecodeError identity (Shape.parse Shape.Authored specShape ("{\"preset\":\"Passive\",\"playerSeed\":\"1\"" <> rounds <> "}")) >>= decodeSpec
      isLeft (spec ",\"rounds\":\"default\"") `shouldEqual` true
      map _.rounds (spec "") `shouldEqual` Right Nothing
      map _.rounds (spec ",\"rounds\":\"3\"") `shouldEqual` Right (Just 3)
      (lmapContains (map _.rounds (spec ",\"rounds\":\"lots\""))) `shouldEqual` true

-- | The stored rounds decode failure names the value and the field.
lmapContains :: forall a. Either String a -> Boolean
lmapContains = case _ of
  Left e -> contains (Pattern "the stored spec's rounds is not an integer: lots") e
  Right _ -> false
