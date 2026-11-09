-- | The game package's tests: seat confinement (a ProposalBody carries no author, and the runner assigns
-- | the seat it asked), the seat presets' name check, assembly's checks over the bindings and every
-- | bound StrategyRules, and the overrides' JSON round trip.
-- | Pins: override and specification JSON are the stored spec format of docs/06 §8, each case marked `Pins` where it pins bytes.
module Test.Assembly.Main where

import Schema.Build.Decision (gt, lit, named, orderAt, param, passive, restrictionInForce, rule)
import Schema.Build.Quantity (q)
import Prelude

import Assembly.Experiment (Experiment(..), variants)

import Core.Draw (Seed(..))
import Core.Num as Q
import Laws.CandidateTransaction (candidateTransactionAt)
import World.Error (ValidationError(..))
import World.PossibleWorld.Validate (possibleWorldNames, unknownNames)
import Laws.Types (Id(..), StrategyId, RefKind(..), candidateTransactionId)
import Data.Array (fromFoldable, head)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..), isLeft)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Map as Map
import Effect (Effect)
import Foreign.Object as Object
import Data.Tuple (Tuple(..))
import Assembly.Bindings (scriptBindings)
import Assembly.Assemble (assemble, assemblePreset, presetBindings, unknownSeats)
import Assembly.Error (AssemblyError(..), renderAssemblyError)
import Assembly.Override (Override(..), decodeOverrides, encodeOverrides)
import Assembly.Premise (Premise(..))
import Schema.Run (specShape)
import Core.Shape as Shape
import Data.Argonaut.Decode (printJsonDecodeError)
import Data.Bifunctor (bimap)
import Data.String (Pattern(..), contains)
import Assembly.Recipe (decodeSpec)
import Assembly.Types (SeatBinding(..))
import Handlers.Simulate (simulate)
import Assembly.Patch (setAccount, setCurve)
import Testkit.Meridian (orThrow, premise, silent)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Variant experiments" do
    it "multi-parameter: every combination, in the order given" do
      let axes = [ { name: "a", values: [ Q.fromInt 1, Q.fromInt 2 ] }, { name: "b", values: [ Q.fromInt 3, Q.fromInt 4 ] } ]
      map _.label (variants (Parameters axes)) `shouldEqual` [ "a=1.0000, b=3.0000", "a=1.0000, b=4.0000", "a=2.0000, b=3.0000", "a=2.0000, b=4.0000" ]
    it "seat-Strategy: the baseline, then one deviating seat per alternative" do
      (map _.overrides (variants (SeatStrategy { seat: Id "US", alternatives: [ "Aggressive" ] })) == [ [], [ SeatPreset (Id "US") "Aggressive" ] ]) `shouldEqual` true
    it "player seeds and game seeds: one run per seed" do
      map _.playerSeed (variants (PlayerSeeds [ 1, 2 ])) `shouldEqual` [ Just 1, Just 2 ]
      (map _.overrides (variants (GameSeeds [ 7 ])) == [ [ GameSeed 7 ] ]) `shouldEqual` true
  describe "Seat confinement (docs/02 §8.4)" do
    it "a CandidateTransactionBody has no author: candidateTransactionAt assigns the seat that was asked" do
      let body = { intents: [], signers: [], procedure: Nothing }
      (candidateTransactionAt 2 (Id "US") 0 body).author `shouldEqual` Id "US"
      (candidateTransactionAt 2 (Id "DE") 0 body).author `shouldEqual` Id "DE"
      (candidateTransactionAt 2 (Id "DE") 1 body).author `shouldEqual` Id "DE"

  describe "Seat presets" do
    it "a human named for a live session must be a configured seat" do
      let config = { seats: Map.singleton (Id "US") "seat-1", nonPlayableActors: Map.empty, representations: [], fallbacks: Map.empty, seed: Seed 1, rounds: 5 }
      unknownSeats [ Id "US", Id "USA" ] config `shouldEqual` [ Id "USA" ]
    it "a preset that does not exist is an error, and no seat is bound silently" do
      case presetBindings Object.empty (const "Missing") [ Id "US" ] of
        Left e -> renderAssemblyError e `shouldEqual` "unknown preset Missing"
        Right _ -> fail "expected an unknown-preset error"
      bs <- orThrow (presetBindings (Object.singleton "Passive" passive) (const "Passive") [ Id "US", Id "DE" ])
      Map.size bs `shouldEqual` 2

  describe "Script bindings" do
    it "a seat's scripted CandidateTransactions are in submission order, not the order written" do
      let
        input i = { candidateTransaction: { id: candidateTransactionId 1 (Id "US") i, author: Id "US", intents: [], signers: [ Id (show i) ], procedure: Nothing }, signatures: [ Id "DE" ] }
      case Map.lookup (Id "US") (scriptBindings [ { round: 1, inputs: [ input 1, input 0 ] } ]) of
        Just (Script [ r ]) -> map _.signers r.propose `shouldEqual` [ [ Id "0" ], [ Id "1" ] ]
        _ -> fail "expected one scripted round"

  describe "Named patches of a premise" do
    it "a patch sets what it names, and fails on a name the premise does not have" do
      p <- premise
      map (\g -> (Map.lookup (Id "SA") g.world.actors) >>= Map.lookup (Id "energy-production") <<< _.accounts) (setAccount (Id "SA") (Id "energy-production") (Q.fromInt 7) p.possibleWorld)
        `shouldEqual` Right (Just (Q.fromInt 7))
      isLeft (setAccount (Id "NOBODY") (Id "energy-production") (Q.fromInt 7) p.possibleWorld) `shouldEqual` true
      isLeft (setCurve (Id "no-exchange") { intercept: Q.fromInt 1, slope: Q.zero, domain: { lo: Q.zero, hi: Q.fromInt 1 } } p.possibleWorld) `shouldEqual` true
  describe "Assembly (docs/06 §4.3)" do
    let
      assembled p bindings = map (const unit) (assemble (PossibleWorldPremise p) bindings [])
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
    it "a bound StrategyRules's names are checked: a misspelled parameter is an error, not a zero" do
      p <- premise
      let
        typo = ({ rules: [ rule [ gt (param "no-such-parameter") (lit 0) ] [] ], sign: [] })
        bindings = Map.insert (Id "US") (StrategyRules typo) (silent p)
      assembled p bindings `shouldEqual` Left (Unresolved "seat US" RefParameter "no-such-parameter")
    it "a bound StrategyRules naming an unknown Exchange or Actor is an error" do
      p <- premise
      let
        exchange = ({ rules: [ rule [] [ orderAt (named "energy-nowhere") (lit 1) Nothing ] ], sign: [] })
        actor = ({ rules: [ rule [ restrictionInForce { by: "ATLANTIS" } ] [] ], sign: [] })
      assembled p (Map.insert (Id "US") (StrategyRules exchange) (silent p)) `shouldEqual` Left (Unresolved "seat US" RefExchange "energy-nowhere")
      assembled p (Map.insert (Id "US") (StrategyRules actor) (silent p)) `shouldEqual` Left (Unresolved "seat US" RefActor "ATLANTIS")
    it "a literal premise takes no overrides" do
      p <- premise
      let world = (_.world) <$> assemble (PossibleWorldPremise p) (silent p) []
      w <- orThrow (world)
      map (const unit) (assemble (LiteralPremise { world: w, rounds: 1 }) Map.empty [ GameSeed 1 ]) `shouldEqual` Left LiteralOverrides
    it "an override naming an unknown Actor is an error" do
      p <- premise
      map (const unit) (assemblePreset p "Passive" [ Capacity (Id "NOBODY") (Id "energy") (Q.fromInt 1) ] Nothing) `shouldEqual` Left (Unresolved "override" RefActor "NOBODY")
    it "an override naming an unknown capacity, or making an unknown Actor implicit, is an error" do
      p <- premise
      map (const unit) (assemblePreset p "Passive" [ Capacity (Id "US") (Id "no-such-capacity") (Q.fromInt 1) ] Nothing) `shouldEqual` Left (Unresolved "override" RefResource "no-such-capacity")
      map (const unit) (assemblePreset p "Passive" [ Implicit (Id "NOBODY") ] Nothing) `shouldEqual` Left (Unresolved "override" RefActor "NOBODY")
    -- Pins the overrides format stored in a run's spec (docs/06 §3.1).
    it "a recipe read back from a stored spec checks its overrides when it is played" do
      p <- premise
      case decodeOverrides =<< jsonParser """[{"type":"implicit","actor":"NOBODY"}]""" of
        Left e -> fail e
        Right overrides -> map (const unit) (assemblePreset p "Passive" overrides Nothing) `shouldEqual` Left (Unresolved "override" RefActor "NOBODY")
    it "the name check knows every kind of reference" do
      p <- premise
      let
        scope = possibleWorldNames p.possibleWorld
        firstStrategy = fromMaybe "" (head (map show (fromFoldable (Map.keys p.possibleWorld.strategies) :: Array StrategyId)))
      unknownNames scope [ Tuple RefStrategy "no-such-strategy", Tuple RefRuleKind "no-such-kind" ]
        `shouldEqual` [ UnknownReference RefStrategy "no-such-strategy", UnknownReference RefRuleKind "no-such-kind" ]
      unknownNames scope [ Tuple RefStrategy firstStrategy, Tuple RefRuleKind "tariff" ] `shouldEqual` []
    it "an Agent or Human seat cannot be simulated" do
      p <- premise
      g <- orThrow (assemble (PossibleWorldPremise p) (Map.insert (Id "US") Agent (silent p)) [])
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
    -- Pins the overrides format stored in a run's spec (docs/06 §3.1).
    it "the stored form is byte-identical to specs written before the shape" do
      -- the literal is the output of the hand-written encoder this shape replaced
      let
        stored = """[{"type":"gameSeed","seed":3},{"type":"parameter","name":"tariff","value":"2.0000"},{"type":"capacity","actor":"ENCO","capacity":"energy","value":"7.0000"},{"type":"shrinkage","scheme":"sme-energy-sample","value":"1.0000"},{"type":"implicit","actor":"AID"}]"""
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
