-- | The game package's tests: seat confinement (a ProposalBody carries no author, and the runner assigns
-- | the seat it asked), the seat presets' name check, assembly's checks over the bindings and every
-- | bound StrategyRules, and the overrides' JSON round trip.
-- | Pins: override and specification JSON are the stored spec format of docs/07 §8, each case marked `Pins` where it pins bytes.
module Test.Assembly.Main where

import Schema.Build.Decision (gt, lit, named, orderAt, param, passive, restrictionInForce, rule)
import Schema.Build.Quantity (q)
import Prelude

import Assembly.Composition (checkRefs, checkComposition, Origin(..))
import Assembly.Error (AssemblyError(..), renderAssemblyError)
import Laws.StrategyRules (SignRule(..), Template(..))
import Laws.Types (IntentF(..), RefKind(..), Id(..), StrategyId, candidateTransactionId)
import World.Composition.Validate (compositionNames, unknownNames)

import Core.Kernel.Types (Account(..), Amount(..), Party(..), Short(..), noNext)
import Core.Kernel.Types as K
import Laws.World (WorldTemplate(..))
import Data.Either (either, Either(..), isLeft)
import Data.Array (fromFoldable, head)

import Assembly.Experiment (Experiment(..), variants)

import Core.Draw (Seed(..))
import Core.Num as Q
import Laws.CandidateTransaction (candidateTransactionAt)
import World.Error (ValidationError(..))
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Map as Map
import Effect (Effect)
import Foreign.Object as Object
import Data.Tuple (Tuple(..))
import Assembly.Bindings (scriptBindings)
import Assembly.Assemble (assemble, assemblePreset, presetBindings, unknownSeats)
import Assembly.Override (Override(..), decodeOverrides, encodeOverrides)
import Schema.Run (specShape)
import Core.Shape as Shape
import Data.Argonaut.Decode (printJsonDecodeError)
import Data.Bifunctor (bimap)
import Data.String (Pattern(..), contains)
import Assembly.Recipe (decodeSpec)
import Assembly.Types (SeatBinding(..))
import Handlers.Simulate (simulate)
import Assembly.Patch (setAccount, setCurve)
import Testkit.Run (orThrow, pacificParts, silent)
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

  describe "Agreements in a Premise" do
    let
      step name actor resource = Tuple name (WorldTemplate { params: [], parties: [ "p" ], transaction: { obligations: [ { id: "x", obligor: Named (Id actor), effect: K.Move { from: Created, to: HeldBy (Bound "p"), resource: Id resource, owed: Exactly Q.zero }, when: [], onShort: Reject, next: noNext } ], ration: { tier: 0, whole: true, scaleGroup: Nothing }, phase: 0, cause: Nothing, consented: [] } })
      withTemplate p (Tuple k v) = p { composition = p.composition { world = p.composition.world { templates = Map.insert k v p.composition.world.templates } } }
    it "load refuses a template naming what the Premise lacks" do
      parts <- pacificParts
      let kept = withTemplate parts (step "grant" "US-private" "T")
      either (const false) (const true) (checkComposition (withTemplate parts (step "bad" "US-private" "nothing"))) `shouldEqual` false
      case checkComposition kept of
        Left e -> fail (show e)
        Right ok -> do
          Map.member "grant" ok.composition.world.templates `shouldEqual` true

  describe "A Strategy that enters an agreement" do
    let
      grant = WorldTemplate { params: [ "amount" ], parties: [ "p" ], transaction: { obligations: [], ration: { tier: 0, whole: true, scaleGroup: Nothing }, phase: 0, cause: Nothing, consented: [] } }
      entering template args parties = { rules: [ { when: [], propose: [ IntentT (Enter { template, args: map (\name -> { name, value: lit 1 }) args, parties: map (\role -> { role, actor: Id "self" }) parties }) ] } ], sign: [] }
      check parts = checkRefs "s" (compositionNames parts.composition)
      withGrant p = p { composition = p.composition { world = p.composition.world { templates = Map.insert "grant" grant p.composition.world.templates } } }
    it "is refused at load for an unknown template, and for a missing argument or party" do
      parts <- withGrant <$> pacificParts
      check parts (entering "grant" [ "amount" ] [ "p" ]) `shouldEqual` Right unit
      check parts (entering "nope" [ "amount" ] [ "p" ]) `shouldEqual` Left (Unresolved "s" RefTemplate "nope")
      check parts (entering "grant" [] [ "p" ]) `shouldEqual` Left (IncompleteAgreement "s" "grant" "amount")
      check parts (entering "grant" [ "amount" ] []) `shouldEqual` Left (IncompleteAgreement "s" "grant" "p")
    it "signing by an unknown template is refused at load" do
      parts <- withGrant <$> pacificParts
      check parts { rules: [], sign: [ SignAgreements [ "grant" ] ] } `shouldEqual` Right unit
      check parts { rules: [], sign: [ SignAgreements [ "nope" ] ] } `shouldEqual` Left (Unresolved "s" RefTemplate "nope")

  describe "Seat presets" do
    it "a human named for a live session must be a configured seat" do
      let config = { seats: Map.singleton (Id "US") "seat-1", nonPlayableActors: Map.empty, representations: [], fallbacks: Map.empty, seed: Seed 1, rounds: 5, preset: Nothing }
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

  describe "Named patches of a composition" do
    it "a patch sets what it names, and fails on a name the composition does not have" do
      p <- pacificParts
      map (\g -> (Map.lookup (Id "US") g.world.actors) >>= Map.lookup (Id "K_T") <<< _.accounts) (setAccount (Id "US") (Id "K_T") (Q.fromInt 7) p.composition)
        `shouldEqual` Right (Just (Q.fromInt 7))
      isLeft (setAccount (Id "NOBODY") (Id "K_T") (Q.fromInt 7) p.composition) `shouldEqual` true
      isLeft (setCurve (Id "no-exchange") { intercept: Q.fromInt 1, slope: Q.zero, domain: { lo: Q.zero, hi: Q.fromInt 1 } } p.composition) `shouldEqual` true
  describe "Assembly (docs/07 §4.3)" do
    let
      assembled p bindings = map (const unit) (assemble (FromComposition p) bindings [])
    it "a binding for something that is not a configured seat is rejected" do
      p <- pacificParts
      assembled p (Map.insert (Id "US-private") (Script []) (silent p)) `shouldEqual` Left (NotASeat (Id "US-private"))
      assembled p (Map.insert (Id "NOBODY") (Script []) (silent p)) `shouldEqual` Left (NotASeat (Id "NOBODY"))
    it "a configured seat with no binding is rejected" do
      p <- pacificParts
      assembled p (Map.delete (Id "CN-firm") (silent p)) `shouldEqual` Left (UnboundSeat (Id "CN-firm"))
    it "a bound StrategyRules's names are checked: a misspelled parameter is an error, not a zero" do
      p <- pacificParts
      let
        typo = ({ rules: [ rule [ gt (param "no-such-parameter") (lit 0) ] [] ], sign: [] })
        bindings = Map.insert (Id "US") (StrategyRules typo) (silent p)
      assembled p bindings `shouldEqual` Left (Unresolved "seat US" RefParameter "no-such-parameter")
    it "a bound StrategyRules naming an unknown Exchange or Actor is an error" do
      p <- pacificParts
      let
        exchange = ({ rules: [ rule [] [ orderAt (named "energy-nowhere") (lit 1) Nothing ] ], sign: [] })
        actor = ({ rules: [ rule [ restrictionInForce { by: "ATLANTIS" } ] [] ], sign: [] })
      assembled p (Map.insert (Id "US") (StrategyRules exchange) (silent p)) `shouldEqual` Left (Unresolved "seat US" RefExchange "energy-nowhere")
      assembled p (Map.insert (Id "US") (StrategyRules actor) (silent p)) `shouldEqual` Left (Unresolved "seat US" RefActor "ATLANTIS")
    it "a literal composition takes no overrides" do
      p <- pacificParts
      let world = (_.world) <$> assemble (FromComposition p) (silent p) []
      w <- orThrow (world)
      map (const unit) (assemble (FromLiteral { world: w, rounds: 1 }) Map.empty [ GameSeed 1 ]) `shouldEqual` Left LiteralOverrides
    it "an override naming an unknown Actor is an error" do
      p <- pacificParts
      map (const unit) (assemblePreset p "Passive" [ Capacity (Id "NOBODY") (Id "energy") (Q.fromInt 1) ] Nothing) `shouldEqual` Left (Unresolved "override" RefActor "NOBODY")
    it "an override naming an unknown capacity, or making an unknown Actor implicit, is an error" do
      p <- pacificParts
      map (const unit) (assemblePreset p "Passive" [ Capacity (Id "US") (Id "no-such-capacity") (Q.fromInt 1) ] Nothing) `shouldEqual` Left (Unresolved "override" RefResource "no-such-capacity")
      map (const unit) (assemblePreset p "Passive" [ Implicit (Id "NOBODY") ] Nothing) `shouldEqual` Left (Unresolved "override" RefActor "NOBODY")
    -- Pins the overrides format stored in a run's spec (docs/07 §3.1).
    it "a recipe read back from a stored spec checks its overrides when it is played" do
      p <- pacificParts
      case decodeOverrides =<< jsonParser """[{"type":"implicit","actor":"NOBODY"}]""" of
        Left e -> fail e
        Right overrides -> map (const unit) (assemblePreset p "Passive" overrides Nothing) `shouldEqual` Left (Unresolved "override" RefActor "NOBODY")
    it "the name check knows every kind of reference" do
      p <- pacificParts
      let
        scope = compositionNames p.composition
        firstStrategy = fromMaybe "" (head (map show (fromFoldable (Map.keys p.composition.strategies) :: Array StrategyId)))
      unknownNames scope [ Tuple RefStrategy "no-such-strategy", Tuple RefRuleKind "no-such-kind" ]
        `shouldEqual` [ UnknownReference RefStrategy "no-such-strategy", UnknownReference RefRuleKind "no-such-kind" ]
      unknownNames scope [ Tuple RefStrategy firstStrategy, Tuple RefRuleKind "us-import-tariff" ] `shouldEqual` []
    it "an Agent or Human seat cannot be simulated" do
      p <- pacificParts
      g <- orThrow (assemble (FromComposition p) (Map.insert (Id "US") Agent (silent p)) [])
      case simulate (Seed 1) g of
        Left e -> e `shouldEqual` NotSimulable (Id "US")
        Right _ -> fail "expected NotSimulable"

  describe "Overrides (docs/07 §3.1)" do
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
    -- Pins the overrides format stored in a run's spec (docs/07 §3.1).
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
        spec rounds = bimap printJsonDecodeError identity (Shape.parse Shape.Authored specShape ("{\"preset\":\"Restrict\",\"playerSeed\":\"1\"" <> rounds <> "}")) >>= decodeSpec
      isLeft (spec ",\"rounds\":\"default\"") `shouldEqual` true
      map _.rounds (spec "") `shouldEqual` Right Nothing
      map _.rounds (spec ",\"rounds\":\"3\"") `shouldEqual` Right (Just 3)
      (lmapContains (map _.rounds (spec ",\"rounds\":\"lots\""))) `shouldEqual` true

-- | The stored rounds decode failure names the value and the field.
lmapContains :: forall a. Either String a -> Boolean
lmapContains = case _ of
  Left e -> contains (Pattern "the stored spec's rounds is not an integer: lots") e
  Right _ -> false
