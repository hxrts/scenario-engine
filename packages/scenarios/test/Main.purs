module Test.Scenarios.Main where

import Prelude

import Board.Decode.Common as BD
import Board.Decode.Intents as BDI
import Board.Fixture (Field(..), FixtureError(..), checkFixture)
import Board.Gameboard.Instantiate (instantiate)
import Board.Gameboard.Reconcile (reconcile)
import Board.Gameboard.Schema (Difference(..), Fallback(..), Gameboard, InstantiationConfig, RefKind(..), ValidationError(..))
import Board.Gameboard.Validate (validateGameboard)
import Core.Num (Q)
import Core.Num as Q
import Core.Types (Capability(..), Controller(..), Entity, EntityKind(..), Id(..), Intent(..), Rule(..), RuleChange(..), RuleKind(..))
import Core.World (Event(..), ShareSource(..), View, WorldState)
import Data.Argonaut.Core (Json, stringify, toString)
import Data.Argonaut.Decode (printJsonDecodeError)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (any, elem, filter, fromFoldable, length)
import Data.Either (Either(..), isRight)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign.Object (Object)
import Foreign.Object as Object
import Policy.Interpret (Env, propose)
import Policy.Program (Program, SignRule(..), decodeProgram, programRefs)
import DSL.Base (q, keyed, builtJson, trade, transfer, transform, enactTariff, enactAccessBan, enactWithholding, holdingOf)
import DSL.World (atLeast, own)
import DSL.Decision (deal, lit, max', param, program, rule, signProcedures, tradeT, transformT)
import DSL.Fixture (enactAccessBanI, enactWithholdingI, intentJson, tariffRule, withholdingRule)
import Adapters.Scenarios (readScenario)
import Scenarios.Load (LoadError(..), checkPresets, decodePresets, fromFiles, load)
import Scenarios.FracturedWorld (files)
import Scenarios.FracturedWorld.Policies (policies)
import Scenarios.Presets as Presets
import Test.Scenarios.Golden (failWith, fixture, golden)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

-- | Initial holdings of the reference instance (docs/05_test_instance.md §15).
holdings :: Array { entity :: String, m :: Int, e :: Int, mil :: Int, s :: Int }
holdings =
  [ { entity: "US", m: 260, e: 70, mil: 95, s: 60 }
  , { entity: "DE", m: 125, e: 38, mil: 24, s: 45 }
  , { entity: "IN", m: 180, e: 65, mil: 65, s: 35 }
  , { entity: "JP", m: 150, e: 40, mil: 36, s: 48 }
  , { entity: "UK", m: 130, e: 30, mil: 38, s: 47 }
  , { entity: "BR", m: 105, e: 85, mil: 25, s: 32 }
  , { entity: "SA", m: 155, e: 110, mil: 32, s: 28 }
  , { entity: "NG", m: 85, e: 60, mil: 18, s: 24 }
  , { entity: "ENCO", m: 140, e: 140, mil: 0, s: 20 }
  , { entity: "CHIP", m: 135, e: 55, mil: 0, s: 30 }
  , { entity: "SHIP", m: 110, e: 65, mil: 0, s: 25 }
  , { entity: "DEF", m: 110, e: 30, mil: 5, s: 18 }
  , { entity: "LABOR", m: 30, e: 4, mil: 0, s: 42 }
  , { entity: "SME", m: 15, e: 5, mil: 0, s: 12 }
  , { entity: "AID", m: 45, e: 12, mil: 0, s: 55 }
  , { entity: "CN", m: 255, e: 95, mil: 88, s: 57 }
  , { entity: "EU", m: 320, e: 125, mil: 50, s: 75 }
  , { entity: "CN_PRIVATE", m: 210, e: 85, mil: 0, s: 25 }
  , { entity: "US_WORKERS", m: 260, e: 60, mil: 0, s: 45 }
  , { entity: "GLOBAL_SMES", m: 300, e: 80, mil: 0, s: 25 }
  , { entity: "ENERGY_SUPPLIERS", m: 200, e: 240, mil: 0, s: 22 }
  , { entity: "WORLD_HOUSEHOLDS", m: 360, e: 90, mil: 0, s: 35 }
  ]

-- | The reference instance's Gameboard and configuration, as the loader reads them.
loaded :: Aff { gameboard :: Gameboard, config :: InstantiationConfig }
loaded = do
  r <- liftEffect (map (_ >>= load) (readScenario "fractured-world/v0"))
  case r of
    Left e -> failWith e
    Right x -> pure x

-- | The reference instance's Fractured World files, as name-and-JSON sections.
sections :: Array { name :: String, json :: Json }
sections = map (\(Tuple name json) -> { name, json }) files

-- | The sections with one replaced.
withSection :: String -> Json -> Array { name :: String, json :: Json }
withSection name json = map (\s -> if s.name == name then s { json = json } else s) sections

-- | The shipped seat presets, decoded.
shippedPresets :: Either String (Object Program)
shippedPresets = decodePresets (encodeJson Presets.presets)

-- | China in round 2, after a major US action in round 1, with the capability to procure.
chinaView :: View
chinaView =
  { round: 2
  , self: { id: Id "CN", accounts: Map.empty, capacities: Map.empty, capabilities: [ MayTransform (Id "defense-procurement-cn"), MayTrade (Id "energy-asia") ], kind: Actor (NPC (Id "china-strategic-v0")) }
  , prices: Map.fromFoldable [ Tuple (Id "energy-asia") (Q.fromInt 2) ]
  , rules: []
  , events: [ Transformed { round: 1, by: Id "US", transformation: Id "major-military-action", quantity: Q.fromInt 12, paid: Q.zero } ]
  , factors: Map.empty
  , toSign: []
  }

-- | An environment whose every draw is `u`, with `$home` at energy-asia.
envWith :: View -> Q -> Env
envWith v u = { view: v, params: Object.empty, home: Just (Id "energy-asia"), random: const u }

-- | The Policy named in the catalog, decoded.
policyNamed :: Gameboard -> String -> Aff Program
policyNamed gb name = case Object.lookup name gb.policies of
  Nothing -> failWith ("no policy named " <> name)
  Just j -> case decodeProgram j of
    Left e -> failWith (printJsonDecodeError e)
    Right p -> pure p

-- | A load outcome without its (non-comparable) scenario, for equality checks.
outcome :: forall a. Either LoadError a -> Either LoadError Unit
outcome = map (const unit)

-- Edits for the validation tests --------------------------------------------------------

type Edit = Gameboard -> InstantiationConfig -> Tuple Gameboard InstantiationConfig

editWorld :: (WorldState -> WorldState) -> Edit
editWorld f g c = Tuple g { world = f g.world } c

editConfig :: (InstantiationConfig -> InstantiationConfig) -> Edit
editConfig f g c = Tuple g (f c)

-- | One Entity of the world, edited.
onEntity :: String -> (Entity -> Entity) -> WorldState -> WorldState
onEntity i f w = w { entities = Map.update (Just <<< f) (Id i) w.entities }

-- | An Aggregate whose every procedure weighs an extra, unknown Entity.
withWeightOf :: String -> Entity -> Entity
withWeightOf name e = case e.kind of
  Aggregate a -> e { kind = Aggregate a { procedures = map (\p -> p { weights = Map.insert (Id name) (Q.fromInt 1) p.weights }) a.procedures } }
  _ -> e

-- | A test that the edited reference instance fails validation with exactly this error.
rejects :: String -> Edit -> ValidationError -> Spec Unit
rejects name edit err = it name do
  { gameboard, config } <- loaded
  let Tuple gb cfg = edit gameboard config
  case validateGameboard gb cfg of
    Left es -> (err `elem` es) `shouldEqual` true
    Right _ -> fail ("expected " <> show err)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  golden

  describe "Scenario DSL builders" do
    it "max' and signProcedures compile to a program the decoder accepts" do
      let
        p = program
          { rules: [ rule [] [ transformT { transformation: "defense-procurement", quantity: max' (lit 1) (lit 2) } ] ]
          , sign: [ signProcedures [ "EU" ] ]
          }
      case decodeProgram (builtJson p) of
        Left e -> fail (show e)
        Right prog -> do
          length prog.rules `shouldEqual` 1
          case prog.sign of
            [ SignProcedures ids ] -> ids `shouldEqual` [ Id "EU" ]
            _ -> fail "expected one procedures sign rule"
    it "enactAccessBanI decodes to an AccessBan enactment" do
      case BDI.intent (intentJson (enactAccessBanI { jurisdiction: "US", target: "CN" })) of
        Left e -> fail (show e)
        Right (ChangeRule (Enact (AccessBan a))) -> do
          a.jurisdiction `shouldEqual` Id "US"
          a.target `shouldEqual` Id "CN"
        Right other -> fail ("unexpected intent " <> show other)
    it "tariffRule decodes to a Tariff" do
      case BDI.rule (builtJson (tariffRule { jurisdiction: "US", origin: "CN", rate: q "0.25" })) of
        Left e -> fail (show e)
        Right (Tariff t) -> do
          t.jurisdiction `shouldEqual` Id "US"
          t.origin `shouldEqual` Id "CN"
        Right other -> fail ("unexpected rule " <> show other)
    it "a withholding rule and an enactment of one decode to the Withholding rule" do
      BDI.rule (builtJson (withholdingRule { capacity: "semiconductors", units: q 3 }))
        `shouldEqual` Right (Withholding { capacity: Id "semiconductors", units: Q.fromInt 3 })
      BDI.intent (intentJson (enactWithholdingI { capacity: "semiconductors", units: q 3 }))
        `shouldEqual` Right (ChangeRule (Enact (Withholding { capacity: Id "semiconductors", units: Q.fromInt 3 })))
    it "every capability the DSL writes is a tag the Gameboard decoder reads" do
      for_ [ trade "energy-atlantic", transfer "M", transform "energy-shipping", enactTariff, enactAccessBan, enactWithholding ] \c ->
        case toString (encodeJson c) of
          Nothing -> fail "a capability is not a string"
          Just s -> isRight (BD.capability s) `shouldEqual` true
    it "a deal decodes, and the parties and Resource it names are reported" do
      let
        p = program
          { rules: [ rule [] [ deal { signers: [ "DE" ], transfers: [ { from: "self", to: "DE", resource: "E", quantity: lit 2 } ] } ] ] }
      case decodeProgram (builtJson p) of
        Left e -> fail (show e)
        Right prog -> do
          let refs = programRefs prog
          refs.resources `shouldEqual` [ "E" ]
          refs.entities `shouldEqual` [ "DE", "DE" ]
    it "a bound given as a metric is the metric object itself" do
      stringify (builtJson (atLeast (own "CN" (holdingOf "Mil")))) `shouldEqual` stringify (builtJson (own "CN" (holdingOf "Mil")))

  describe "The Fractured World" do
    let
      instantiated = do
        { gameboard, config } <- loaded
        case instantiate gameboard config of
          Left es -> failWith (show es)
          Right x -> pure { gameboard, config, initial: x }
      amount w e r = fromMaybe Q.zero (Map.lookup (Id e) w.entities >>= \x -> Map.lookup (Id r) x.accounts)
      capacity w e c = fromMaybe Q.zero (Map.lookup (Id e) w.entities >>= \x -> Map.lookup (Id c) x.capacities)

    it "validates, and its shipped presets resolve against it" do
      { gameboard, config } <- loaded
      map (const unit) (validateGameboard gameboard config) `shouldEqual` Right unit
      case shippedPresets of
        Left e -> fail e
        Right ps -> checkPresets gameboard ps `shouldEqual` Right unit

    it "instantiates with the initial holdings of §15" do
      { initial } <- instantiated
      for_ holdings \h -> do
        let w = initial.world
        { entity: h.entity, m: amount w h.entity "M", e: amount w h.entity "E", mil: amount w h.entity "Mil", s: amount w h.entity "S" }
          `shouldEqual` { entity: h.entity, m: Q.fromInt h.m, e: Q.fromInt h.e, mil: Q.fromInt h.mil, s: Q.fromInt h.s }

    it "instantiates the market and capacity state of §16" do
      { initial } <- instantiated
      let w = initial.world
      map (\m -> m.reserves) (Map.lookup (Id "energy-atlantic") w.markets) `shouldEqual` Just (Map.fromFoldable [ Tuple (Id "E") (Q.fromInt 350), Tuple (Id "M") (Q.fromInt 700) ])
      map (\m -> m.reserves) (Map.lookup (Id "energy-asia") w.markets) `shouldEqual` Just (Map.fromFoldable [ Tuple (Id "E") (Q.fromInt 250), Tuple (Id "M") (Q.fromInt 500) ])
      capacity w "CHIP" "semiconductors" `shouldEqual` Q.fromInt 15
      capacity w "SHIP" "transport" `shouldEqual` Q.fromInt 60
      map (\x -> x.kind) (Map.lookup (Id "CN") w.entities) `shouldEqual` Just (Actor (NPC (Id "china-strategic-v0")))
      length w.representations `shouldEqual` 1

    it "the production list states exactly the energy capacity the Entities hold" do
      { gameboard, initial } <- instantiated
      let
        w = initial.world
        listed = map (\p -> capacity w (unwrap p.entity) (unwrap p.capacity)) gameboard.world.circulation.production
        held = map (\e -> fromMaybe Q.zero (Map.lookup (Id "energy-production") e.capacities)) (fromFoldable (Map.values w.entities))
      Q.sumQ listed `shouldEqual` Q.sumQ held

    it "reconciles Germany explicit against Germany implicit (§14.2)" do
      { gameboard, config, initial } <- instantiated
      case instantiate gameboard config { seats = Map.delete (Id "DE") config.seats } of
        Left es -> fail (show es)
        Right implicit -> do
          let eu w = map (amount w "EU") [ "M", "E", "Mil", "S" ]
          eu implicit.world `shouldEqual` map Q.fromInt [ 445, 163, 74, 120 ]
          reconcile initial implicit `shouldEqual` Right unit
          let
            residual p = Map.lookup (Id "EU") implicit.world.entities >>= \x -> case x.kind of
              Aggregate a -> map _.residualWeight (Map.lookup (Id p) a.procedures)
              _ -> Nothing
          residual "eu-solidarity" `shouldEqual` Q.parse "0.6"
          residual "eu-trade" `shouldEqual` Q.parse "0.68"

  describe "Gameboard validation: each check reports its own error" do
    rejects "UnknownReference: a market access list names no Entity"
      (editWorld \w -> w { markets = Map.update (\m -> Just m { access = m.access <> [ Id "NOBODY" ] }) (Id "energy-atlantic") w.markets })
      (UnknownReference EntityRef "NOBODY")
    rejects "UnknownReference: a market's quote is not a Resource"
      (editWorld \w -> w { markets = Map.update (\m -> Just m { quote = Id "X" }) (Id "energy-asia") w.markets })
      (UnknownReference ResourceRef "X")
    rejects "UnknownReference: a facility's payee is not an Entity"
      (editWorld \w -> w { transformations = Map.update (\t -> Just t { payee = Just (Id "NOBODY") }) (Id "energy-shipping") w.transformations })
      (UnknownReference EntityRef "NOBODY")
    rejects "UnknownReference: a jurisdiction is not an Entity"
      (editWorld \w -> w { jurisdiction = Map.insert (Id "DE") (Id "NOBODY") w.jurisdiction })
      (UnknownReference EntityRef "NOBODY")
    rejects "UnknownReference: a Representation scheme's domain is not a Resource"
      (\g c -> Tuple g { schemes = map (\s -> s { domain = Id "NOPE" }) g.schemes } c)
      (UnknownReference ResourceRef "NOPE")
    rejects "UnknownReference: an Aggregate member is not an Entity"
      (editWorld (onEntity "EU" \e -> e { kind = Aggregate { policy: Id "eu-coordination-v0", members: [ Id "NOBODY" ], procedures: Map.empty } }))
      (UnknownReference EntityRef "NOBODY")
    rejects "UnknownReference: an Actor's NPC policy is not in the catalog"
      (\g c -> Tuple g { actors = Map.update (\d -> Just d { npc = [ Id "no-such-policy" ] }) (Id "CN") g.actors } c)
      (UnknownReference PolicyRef "no-such-policy")
    rejects "UnknownReference: a Withholding rule names a capacity no Entity holds"
      (editWorld \w -> w { rules = Map.insert (Id "rule-x") (Withholding { capacity: Id "nowhere", units: Q.fromInt 1 }) w.rules })
      (UnknownReference CapacityName "nowhere")
    rejects "MissingFallback: a non-required Actor with no fallback"
      (\g c -> Tuple g { actors = Map.update (\d -> Just d { fallback = Nothing }) (Id "DE") g.actors } c)
      (MissingFallback (Id "DE"))
    rejects "MissingObjective: an Actor with no objective"
      (\g c -> Tuple g { actors = Map.update (\d -> Just d { objective = Nothing }) (Id "DE") g.actors } c)
      (MissingObjective (Id "DE"))
    rejects "DuplicateCapacityOwner: two Entities hold the same capacity"
      (editWorld (onEntity "SA" \e -> e { capacities = Map.insert (Id "semiconductors") (Q.fromInt 1) e.capacities }))
      (DuplicateCapacityOwner "semiconductors")
    rejects "FacilityWithoutPayee: a facility with a cost and no payee"
      (editWorld \w -> w { transformations = Map.update (\t -> Just t { payee = Nothing }) (Id "public-attention") w.transformations })
      (FacilityWithoutPayee (Id "public-attention"))
    rejects "ProcedureExceedsAuthority: a procedure authorizes what its Aggregate may not do"
      (editWorld (onEntity "EU" \e -> e { capabilities = filter (_ /= MayEnact TariffRule) e.capabilities }))
      (ProcedureExceedsAuthority (Id "EU"))
    rejects "ProductionWithoutCapacity: a producer that does not hold the capacity it produces from"
      (editWorld \w -> w { circulation = w.circulation { production = w.circulation.production <> [ { entity: Id "US", resource: Id "E", capacity: Id "energy-production" } ] } })
      (ProductionWithoutCapacity (Id "US") "energy-production")
    rejects "CoverageNotAggregate: a coverage fallback that names an Entity which is not an Aggregate"
      (\g c -> Tuple g { actors = Map.update (\d -> Just d { fallback = Just (ImplicitCoverage (Id "US")) }) (Id "DE") g.actors } c)
      (CoverageNotAggregate (Id "DE") (Id "US"))
    rejects "SchemeTargetNotAggregate: a scheme whose target is not an Aggregate"
      (\g c -> Tuple g { schemes = map (\s -> s { target = Id "US" }) g.schemes } c)
      (SchemeTargetNotAggregate "sme-energy-sample" (Id "US"))
    rejects "RequiredNotSelected: a required Actor with no seat or NPC"
      (editConfig \c -> c { seats = Map.delete (Id "US") c.seats })
      (RequiredNotSelected (Id "US"))
    rejects "ControllerNotAllowed: a seat for an Entity that is not an Actor"
      (editConfig \c -> c { seats = Map.insert (Id "NOBODY") "seat-99" c.seats })
      (ControllerNotAllowed (Id "NOBODY"))
    rejects "IneligibleRepresentative: a selected Actor the scheme does not cover"
      (editConfig \c -> c { representations = c.representations <> [ { scheme: "sme-energy-sample", actor: Id "US", weight: Q.fromInt 1 } ] })
      (IneligibleRepresentative (Id "US"))
    rejects "RoundsOutOfRange: a configured round count outside the supported range"
      (editConfig \c -> c { rounds = 9 })
      (RoundsOutOfRange { rounds: 9, min: 4, max: 8 })
    rejects "ReservesNonReservable: a facility reserves Money, which is not reservable"
      (editWorld \w -> w { transformations = Map.update (\t -> Just t { reserves = Map.insert (Id "M") (Q.fromInt 1) t.reserves }) (Id "military-deployment") w.transformations })
      (ReservesNonReservable (Id "military-deployment") (Id "M"))
    rejects "UnknownReference: a market's base is not a Resource"
      (editWorld \w -> w { markets = Map.update (\m -> Just m { base = Id "X" }) (Id "energy-atlantic") w.markets })
      (UnknownReference ResourceRef "X")
    rejects "UnknownReference: a market's reserve is not a Resource"
      (editWorld \w -> w { markets = Map.update (\m -> Just m { reserves = Map.insert (Id "X") (Q.fromInt 1) m.reserves }) (Id "energy-asia") w.markets })
      (UnknownReference ResourceRef "X")
    rejects "UnknownReference: a facility input is not a Resource"
      (editWorld \w -> w { transformations = Map.update (\t -> Just t { inputs = Map.insert (Id "X") (Q.fromInt 1) t.inputs }) (Id "defense-procurement") w.transformations })
      (UnknownReference ResourceRef "X")
    rejects "UnknownReference: a facility output is not a Resource"
      (editWorld \w -> w { transformations = Map.update (\t -> Just t { outputs = Map.insert (Id "X") (Q.fromInt 1) t.outputs }) (Id "defense-procurement") w.transformations })
      (UnknownReference ResourceRef "X")
    rejects "UnknownReference: a facility effect names a capacity no Entity holds"
      (editWorld \w -> w { transformations = Map.update (\t -> Just t { effects = map (\x -> x { capacity = Id "nowhere" }) t.effects }) (Id "strike") w.transformations })
      (UnknownReference CapacityName "nowhere")
    rejects "UnknownReference: a facility effect's holder is not an Entity"
      (editWorld \w -> w { transformations = Map.update (\t -> Just t { effects = map (\x -> x { holder = Just (Id "NOBODY") }) t.effects }) (Id "strike") w.transformations })
      (UnknownReference EntityRef "NOBODY")
    rejects "UnknownReference: an Entity account in a Resource that does not exist"
      (editWorld (onEntity "US" \e -> e { accounts = Map.insert (Id "X") (Q.fromInt 1) e.accounts }))
      (UnknownReference ResourceRef "X")
    rejects "UnknownReference: an Entity capacity no facility or production uses"
      (editWorld (onEntity "US" \e -> e { capacities = Map.insert (Id "unused") (Q.fromInt 1) e.capacities }))
      (UnknownReference CapacityName "unused")
    rejects "UnknownReference: a production entry's Resource that does not exist"
      (editWorld \w -> w { circulation = w.circulation { production = map (\p -> p { resource = Id "X" }) w.circulation.production } })
      (UnknownReference ResourceRef "X")
    rejects "UnknownReference: a flow from an Entity that does not exist"
      (editWorld \w -> w { circulation = w.circulation { flows = w.circulation.flows <> [ { from: Id "NOBODY", to: Id "US", resource: Id "M", amount: Q.fromInt 1 } ] } })
      (UnknownReference EntityRef "NOBODY")
    rejects "UnknownReference: a flow of a Resource that does not exist"
      (editWorld \w -> w { circulation = w.circulation { flows = w.circulation.flows <> [ { from: Id "US", to: Id "DE", resource: Id "X", amount: Q.fromInt 1 } ] } })
      (UnknownReference ResourceRef "X")
    rejects "UnknownReference: a share sourced from a Resource that does not exist"
      (editWorld \w -> w { circulation = w.circulation { shares = w.circulation.shares <> [ { from: Id "US", to: Id "DE", share: Q.fromInt 1, source: SalesOf (Id "X") } ] } })
      (UnknownReference ResourceRef "X")
    rejects "UnknownReference: a requirement on an Entity that does not exist"
      (editWorld \w -> w { circulation = w.circulation { requirements = w.circulation.requirements <> [ { entity: Id "NOBODY", resource: Id "E", amount: Just (Q.fromInt 1), penalty: Q.zero } ] } })
      (UnknownReference EntityRef "NOBODY")
    rejects "UnknownReference: an access ban whose target is not an Entity"
      (editWorld \w -> w { rules = Map.insert (Id "rule-y") (AccessBan { jurisdiction: Id "US", target: Id "NOBODY" }) w.rules })
      (UnknownReference EntityRef "NOBODY")
    rejects "UnknownReference: a tariff whose origin is not an Entity"
      (editWorld \w -> w { rules = Map.insert (Id "rule-z") (Tariff { jurisdiction: Id "US", origin: Id "NOBODY", rate: Q.zero }) w.rules })
      (UnknownReference EntityRef "NOBODY")
    rejects "UnknownReference: a transport entry whose facility does not exist"
      (editWorld \w -> w { transport = Map.insert (Id "E") (Id "no-such-facility") w.transport })
      (UnknownReference TransformationRef "no-such-facility")
    rejects "UnknownReference: a procedure weight on an Entity that does not exist"
      (editWorld (onEntity "EU" (withWeightOf "NOBODY")))
      (UnknownReference EntityRef "NOBODY")
    it "ResolutionInconsistent: an implicit Actor whose authority its Aggregate would not keep" do
      { gameboard, config } <- loaded
      -- DE may enact tariffs, but once the EU covers DE, DE's authority disappears
      let
        gb = gameboard { world = gameboard.world { entities = Map.update (\e -> Just e { capabilities = e.capabilities <> [ MayEnact TariffRule ] }) (Id "DE") gameboard.world.entities } }
      case validateGameboard gb config of
        Left es -> (ResolutionInconsistent (Id "DE") [ AuthorityDiffers ] `elem` es) `shouldEqual` true
        Right _ -> fail "expected ResolutionInconsistent"
    it "a valid Gameboard reports no errors" do
      { gameboard, config } <- loaded
      map (const unit) (validateGameboard gameboard config) `shouldEqual` Right unit

  describe "Shipped Policies and presets" do
    it "china-strategic-v0 procures after a major US action, and only with the capability" do
      { gameboard } <- loaded
      p <- policyNamed gameboard "china-strategic-v0"
      map _.intents (propose p (envWith chinaView Q.zero))
        `shouldEqual` [ [ Transform { by: Id "CN", transformation: Id "defense-procurement-cn", quantity: Q.fromInt 4 } ] ]
      map _.intents (propose p (envWith (chinaView { self = chinaView.self { capabilities = [] } }) Q.zero)) `shouldEqual` []
    it "RandomValid's choose picks an option by its own seeded draw" do
      case decodePresets (encodeJson Presets.presets) of
        Left e -> fail e
        Right ps -> case Object.lookup "RandomValid" ps of
          Nothing -> fail "no RandomValid preset"
          Just p -> do
            let
              sme = chinaView { self = chinaView.self { id = Id "SME", capabilities = [ MayTrade (Id "energy-asia") ] } }
              at u = map _.intents (propose p (envWith sme u))
            -- three options: noAction, buy 5 at 3, sell 5 at 2; a draw of 0.9 picks the last
            at Q.zero `shouldEqual` []
            at (fromMaybe Q.zero (Q.parse "0.9")) `shouldEqual` [ [ Trade { market: Id "energy-asia", quantity: Q.fromInt (-5), limitPrice: Just (Q.fromInt 2) } ] ]

  describe "Reservations in the DSL" do
    it "the shipped military-deployment reserves Mil per unit and consumes and produces nothing" do
      { gameboard } <- loaded
      case Map.lookup (Id "military-deployment") gameboard.world.transformations of
        Nothing -> fail "military-deployment is missing"
        Just t -> do
          Map.toUnfoldable t.reserves `shouldEqual` [ Tuple (Id "Mil") (Q.fromInt 1) ]
          Map.isEmpty t.inputs `shouldEqual` true
          Map.isEmpty t.outputs `shouldEqual` true
    it "the Deployer preset proposes a reserving deployment and a Mil sale only while the Atlantic price is above 3" do
      case decodePresets (encodeJson Presets.presets) of
        Left e -> fail e
        Right ps -> case Object.lookup "Deployer" ps of
          Nothing -> fail "no Deployer preset"
          Just p -> do
            let
              us = chinaView { self = chinaView.self { id = Id "US", capabilities = [ MayTransform (Id "military-deployment"), MayTrade (Id "defence-goods") ] } }
              at price = map _.intents (propose p (envWith (us { prices = Map.fromFoldable [ Tuple (Id "energy-atlantic") (fromMaybe Q.zero (Q.parse price)) ] }) Q.zero))
            at "3" `shouldEqual` []
            -- one Proposal per template: the deployment, then the Mil sale it competes with
            at "3.01" `shouldEqual`
              [ [ Transform { by: Id "US", transformation: Id "military-deployment", quantity: Q.fromInt 60 } ]
              , [ Trade { market: Id "defence-goods", quantity: Q.fromInt (-40), limitPrice: Just (Q.parse "2.4" # fromMaybe Q.zero) } ]
              ]

  describe "Scenario loader: files and policies fail at load time" do
    it "a key that two section files both define is an error, not an overwrite" do
      outcome (fromFiles (sections <> [ { name: "extra.json", json: encodeJson { resources: Object.empty :: Object Json } } ]))
        `shouldEqual` Left (DuplicateKey "extra.json" "resources")
    it "a section that is not a JSON object is an error" do
      outcome (fromFiles (withSection "scenario.json" (encodeJson "not an object")))
        `shouldEqual` Left (NotAnObject "scenario.json")
    it "a policy that names an unknown market fails the load" do
      let
        bad = program { rules: [ rule [] [ tradeT { market: "energy-nowhere", quantity: lit 1 } ] ] }
        catalog = policies <> keyed [ Tuple "bad-v0" bad ]
      outcome (fromFiles (withSection "policies.json" (encodeJson { policies: catalog })))
        `shouldEqual` Left (Unresolved "policy bad-v0" MarketRef "energy-nowhere")
    it "a policy that reads an unknown parameter fails the load" do
      let
        bad = program { rules: [ rule [] [ tradeT { market: "energy-atlantic", quantity: param "no-such-parameter" } ] ] }
        catalog = policies <> keyed [ Tuple "bad-v0" bad ]
      outcome (fromFiles (withSection "policies.json" (encodeJson { policies: catalog })))
        `shouldEqual` Left (Unresolved "policy bad-v0" ParameterRef "no-such-parameter")
    it "a preset that names an unknown transformation fails the check" do
      { gameboard } <- loaded
      let bad = program { rules: [ rule [] [ transformT { transformation: "no-such-facility", quantity: lit 1 } ] ] }
      case decodeProgram (builtJson bad) of
        Left e -> fail (show e)
        Right p -> checkPresets gameboard (Object.singleton "Bad" p)
          `shouldEqual` Left (Unresolved "preset Bad" TransformationRef "no-such-facility")

  describe "Fixture expectations" do
    it "an asserted empty list of deliveries fails when a delivery happened" do
      f <- fixture "F"
      let asserted = f { rounds = map (\r -> r { expect = r.expect { deliveries = Just [] } }) f.rounds }
      any isDeliveryMismatch (checkFixture asserted) `shouldEqual` true
    it "an asserted empty list of deliveries passes when nothing was delivered" do
      f <- fixture "A"
      let asserted = f { rounds = map (\r -> r { expect = r.expect { deliveries = Just [] } }) f.rounds }
      checkFixture asserted `shouldEqual` []
  where
  isDeliveryMismatch = case _ of
    Mismatch _ Deliveries _ _ -> true
    _ -> false
