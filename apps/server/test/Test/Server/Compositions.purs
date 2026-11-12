-- | A second PremiseComposition, built only from the Kit: compiled, loaded, played, viewed, and compared with Meridian
-- | as a batch.
-- | Pins: the numbers of this suite's own small Kit-built Composition (docs/03 §12); no docs/06 values.
module Test.Server.Compositions (spec) where

import Prelude

import Core.Shape (kindsNamed)
import Data.Array (fromFoldable, sort)
import Data.Map as Map
import Handlers.Llm.Call (interpretCall)
import Handlers.Llm.Joint (jointRequest)
import Laws.Types (idText, Actor, ActorId, Id(..), Transferability(..))
import Data.Maybe (Maybe(..))

import Test.Server.Host (testHost)

import Testkit.Store (open, storeOver)
import Core.Draw (Seed(..))
import Laws.World (Focus)
import Data.Either (Either(..), either, isLeft)
import Data.String (Pattern(..), contains, joinWith)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Class (liftEffect)
import Foreign.Object as Object
import Handlers.Api.Route (route)
import Premises.Compose (CompositionSource, premiseOf)
import Premises.Kit.Seats (Cover(..), seat)
import Premises.Kit.Holdings (HoldingTable)
import Premises.Kit.Exchanges (exchange)
import Premises.Kit.Presets (PresetBinding, presetsOf, standardThresholds)
import Premises.Load (composedOf, parts)
import Schema.Build.Decision (lit)
import Schema.Composition (ActorDefinition, PremiseComposition)
import Schema.Build.Base (holdingOf, keyed, mayTrade, mayTransfer, (:=))
import Schema.Build.Quantity (q)
import Schema.Build.World (atLeast, byKey, emptyCirculation, emptyWorld, actorsOf, finishes, noResource, resourcesOf)
import Server.OpenApi (documentsOf)
import Server.Stack (implement, newServerCache)
import Data.Argonaut.Core (stringify)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldSatisfy)
import Testkit.Meridian (meridianParts)
import Premises.Pacific as Pacific

focus :: Focus
focus = { resource: Id "C", label: "Corn" }

table :: HoldingTable
table =
  [ Tuple (Id "A") [ Id "G" := 100, Id "C" := 10 ]
  , Tuple (Id "B") [ Id "G" := 80, Id "C" := 20 ]
  ]

seats :: Array (Tuple ActorId (Tuple Actor ActorDefinition))
seats = map (\k -> seat table k spec') [ Id "A", Id "B" ]
  where
  spec' =
    { capabilities: [ mayTrade "corn", mayTransfer "G", mayTransfer "C" ]
    , required: true
    , cover: Unassigned
    , goal: "Stay solvent"
    , criteria: [ finishes "A" (holdingOf "G") (atLeast 1) ]
    , description: "A trader"
    , dependency: "The corn turn"
    }

harborWorld :: PremiseComposition
harborWorld =
  { id: "harbor"
  , version: "v0"
  , premises: [ { id: "harbor", version: "v0" } ]
  , horizon: 3
  , rounds: { min: 2, max: 4 }
  , label: Just "harbor-01"
  , parameters: Object.empty
  , schemes: []
  , strategies: Map.empty
  , explicitActors: keyed (map (map snd) seats)
  , authored: { circulation: emptyCirculation, reservable: [], decays: [] }
  , world: emptyWorld
      { numeraire = Id "G"
      , focus = Just focus
      , resources = resourcesOf [ Tuple "G" (noResource { transferability = Free }), Tuple "C" (noResource { transferability = Free }) ]
      , exchanges = byKey [ Tuple "corn" (exchange { base: Id "C", quote: Id "G", cost: { intercept: q 2, slope: q "0.01", domain: { lo: q (-50), hi: q 50 } }, reserves: keyed [ "G" := 500, "C" := 200 ] } [ Id "A", Id "B" ]) ]
      , actors = actorsOf (map (map fst) seats)
      }
  }

harbor :: CompositionSource
harbor =
  { id: "harbor"
  , version: "v0"
  , premises: [ premiseOf { id: "harbor", version: "v0" } harborWorld ]
  , config: { seed: Seed 7, rounds: 3, seats: keyed [ Tuple "A" "seat-1", Tuple "B" "seat-2" ], nonPlayableActors: Map.empty, fallbacks: Map.empty, representations: [], preset: Nothing }
  , presets: presetsOf harborBinding
  }

harborBinding :: PresetBinding
harborBinding =
  { staple: Id "C"
  , money: Id "G"
  , build: Nothing
  , invest: Nothing
  , deploy: Nothing
  , thresholds: standardThresholds
  }

spec :: Spec Unit
spec = describe "A second Premise" do
  it "built only from the Kit, it splits into sections, loads, and checks" do
    case composedOf { id: harbor.id, version: harbor.version, premises: harbor.premises }, parts harbor of
      Right d, Right _ -> d.id `shouldEqual` "harbor"
      _, Left e -> fail e
      Left e, _ -> fail e
  it "a Composition's OpenAPI document lists its own Resource ids, and not another Composition's" do
    meridian <- meridianParts
    case parts harbor of
      Left e -> fail e
      Right h -> do
        let
          doc name = either identity (\ds -> joinWith "" (map (stringify <<< snd) ds)) (documentsOf [ meridian, h ] { composition: name, server: "s" })
        doc "harbor" `shouldSatisfy` contains (Pattern "\"enum\":[\"C\",\"G\"]")
        doc "harbor" `shouldSatisfy` (not <<< contains (Pattern "\"enum\":[\"E\",\"M\",\"Mil\",\"S\",\"attention\",\"energy-production\",\"semiconductors\",\"transport\"]"))
        doc "meridian" `shouldSatisfy` contains (Pattern "\"enum\":[\"E\",\"M\",\"Mil\",\"S\",\"attention\",\"energy-production\",\"semiconductors\",\"transport\"]")
        doc "meridian" `shouldSatisfy` (not <<< contains (Pattern "\"enum\":[\"C\",\"G\"]"))
  it "Pacific is listed with its own presets, launched, and its Dependence view served" do
    meridian <- meridianParts
    case parts (Pacific.pacific Pacific.defaultDials) of
      Left e -> fail e
      Right pc -> do
        db <- liftEffect (open ":memory:")
        cache <- liftEffect newServerCache
        let api method path query body = liftEffect (route (implement [ meridian, pc ] (storeOver db) cache (testHost (storeOver db))) { method, path, query: Object.fromFoldable query, body })
        list <- api "GET" "/api/premise" [] ""
        list.body `shouldSatisfy` contains (Pattern "\"presets\":[\"Adaptive\",\"Builder\",\"Exporter\",\"FreeTrade\",\"Hawk\",\"Hedger\",\"Merchant\",\"Patron\",\"Planner\",\"Populist\",\"Rebalance\",\"Reformer\",\"Restrict\",\"Retaliate\",\"Substitute\",\"Tariff\"]")
        list.body `shouldSatisfy` contains (Pattern "\"defaultPreset\":\"FreeTrade\"")
        launched <- api "POST" "/api/runs" [] "{\"composition\":\"pacific\",\"name\":\"p\",\"preset\":\"Restrict\",\"playerSeed\":1,\"rounds\":6}"
        launched.status `shouldEqual` 200
        dependence <- api "GET" "/api/runs/run-1/dependence" [] ""
        dependence.status `shouldEqual` 200
        dependence.body `shouldSatisfy` contains (Pattern "Coverage")

  it "a preset naming what the Composition lacks is refused" do
    isLeft (parts harbor { presets = presetsOf harborBinding { build = Just { transformation: Id "no-such", quantity: lit 1 } } }) `shouldEqual` true
  it "a joint pass over it offers this Composition's own names, for every kind a reply can hold" do
    case parts harbor of
      Left e -> fail e
      Right h -> do
        let
          world = h.composition.world
          names = (jointRequest world 1 []).names
        sort (Object.keys names) `shouldEqual` sort (kindsNamed interpretCall.wire.desc)
        Object.lookup "resource" names `shouldEqual` Just (map idText (fromFoldable (Map.keys world.resources)))
        Object.lookup "actor" names `shouldEqual` Just (map idText (fromFoldable (Map.keys world.actors)))
  it "is played and viewed, and compared with Meridian as a batch; each run records its Premise" do
    meridian <- meridianParts
    case parts harbor of
      Left e -> fail e
      Right h -> do
        db <- liftEffect (open ":memory:")
        cache <- liftEffect newServerCache
        let api method path query body = liftEffect (route (implement [ meridian, h ] (storeOver db) cache (testHost (storeOver db))) { method, path, query: Object.fromFoldable query, body })
        list <- api "GET" "/api/premise" [] ""
        list.body `shouldSatisfy` contains (Pattern "\"harbor\"")
        launched <- api "POST" "/api/runs" [] "{\"composition\":\"harbor\",\"name\":\"h\",\"preset\":\"Conservative\",\"playerSeed\":1,\"rounds\":3}"
        launched.status `shouldEqual` 200
        charts <- api "GET" "/api/runs/run-1/exchanges" [] ""
        charts.body `shouldSatisfy` contains (Pattern "Corn reserves")
        objectives <- api "GET" "/api/runs/run-1/objectives" [] ""
        objectives.status `shouldEqual` 200
        rerun <- api "POST" "/api/runs/run-1/derive" [] "{\"kind\":\"rerun\"}"
        rerun.status `shouldEqual` 201
        rerun.body `shouldSatisfy` contains (Pattern "\"recordsMatch\":true")
        batch <- api "POST" "/api/batch" [] "{\"preset\":\"Conservative\",\"playerSeed\":1}"
        batch.status `shouldEqual` 200
        batch.body `shouldSatisfy` contains (Pattern "\"composition\":\"meridian\"")
        batch.body `shouldSatisfy` contains (Pattern "\"composition\":\"harbor\"")
        compared <- api "GET" "/api/compare" [ Tuple "runs" "run-3,run-4" ] ""
        compared.status `shouldEqual` 200
        compared.body `shouldSatisfy` contains (Pattern "\"skipped\":[]")
