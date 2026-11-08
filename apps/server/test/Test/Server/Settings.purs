-- | A second Setting, built only from the Kit: compiled, loaded, played, viewed, and compared with Meridian
-- | as a batch.
module Test.Server.Settings (spec) where

import Prelude

import Adapters.SQLite (open, sqliteBackend)
import Core.Draw (Seed(..))
import Core.Types (Entity, Id(..), Transferability(..))
import Core.World (Focus)
import Data.Either (Either(..), isLeft)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Class (liftEffect)
import Foreign.Object as Object
import Handlers.Api.Route (route)
import Settings.Compose (Setting, files, parts)
import Settings.Kit (Cover(..), HoldingTable, market, seat)
import Settings.Kit.Presets (standardThresholds)
import Settings.Load (fromFiles)
import Schema.Build.Decision (lit)
import Schema.Gameboard (ActorDefinition)
import Schema.Build.Base (holdingOf, keyed, mayTrade, mayTransfer, (:=))
import Schema.Build.Quantity (q)
import Schema.Build.World (atLeast, byKey, emptyCirculation, emptyWorld, entitiesOf, finishes, noResource, resourcesOf)
import Server.Cache (newCache)
import Server.Implementation (implement)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldSatisfy)
import Testkit.Meridian (setting)

focus :: Focus
focus = { resource: Id "C", label: "Corn" }

table :: HoldingTable
table =
  [ Tuple "A" { accounts: [ "G" := 100, "C" := 10 ], capacities: [] }
  , Tuple "B" { accounts: [ "G" := 80, "C" := 20 ], capacities: [] }
  ]

seats :: Array (Tuple String (Tuple Entity ActorDefinition))
seats = map (\k -> Tuple k (seat table k spec')) [ "A", "B" ]
  where
  spec' =
    { capabilities: [ mayTrade "corn", mayTransfer "G", mayTransfer "C" ]
    , required: true
    , cover: Unassigned
    , goal: "Stay solvent"
    , criteria: [ finishes "A" (holdingOf "G") (atLeast 1) ]
    , description: "A trader"
    , dependency: "The corn exchange"
    }

harbor :: Setting
harbor =
  { id: "harbor"
  , version: "v0"
  , gameboard:
      { id: "harbor"
      , version: "v0"
      , horizon: 3
      , rounds: { min: 2, max: 4 }
      , label: Just "harbor-01"
      , parameters: Object.empty
      , schemes: []
      , policies: keyed []
      , actors: keyed (map (map snd) seats)
      , authored: { circulation: emptyCirculation, reservable: [], decays: [] }
      , world: emptyWorld
          { numeraire = Id "G"
          , focus = Just focus
          , resources = resourcesOf [ Tuple "G" (noResource { transferability = Free }), Tuple "C" (noResource { transferability = Free }) ]
          , markets = byKey [ Tuple "corn" (market { base: "C", quote: "G", cost: { intercept: q 2, slope: q "0.01", domain: { lo: q (-50), hi: q 50 } }, reserves: keyed [ "G" := 500, "C" := 200 ] } [ "A", "B" ]) ]
          , entities = entitiesOf (map (map fst) seats)
          }
      }
  , config: { seed: Seed 7, rounds: 3, seats: keyed [ Tuple "A" "seat-1", Tuple "B" "seat-2" ], npcs: Map.empty, fallbacks: Map.empty, representations: [] }
  , presets: { staple: "C", money: "G", build: Nothing, invest: Nothing, deploy: Nothing, thresholds: standardThresholds }
  }

spec :: Spec Unit
spec = describe "A second Setting" do
  it "built only from the Kit, it splits into sections, loads, and checks" do
    case fromFiles (map (\(Tuple name json) -> { name, json }) (files harbor)), parts harbor of
      Right d, Right _ -> d.gameboard.id `shouldEqual` "harbor"
      _, Left e -> fail e
      Left e, _ -> fail e
  it "a preset naming what the Setting lacks is refused" do
    isLeft (parts harbor { presets { build = Just { transformation: "no-such", quantity: lit 1 } } }) `shouldEqual` true
  it "is played and viewed, and compared with Meridian as a batch; each run records its Setting" do
    meridian <- setting
    case parts harbor of
      Left e -> fail e
      Right h -> do
        db <- liftEffect (open ":memory:")
        cache <- liftEffect newCache
        let api method path query body = liftEffect (route (implement [ meridian, h ] (sqliteBackend db) cache) { method, path, query: Object.fromFoldable query, body })
        list <- api "GET" "/api/setting" [] ""
        list.body `shouldSatisfy` contains (Pattern "\"harbor\"")
        launched <- api "POST" "/api/runs" [] "{\"setting\":\"harbor\",\"name\":\"h\",\"preset\":\"Conservative\",\"playerSeed\":1,\"rounds\":3}"
        launched.status `shouldEqual` 200
        charts <- api "GET" "/api/runs/run-1/markets" [] ""
        charts.body `shouldSatisfy` contains (Pattern "Corn reserves")
        objectives <- api "GET" "/api/runs/run-1/objectives" [] ""
        objectives.status `shouldEqual` 200
        rerun <- api "POST" "/api/runs/run-1/derive" [] "{\"kind\":\"rerun\"}"
        rerun.status `shouldEqual` 201
        rerun.body `shouldSatisfy` contains (Pattern "\"recordsMatch\":true")
        batch <- api "POST" "/api/batch" [] "{\"preset\":\"Conservative\",\"playerSeed\":1}"
        batch.status `shouldEqual` 200
        batch.body `shouldSatisfy` contains (Pattern "\"setting\":\"meridian\"")
        batch.body `shouldSatisfy` contains (Pattern "\"setting\":\"harbor\"")
        compared <- api "GET" "/api/compare" [ Tuple "runs" "run-3,run-4" ] ""
        compared.status `shouldEqual` 200
        compared.body `shouldSatisfy` contains (Pattern "\"skipped\":[]")
