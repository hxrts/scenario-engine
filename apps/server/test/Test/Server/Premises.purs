-- | A second Premise, built only from the Kit: compiled, loaded, played, viewed, and compared with Meridian
-- | as a batch.
-- | Pins: the numbers of this suite's own small Kit-built Premise (docs/03 §12); no docs/05 values.
module Test.Server.Premises (spec) where

import Prelude

import Test.Server.Host (testHost)

import Testkit.Store (open, storeOver)
import Core.Draw (Seed(..))
import Laws.Types (Actor, ActorId, Id(..), Transferability(..))
import Laws.World (Focus)
import Data.Either (Either(..), isLeft)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Class (liftEffect)
import Foreign.Object as Object
import Handlers.Api.Route (route)
import Premises.Compose (Premise, files, parts)
import Premises.Kit.Seats (Cover(..), seat)
import Premises.Kit.Holdings (HoldingTable)
import Premises.Kit.Exchanges (exchange)
import Premises.Kit.Presets (standardThresholds)
import Premises.Load (fromFiles)
import Schema.Build.Decision (lit)
import Schema.PossibleWorld (ActorDefinition)
import Schema.Build.Base (holdingOf, keyed, mayTrade, mayTransfer, (:=))
import Schema.Build.Quantity (q)
import Schema.Build.World (atLeast, byKey, emptyCirculation, emptyWorld, actorsOf, finishes, noResource, resourcesOf)
import Server.Stack (implement, newServerCache)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldSatisfy)
import Testkit.Meridian (premise)

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

harbor :: Premise
harbor =
  { id: "harbor"
  , version: "v0"
  , possibleWorld:
      { id: "harbor"
      , version: "v0"
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
  , config: { seed: Seed 7, rounds: 3, seats: keyed [ Tuple "A" "seat-1", Tuple "B" "seat-2" ], nonPlayableActors: Map.empty, fallbacks: Map.empty, representations: [] }
  , presets: { staple: Id "C", money: Id "G", build: Nothing, invest: Nothing, deploy: Nothing, thresholds: standardThresholds }
  }

spec :: Spec Unit
spec = describe "A second Premise" do
  it "built only from the Kit, it splits into sections, loads, and checks" do
    case fromFiles (map (\(Tuple name json) -> { name, json }) (files harbor)), parts harbor of
      Right d, Right _ -> d.possibleWorld.id `shouldEqual` "harbor"
      _, Left e -> fail e
      Left e, _ -> fail e
  it "a preset naming what the Premise lacks is refused" do
    isLeft (parts harbor { presets { build = Just { transformation: Id "no-such", quantity: lit 1 } } }) `shouldEqual` true
  it "is played and viewed, and compared with Meridian as a batch; each run records its Premise" do
    meridian <- premise
    case parts harbor of
      Left e -> fail e
      Right h -> do
        db <- liftEffect (open ":memory:")
        cache <- liftEffect newServerCache
        let api method path query body = liftEffect (route (implement [ meridian, h ] (storeOver db) cache (testHost (storeOver db))) { method, path, query: Object.fromFoldable query, body })
        list <- api "GET" "/api/premise" [] ""
        list.body `shouldSatisfy` contains (Pattern "\"harbor\"")
        launched <- api "POST" "/api/runs" [] "{\"premise\":\"harbor\",\"name\":\"h\",\"preset\":\"Conservative\",\"playerSeed\":1,\"rounds\":3}"
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
        batch.body `shouldSatisfy` contains (Pattern "\"premise\":\"meridian\"")
        batch.body `shouldSatisfy` contains (Pattern "\"premise\":\"harbor\"")
        compared <- api "GET" "/api/compare" [ Tuple "runs" "run-3,run-4" ] ""
        compared.status `shouldEqual` 200
        compared.body `shouldSatisfy` contains (Pattern "\"skipped\":[]")
