module Test.Scenarios.Main where

import Prelude

import Board.Gameboard (ValidationError(..), instantiate, reconcile, validateGameboard)
import Control.Monad.Error.Class (throwError)
import Core.Num as Q
import Core.Types (Capability(..), Controller(..), EntityKind(..), Id(..), RuleKind(..))
import Data.Array (elem, filter, length)
import Data.Either (Either(..), isRight)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Scenarios (load)
import Test.Spec (describe, it)
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

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "The Fractured World" do
    let
      loaded = do
        r <- liftEffect (load "fractured-world/v0")
        case r of
          Left e -> throwError (error e)
          Right x -> pure x
      instantiated = do
        { gameboard, config } <- loaded
        case instantiate gameboard config of
          Left es -> throwError (error (show es))
          Right x -> pure { gameboard, config, initial: x }
      amount w e r = fromMaybe Q.zero (Map.lookup (Id e) w.entities >>= \x -> Map.lookup (Id r) x.accounts)
      capacity w e c = fromMaybe Q.zero (Map.lookup (Id e) w.entities >>= \x -> Map.lookup (Id c) x.capacities)

    it "validates" do
      { gameboard, config } <- loaded
      map (const unit) (validateGameboard gameboard config) `shouldEqual` Right unit

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
      Q.sumQ (map (\e -> capacity w e "energy-production") [ "ENERGY_SUPPLIERS", "SA", "ENCO", "BR", "NG", "IN" ]) `shouldEqual` Q.fromInt 140
      map (\x -> x.kind) (Map.lookup (Id "CN") w.entities) `shouldEqual` Just (Actor (NPC (Id "china-strategic-v0")))
      length w.representations `shouldEqual` 1

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

    it "rejects invalid gameboards with their specific errors" do
      { gameboard, config } <- loaded
      let
        rejects f err = case validateGameboard (f gameboard) config of
          Left es -> (err `elem` es) `shouldEqual` true
          Right _ -> fail ("expected " <> show err)
        w = gameboard.world
      rejects (\g -> g { actors = Map.update (\d -> Just d { fallback = Nothing }) (Id "DE") g.actors }) (MissingFallback (Id "DE"))
      rejects (\g -> g { world = w { entities = Map.update (\e -> Just e { capacities = Map.insert (Id "semiconductors") (Q.fromInt 1) e.capacities }) (Id "SA") w.entities } }) (DuplicateCapacityOwner (Id "semiconductors"))
      rejects (\g -> g { world = w { transformations = Map.update (\t -> Just t { payee = Nothing }) (Id "public-attention") w.transformations } }) (FacilityWithoutPayee (Id "public-attention"))
      rejects (\g -> g { world = w { entities = Map.update (\e -> Just e { capabilities = filter (_ /= MayEnact TariffRule) e.capabilities }) (Id "EU") w.entities } }) (ProcedureExceedsAuthority (Id "EU"))
      isRight (validateGameboard gameboard config { npcs = Map.empty }) `shouldEqual` false
