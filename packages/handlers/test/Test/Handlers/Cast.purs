-- | The cast at initialization (docs/04 §3.1): each castable Actor a human, agent or preset seat, or a non-playable
-- | Actor running a preset as its in-world Strategy, with its routine after it.
module Test.Handlers.Cast (spec) where

import Prelude

import Assembly.Assemble (assembleCast, castable, defaultCast)
import Assembly.Error (AssemblyError(..))
import Assembly.Types (SeatBinding(..))
import Checks.Run (checkRun)
import Core.Draw (Seed(..))
import Data.Either (Either(..), isRight)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Handlers.Simulate (simulate)
import Laws.Types (ActorKind(..), Id(..))
import Premises.Pacific.Names (p)
import Core.Shape (Format(..), decodeJsonWith, encodeWith)
import Data.Argonaut.Core (fromString, toString)
import Schema.Cast (Role(..), roleShape)
import Test.Handlers.Pacific (pacificParts, playAll)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Testkit.Run (orThrow, run)

spec :: Spec Unit
spec = describe "The cast at initialization (docs/04 §3.1)" do
  it "a role reads from and writes to its text, and a malformed one is refused" do
    let texts = [ "human", "agent", "agent:m", "preset:FreeTrade", "nonplayer:Hawk" ]
    let read t = decodeJsonWith Snapshot roleShape (fromString t)
    map (map (toString <<< encodeWith Snapshot roleShape) <<< read) texts `shouldEqual` map (Right <<< Just) texts
    map (isRight <<< read) [ "", "robot", "preset" ] `shouldEqual` [ false, false, false ]

  it "the castable Actors are those with a definition, never an Aggregate" do
    parts <- pacificParts
    castable parts `shouldEqual` [ p.cn, p.cnFirm, p.us, p.usFirm ]

  it "every seat cast to one preset plays exactly today's preset run" do
    parts <- pacificParts
    g <- orThrow (assembleCast parts (defaultCast parts "FreeTrade") [] (Just 6))
    a <- run (Seed 1) g
    b <- playAll parts "FreeTrade" "FreeTrade" "FreeTrade" "FreeTrade" 6
    map _.world a.records `shouldEqual` map _.world b.records

  it "a non-playable Actor runs its preset in-world with its routine after it, and plays as the seat did" do
    parts <- pacificParts
    g <- orThrow (assembleCast parts (Map.insert p.us (CastNonplayer "FreeTrade") (defaultCast parts "FreeTrade")) [] (Just 6))
    map _.kind (Map.lookup p.us g.world.actors) `shouldEqual` Just (NonPlayableActor (Id "preset/FreeTrade"))
    Map.member p.us g.seats `shouldEqual` false
    a <- run (Seed 1) g
    checkRun a.manifest.initial a.records `shouldEqual` []
    b <- playAll parts "FreeTrade" "FreeTrade" "FreeTrade" "FreeTrade" 6
    -- the same economy: every holding after every round; only the US's kind differs
    map (\x -> map _.accounts x.world.actors) a.records `shouldEqual` map (\x -> map _.accounts x.world.actors) b.records

  it "a mixed cast assembles: a human seat, an agent seat on its own model, a preset seat and a non-player" do
    parts <- pacificParts
    let cast = Map.fromFoldable [ Tuple p.us CastHuman, Tuple p.cn (CastNonplayer "Hawk"), Tuple p.usFirm (CastAgentOn "some-model"), Tuple p.cnFirm (CastPreset "Builder") ]
    g <- orThrow (assembleCast parts cast [] Nothing)
    map playerKind (Map.lookup p.us g.seats) `shouldEqual` Just "human"
    map playerKind (Map.lookup p.usFirm g.seats) `shouldEqual` Just "agent some-model"
    map playerKind (Map.lookup p.cnFirm g.seats) `shouldEqual` Just "strategy"
    Map.member p.cn g.seats `shouldEqual` false
    -- a simulated run refuses a human or an agent seat
    map (const unit) (simulate (Seed 1) g) `shouldEqual` Left (NotSimulable p.us)

  it "a cast is refused for an Aggregate, an unknown id, an unknown preset, a forbidden seat, or a seat left out" do
    parts <- pacificParts
    let
      base = defaultCast parts "FreeTrade"
      refusal cast = case assembleCast parts cast [] Nothing of
        Left e -> Just e
        Right _ -> Nothing
      noSeats = parts { composition = parts.composition { explicitActors = Map.update (\d -> Just d { seat = false }) p.us parts.composition.explicitActors } }
    refusal (Map.insert p.usPrivate (CastPreset "FreeTrade") base) `shouldEqual` Just (NotCastable p.usPrivate)
    refusal (Map.insert (Id "NOBODY") (CastPreset "FreeTrade") base) `shouldEqual` Just (NotCastable (Id "NOBODY"))
    refusal (Map.insert p.us (CastPreset "NoSuchPreset") base) `shouldEqual` Just (UnknownPreset "NoSuchPreset")
    refusal (Map.insert p.us (CastPreset "") base) `shouldEqual` Just (UnknownPreset "")
    refusal (Map.insert p.us (CastNonplayer "NoSuchPreset") base) `shouldEqual` Just (UnknownPreset "NoSuchPreset")
    refusal (Map.delete p.us base) `shouldEqual` Just (UnboundSeat p.us)
    map (const unit) (assembleCast noSeats base [] Nothing) `shouldEqual` Left (SeatForbidden p.us)
    -- a forbidden seat may still be cast as a non-player
    isRight (assembleCast noSeats (Map.insert p.us (CastNonplayer "FreeTrade") base) [] Nothing) `shouldEqual` true
  where
  playerKind = case _ of
    Composed c -> playerKind c.player
    Human -> "human"
    Agent (Just m) -> "agent " <> m
    Agent Nothing -> "agent"
    StrategyRules _ -> "strategy"
    Script _ -> "script"
