-- | The mandatory game tests of docs/07_simulator.md §14, played on Pacific: full games under every preset,
-- | replay, submission order, Exchange access, engine reuse, controller integrity, resolution consistency and
-- | resuming from a snapshot. Pacific's own scenarios are in `Test.Handlers.Pacific`.
module Test.Handlers.Simulate (spec) where

import Prelude

import Assembly.Bindings (strategyRulesBindings)
import Assembly.Composition (presetNames)
import Assembly.Types (PreparedRun)
import Core.Draw (Seed(..))
import Core.Num as Q
import Data.Array (concatMap, drop, index, last, length, zip)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Handlers.Replay (runReplay)
import Handlers.Seats (seatEnv)
import Handlers.Simulate (Checkpoint, fromCheckpoint, simulate)
import Laws.Engine.Commit (resolveRound)
import Laws.Engine.Prepare (prepareRound)
import Laws.Invariants (orderIndependent)
import Laws.Slice (event, pickOf)
import Laws.StrategyRules (StrategyRules)
import Laws.Types (ActorKind(..), Capability(..), Id(..), IntentF(..), candidateTransactionId)
import Laws.World (renderReason)
import Schema.Build.Decision (passive)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Testkit.Run (everySeat, initialOf, orThrow, pacificParts, presetGame, run, valid)

-- | The game with the named seat replaced by a StrategyRules.
withStrategyRules :: PreparedRun -> String -> StrategyRules -> PreparedRun
withStrategyRules g e s = g { seats = Map.union (strategyRulesBindings [ Tuple (Id e) s ]) g.seats }

spec :: Spec Unit
spec = describe "Mandatory tests (docs/07_simulator.md §14)" do
  it "1 Full game: FreeTrade and Hawk play the configured rounds with invariants holding, and a seed reproduces its run" do
    p <- pacificParts
    for_ [ "FreeTrade", "Hawk" ] \preset -> do
      g <- everySeat p preset
      a <- run (Seed 1) g
      a' <- run (Seed 1) g
      length a.records `shouldEqual` 6
      valid (initialOf a) a
      map _.world a.records `shouldEqual` map _.world a'.records

  it "Every preset plays 8 rounds with invariants holding and no engine error" do
    p <- pacificParts
    for_ (presetNames p) \preset -> do
      g <- presetGame p preset [] (Just 8)
      r <- run (Seed 1) g
      length r.records `shouldEqual` 8
      valid (initialOf r) r

  it "2 Replay: the replay handler reproduces a run from its records" do
    p <- pacificParts
    g <- everySeat p "Hawk"
    a <- run (Seed 1) g
    out <- orThrow (runReplay (seatEnv (Seed 1) g) 6 (map _.record a.records) g)
    map _.world out.records `shouldEqual` map _.world a.records
    map _.record out.records `shouldEqual` map _.record a.records

  it "3 Submission order: permuting every recorded round's inputs changes nothing" do
    p <- pacificParts
    for_ [ "FreeTrade", "Hawk", "Populist", "Hedger" ] \preset -> do
      g <- everySeat p preset
      a <- run (Seed 1) g
      let befores = [ initialOf a ] <> map _.world a.records
      for_ (zip befores a.records) \(Tuple before r) -> orderIndependent before r `shouldEqual` []

  it "Exchange access: an order by an Actor outside an Exchange's access is rejected, though it holds the capability" do
    p <- pacificParts
    g <- presetGame p "FreeTrade" [] (Just 1)
    let
      -- the US firm is granted a trade capability on China's home Exchange, so only the Exchange's access rejects it
      w = prepareRound g.world
      granted = map (\e -> e { capabilities = e.capabilities <> [ MayTrade (Id "industry-cn") ] }) (Map.lookup (Id "US-firm") w.actors)
      world = maybe w (\e -> w { actors = Map.insert (Id "US-firm") e w.actors }) granted
      order who = { candidateTransaction: { id: candidateTransactionId 1 (Id who) 0, author: Id who, intents: [ Trade { exchange: Id "industry-cn", quantity: Q.fromInt 1, limitPrice: Nothing } ], signers: [], procedure: Nothing }, signatures: [] }
    r <- orThrow (resolveRound world [ order "US-firm", order "CN-firm" ])
    concatMap (\e -> maybe [] (\x -> [ renderReason x.reason ]) (pickOf (event @"Rejected") e)) r.record.events `shouldEqual` [ "US-firm has no access to industry-cn" ]

  it "INV-29 Engine reuse: resolving each round's recorded inputs through the engine reproduces the simulated round" do
    p <- pacificParts
    g <- everySeat p "Hawk"
    r <- run (Seed 1) g
    let befores = [ initialOf r ] <> map _.world r.records
    for_ (zip befores r.records) \(Tuple before rec) ->
      map _.world (resolveRound (prepareRound before) rec.record.inputs) `shouldEqual` Right rec.world

  it "INV-30 Controller integrity: swapping a seat's Strategy never changes its definition or makes it a NonPlayableActor" do
    p <- pacificParts
    g <- everySeat p "FreeTrade"
    hawk <- everySeat p "Hawk"
    a <- run (Seed 1) g
    b <- run (Seed 1) hawk
    c <- run (Seed 1) (withStrategyRules g "US" passive)
    let
      definitionOf w = map (\e -> { capabilities: e.capabilities, kind: e.kind }) (Map.lookup (Id "US") w.actors)
      final r = fromMaybe (initialOf r) (map _.world (last r.records))
    for_ [ a, b, c ] \r -> do
      definitionOf (final r) `shouldEqual` definitionOf (initialOf a)
      map _.kind (Map.lookup (Id "US") (final r).actors) `shouldEqual` Just (PlayableActor (Id "seat-1"))

  it "a run resumed from its third snapshot matches the uninterrupted run" do
    p <- pacificParts
    g <- everySeat p "Hawk"
    full <- run (Seed 1) g
    case index full.records 2 of
      Nothing -> fail "rounds"
      Just r3 -> do
        rest <- orThrow (simulate (Seed 1) (fromCheckpoint g ({ world: r3.world, done: 3 } :: Checkpoint)))
        map _.world rest.records `shouldEqual` map _.world (drop 3 full.records)
