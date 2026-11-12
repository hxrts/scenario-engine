-- | The mandatory tests of docs/07_simulator.md §14 that analyse runs through Experiments.Agency and Checks.Objectives:
-- | test 7 (a counterfactual pair), test 14 (agency), and the player-seed runs. They sit here, not in the simulator
-- | suite, because the simulator must not depend on experiments.
module Test.Experiments.Mandatory (spec) where

import Prelude

import Assembly.Bindings (strategyRulesBindings)
import Assembly.Composition (CompositionParts)
import Control.Monad.Error.Class (throwError)
import Core.Draw (Seed(..))
import Data.Array (any, filter, length, nubEq, range, sortWith)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Traversable (for, for_)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Exception (error)
import Experiments.Agency (agency, counterfactual, playerSeedRuns)
import Laws.Slice (exchangeFills, matches)
import Laws.Types (ActorId, Id(..))
import Core.Num as Q
import Premises.Pacific.Strategies (controlRule)
import Schema.Build.Decision (asTemplate, eqQ, lit, roundQ, rule, selfEnact)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Testkit.Run (everySeat, initialOf, orThrow, valid)
import Views.Agency (agencyOfRuns, agencyProfiles, agencyTable)
import Assembly.Assemble (assembleCast)
import Core.Shape (Format(..), encodeWith)
import Data.Argonaut.Core (stringify)
import Data.Maybe (Maybe(..))
import Handlers.Simulate (simulate)
import Premises.Suite (suiteOf, suitePlayerSeed)
import Schema.Cast (castShape)
import Schema.Run (RunStatus(..), emptySpec)
import Views.Common (storedRun)

-- | Pacific's four seats.
seats :: Array ActorId
seats = map Id [ "US", "CN", "US-firm", "CN-firm" ]

spec :: Aff CompositionParts -> Spec Unit
spec context = describe "Mandatory tests (docs/07_simulator.md §14)" do
  it "7 Counterfactual pair: China's state sells industrial inputs in the base run, and none to America once China controls their export" do
    ctx <- context
    base <- everySeat ctx "FreeTrade"
    let
      ban = { rules: [ rule [ eqQ roundQ (lit 1) ] [ asTemplate (selfEnact (controlRule (Id "CN"))) ] ], sign: [] }
      chinaSellsAfterRoundOne r = any
        (\x -> x.record.round > 1 && any (matches exchangeFills (\f -> f.actor == Id "CN" && f.resource == Id "I" && f.quantity < Q.zero)) x.record.events)
        r.records
    cf <- case counterfactual (Seed 1) base (\g -> g { seats = Map.union (strategyRulesBindings [ Tuple (Id "CN") ban ]) g.seats }) of
      Right x -> pure x
      Left e -> throwError (error (show e))
    chinaSellsAfterRoundOne cf.base `shouldEqual` true
    chinaSellsAfterRoundOne cf.variant `shouldEqual` false

  it "14 Agency: every seat, on its alternative preset with its routine kept, has a measurable effect and its objectives are reported" do
    ctx <- context
    ps <- orThrow (agency ctx (Seed 1) "FreeTrade" ctx.config.alternatives)
    length ps `shouldEqual` 4
    -- hard condition (docs/07 §14): a role with no measurable effect is a broken premise
    map _.explicitActor (filter (not <<< _.measurable) ps) `shouldEqual` []
    -- each criterion is reported under the baseline and the alternative, and the count of changes agrees
    for_ ps \p -> length (filter (\c -> c.baseline /= c.alternative) p.criteria) `shouldEqual` p.objectives
    length (agencyProfiles ps).rows `shouldEqual` length ps

  it "14 Agency from the suite: the agency runs against their preset run give the same table as the analysis" do
    ctx <- context
    analysed <- orThrow (agency ctx (Seed 1) "FreeTrade" ctx.config.alternatives)
    let entries = suiteOf ctx
    map _.name (filter (\e -> e.preset == Nothing) entries) `shouldEqual` [ "FreeTrade with CN on Hawk", "FreeTrade with CN-firm on Builder", "FreeTrade with US on Hawk", "FreeTrade with US-firm on Builder" ]
    -- each suite entry played and read back as a stored run, its spec recording its cast
    stored <- for (filter (\e -> e.preset == Just "FreeTrade" || e.preset == Nothing) entries) \e -> do
      game <- orThrow (assembleCast ctx e.cast [] Nothing)
      r <- orThrow (simulate (Seed suitePlayerSeed) game)
      pure (storedRun { id: e.name, name: e.name, status: Completed, manifest: r.manifest, playerSeed: "1", spec: emptySpec { preset = e.preset, cast = Just (stringify (encodeWith Snapshot castShape e.cast)) } } r.records (map (const []) r.records))
    let fromSuite = agencyOfRuns ctx.composition stored
    sortWith show (map _.explicitActor fromSuite) `shouldEqual` sortWith show seats
    agencyTable (sortWith (show <<< _.explicitActor) fromSuite) `shouldEqual` agencyTable (sortWith (show <<< _.explicitActor) analysed)

  it "player seeds leave a game with no seeded choice unchanged, and every run keeps its invariants" do
    ctx <- context
    g <- everySeat ctx "Hawk"
    runs <- orThrow (playerSeedRuns g (range 1 5))
    for_ runs \r -> valid (initialOf r) r
    length (nubEq (map (map _.world <<< _.records) runs)) `shouldEqual` 1
