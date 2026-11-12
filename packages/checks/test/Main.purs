module Test.Checks.Main where

import Prelude

import Checks.Objectives (evaluateObjectives)
import Checks.Run (checkRun)
import Core.Draw (Seed(..))
import Core.Num as Q
import Data.Array (filter, head, index)
import Data.Foldable (minimum)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect (Effect)
import Laws.Slice (exchangeFills, select)
import Premises.Pacific.Names (p)
import Schema.Build.Quantity (q)
import Schema.Build.Base (bought, everyRound, producedOf, volumeOf)
import Schema.Build.World (atLeast, criterion, metric)
import Schema.Composition (Outcome(..))
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Testkit.Run (unseated, initialOf, run, pacificParts)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "checkRun" do
    it "finds nothing in a run of no rounds" do
      g <- pacificParts >>= unseated
      checkRun g.world [] `shouldEqual` []
    it "finds nothing in a played Pacific run" do
      g <- pacificParts >>= unseated
      r <- run (Seed 1) g
      checkRun (initialOf r) r.records `shouldEqual` []
  describe "Objectives" do
    it "Bought and Produced read each round, and an every-round criterion fails if any one round falls short" do
      parts <- pacificParts
      g <- unseated parts
      r <- run (Seed 1) g
      let
        every who m bar = criterion { metric: metric { actors: [ who ], measure: m, negated: false }, over: everyRound, atLeast: atLeast bar }
        withCriteria cs = parts.composition { explicitActors = Map.update (\d -> Just d { objective = map (_ { criteria = cs }) d.objective }) p.us parts.composition.explicitActors }
        evaluated cs = maybe [] _.criteria (head (filter (\o -> o.explicitActor == p.us) (evaluateObjectives (withCriteria cs) r.records)))
        boughtByHand = map (\x -> Q.sumQ (map _.quantity (filter (\f -> f.actor == p.usPrivate && f.quantity > Q.zero && (f.resource == p.technology || f.resource == p.industry)) (select exchangeFills x.record.events)))) r.records
        results = evaluated [ every p.usPrivate (bought [ p.technology, p.industry ]) Q.zero, every p.us (producedOf p.technology) Q.zero, every p.us (volumeOf p.usTechnologyProduction) Q.zero ]
        values = map _.values results
      map _.outcome results `shouldEqual` [ Met, Met, Met ]
      head values `shouldEqual` Just boughtByHand
      -- the state's technology production delivers one unit per unit run
      index values 1 `shouldEqual` index values 2
      -- a bar above the run's total is met by no round
      let bar = Q.add Q.one (Q.sumQ boughtByHand)
      map _.outcome (evaluated [ every p.usPrivate (bought [ p.technology, p.industry ]) bar ]) `shouldEqual` [ NotMet ]
      -- a bar at the smallest round's value is met; a hair above it fails on that round alone
      let least = fromMaybe Q.zero (minimum boughtByHand)
      map _.outcome (evaluated [ every p.usPrivate (bought [ p.technology, p.industry ]) least ]) `shouldEqual` [ Met ]
      map _.outcome (evaluated [ every p.usPrivate (bought [ p.technology, p.industry ]) (Q.add least (q "0.0001")) ]) `shouldEqual` [ NotMet ]
