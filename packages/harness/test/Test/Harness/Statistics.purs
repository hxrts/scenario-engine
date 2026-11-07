-- | The dashboard's statistics (docs/06_simulator.md §9.2, §10.2) on small hand-built runs: net worth
-- | of a known holding at a known price, the Gini coefficient and the spread, standings and rank, the
-- | action counts, the seed variation, and the grouping of runs by specification.
module Test.Harness.Statistics (spec) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Core.Draw (Seed(..))
import Core.Num (Q)
import Core.Num as Q
import Core.Types (EntityId, Id(..), Intent(..), ResourceId)
import Core.World (Event(..), RoundRecord, WorldState)
import Data.Array (find, index, length, take)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Exception (error)
import Harness.Simulate (Context, Run, presetSeats, simulate)
import Harness.Views.Actions (actions)
import Harness.Views.Balance (gini, seedVariation, spread)
import Harness.Views.Common (StoredRun, Table, players)
import Harness.Views.Compare (sameSpecification)
import Harness.Views.Standings (netWorth, standings)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

seatIds :: Array String
seatIds = [ "US", "DE", "IN", "JP", "UK", "BR", "SA", "NG", "ENCO", "CHIP", "SHIP", "DEF", "LABOR", "SME", "AID" ]

us :: EntityId
us = Id "US"

de :: EntityId
de = Id "DE"

money :: ResourceId
money = Id "M"

energy :: ResourceId
energy = Id "E"

-- | A simulated run's manifest and World States, read back as the Run Store would: the base every
-- | hand-built run below reuses.
baseRun :: Context -> Aff StoredRun
baseRun ctx = case simulate ctx { seats: presetSeats (const "Conservative") (map Id seatIds), playerSeed: Seed 1, overrides: [], rounds: Just 2 } of
  Left e -> throwError (error e)
  Right r -> pure (toStored r)
  where
  toStored :: Run -> StoredRun
  toStored r =
    { id: "base"
    , name: "base"
    , status: "completed"
    , manifest: r.manifest
    , playerSeed: "1"
    , spec: [ Tuple "preset" "Conservative", Tuple "playerSeed" "1" ]
    , sourceDeleted: false
    , records: map _.record r.records
    , snapshots: map _.world r.records
    , assertions: map (const []) r.records
    }

-- | A holding set to an amount in one Entity's account.
setHolding :: WorldState -> EntityId -> ResourceId -> Q -> WorldState
setHolding w e res amount = w { entities = Map.update (\x -> Just (x { accounts = Map.insert res amount x.accounts })) e w.entities }

-- | A World State where every Actor holds nothing of Money, Energy or Military, and every Market's
-- | position is zero, so each Energy and Military price is its base price.
blank :: WorldState -> WorldState
blank w0 = w { markets = map (\m -> m { position = Q.zero }) w.markets }
  where
  w = foldl zeroPlayer w0 (players w0)
  zeroPlayer acc e = setHolding (setHolding (setHolding acc e (Id "Mil") Q.zero) e energy Q.zero) e money Q.zero

-- | A run whose initial World State and single final World State are given, with one empty round.
runWith :: StoredRun -> WorldState -> WorldState -> StoredRun
runWith base initial final = base
  { manifest = (base.manifest) { initial = initial }
  , records = [ { round: 1, inputs: [], events: [] } ]
  , snapshots = [ final ]
  }

-- | The Table's row whose first cell is the given name.
rowOf :: Table -> String -> Maybe (Array String)
rowOf t name = find (\row -> index row 0 == Just name) t.rows

-- | The format of a Maybe Q, for comparisons with literal strings.
fmt :: Maybe Q -> Maybe String
fmt = map Q.format

spec :: Aff Context -> Spec Unit
spec context = describe "Dashboard statistics on hand-built runs" do
  describe "Net worth" do
    it "a known holding at a known price: 100 M plus 10 E at 2 M/E is 120 M" do
      ctx <- context
      base <- baseRun ctx
      let w = setHolding (setHolding (blank base.manifest.initial) us money (Q.fromInt 100)) us energy (Q.fromInt 10)
      Q.format (netWorth w us) `shouldEqual` "120.0000"
    it "a Resource traded in two regional Markets is valued once, not once per Market" do
      ctx <- context
      base <- baseRun ctx
      let w = setHolding (blank base.manifest.initial) us energy (Q.fromInt 10)
      -- both energy Markets are at base price 2 with zero position, so the mean is 2 and 10 E is 20 M
      Q.format (netWorth w us) `shouldEqual` "20.0000"
  describe "Gini and spread" do
    it "equal wealth has a Gini of 0" do
      fmt (gini [ Q.fromInt 4, Q.fromInt 4, Q.fromInt 4, Q.fromInt 4 ]) `shouldEqual` Just "0.0000"
    it "one player holding everything has a Gini of 1 - 1/n" do
      fmt (gini [ Q.zero, Q.zero, Q.zero, Q.fromInt 4 ]) `shouldEqual` Just "0.7500"
      fmt (gini [ Q.zero, Q.zero, Q.zero, Q.zero, Q.fromInt 9 ]) `shouldEqual` Just "0.8000"
    it "the Gini is not defined for no players or a zero total" do
      fmt (gini []) `shouldEqual` Nothing
      fmt (gini [ Q.zero, Q.zero ]) `shouldEqual` Nothing
    it "the spread is the highest minus the lowest net worth" do
      fmt (spread [ Q.fromInt 3, Q.fromInt 10, Q.fromInt 7 ]) `shouldEqual` Just "7.0000"
      fmt (spread []) `shouldEqual` Nothing
  describe "Standings" do
    it "change from the start and rank: the richer player ranks first, ties share a rank" do
      ctx <- context
      base <- baseRun ctx
      let
        start = setHolding (blank base.manifest.initial) us money Q.zero
        final = setHolding (setHolding start us money (Q.fromInt 120)) de money (Q.fromInt 50)
        t = (standings (runWith base start final))
      rowOf t "US" `shouldEqual` Just [ "US", "120", "120", "1" ]
      rowOf t "DE" `shouldEqual` Just [ "DE", "50", "50", "2" ]
      let tied = setHolding final de money (Q.fromInt 120)
      rowOf (standings (runWith base start tied)) "DE" `shouldEqual` Just [ "DE", "120", "120", "1" ]
  describe "Actions" do
    it "counts trades, transforms, transfers paid, deals signed and rejections per player" do
      ctx <- context
      base <- baseRun ctx
      let
        p1 = Id "p1"
        d1 = Id "d1"
        deal = { id: d1, author: us, intents: [ Transfer { from: us, to: de, resource: money, quantity: Q.fromInt 5 } ], signers: [ us, de ], procedure: Nothing }
        own = { id: p1, author: us, intents: [], signers: [], procedure: Nothing }

        round :: RoundRecord
        round =
          { round: 1
          , inputs: [ { proposal: own, signatures: [] }, { proposal: deal, signatures: [ us, de ] } ]
          , events:
              [ Traded { round: 1, market: Id "energy-atlantic", entity: us, quantity: Q.fromInt 1, price: Q.fromInt 2 }
              , Traded { round: 1, market: Id "energy-atlantic", entity: us, quantity: Q.fromInt 1, price: Q.fromInt 2 }
              , Transformed { round: 1, by: us, transformation: Id "defense-procurement", quantity: Q.fromInt 1, paid: Q.fromInt 1 }
              , Transferred { round: 1, from: us, to: de, resource: money, quantity: Q.fromInt 5 }
              , Rejected { round: 1, proposal: p1, reason: "no" }
              ]
          }
        run = base { records = [ round ], snapshots = [ base.manifest.initial ] }
        view = actions run
        totals = view.totals
      rowOf totals "US" `shouldEqual` Just [ "US", "2", "1", "1", "1", "1", "6" ]
      rowOf totals "DE" `shouldEqual` Just [ "DE", "0", "0", "0", "1", "0", "1" ]
      map _.label view.chart.kinds `shouldEqual`
        [ "Market trades (buys and sells)", "Production and procurement", "Payments to other players", "Deals signed", "Proposals rejected" ]
      -- one bar per player, highest total first; the players with no actions follow in seat order
      length view.chart.bars `shouldEqual` length view.totals.rows
      map (\b -> Tuple b.player b.total) (take 2 view.chart.bars) `shouldEqual` [ Tuple "US" 6, Tuple "DE" 1 ]
      map (\b -> map _.count b.counts) (take 2 view.chart.bars) `shouldEqual` [ [ 2, 1, 1, 1, 1 ], [ 0, 0, 0, 1, 0 ] ]
  describe "Seed variation and specification groups" do
    it "one run reports that more runs are needed, and no range" do
      ctx <- context
      base <- baseRun ctx
      let a = runWith base (blank base.manifest.initial) (setHolding (blank base.manifest.initial) us money (Q.fromInt 120))
      case index (seedVariation [ a ]).rows 0 of
        Just row -> index row 0 `shouldEqual` Just "one run of this specification"
        Nothing -> fail "no row"
      length (seedVariation [ a ]).rows `shouldEqual` 2
    it "two runs of one specification report the range of their spread" do
      ctx <- context
      base <- baseRun ctx
      let
        start = blank base.manifest.initial
        a = runWith base start (setHolding start us money (Q.fromInt 120))
        b = runWith (base { playerSeed = "2" }) start (setHolding start us money (Q.fromInt 50))
        t = seedVariation [ a, b ]
      rowOf t "range" `shouldEqual` Just [ "range", "", "", "70", "0.00" ]
    it "runs group by specification, ignoring the player seed, and not with another preset" do
      ctx <- context
      base <- baseRun ctx
      let
        seeded = base { id = "b", spec = [ Tuple "preset" "Conservative", Tuple "playerSeed" "2" ] }
        other = base { id = "c", spec = [ Tuple "preset" "Aggressive", Tuple "playerSeed" "1" ] }
      map _.id (sameSpecification [ base, seeded, other ]) `shouldEqual` [ "base", "b" ]
