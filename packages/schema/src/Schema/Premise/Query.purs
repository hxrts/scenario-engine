-- | The premise form of a `LegQuery` (docs/03_premise.md §7): what a StrategyRules's `recorded` condition reads
-- | and what a share is a share of. A share's query leaves out the party, which is the Actor that pays it.
module Schema.Premise.Query
  ( legQueryShape
  , ShareMeasure
  , measureShape
  , atParty
  ) where

import Core.Shape (Shape, atBare, atOneKey, genericPremiseSum, nameShape, optionalOf, recordShape)
import Laws.Types (ActorId, ResourceId, idShape)
import Laws.World (CausePattern, LegAggregate(..), LegQuery, Side(..), Window(..))
import Data.Maybe (Maybe)
import Record as Record
import Type.Proxy (Proxy(..))

-- | A query with its party still to come.
type ShareMeasure = { window :: Window, side :: Side, resource :: Maybe ResourceId, cause :: CausePattern, aggregate :: LegAggregate }

-- | A measure of the given Actor's settled Flows.
atParty :: ActorId -> ShareMeasure -> LegQuery
atParty party m = Record.insert (Proxy :: Proxy "party") party m

windowShape :: Shape Window
windowShape = nameShape "Window" "thisRoundSoFar or lastRound" name [ ThisRoundSoFar, LastRound ]
  where
  name = case _ of
    ThisRoundSoFar -> "thisRoundSoFar"
    LastRound -> "lastRound"

sideShape :: Shape Side
sideShape = nameShape "Side" "incoming, outgoing or netOutgoing" name [ Incoming, Outgoing, NetOutgoing ]
  where
  name = case _ of
    Incoming -> "incoming"
    Outgoing -> "outgoing"
    NetOutgoing -> "netOutgoing"

causeShape :: Shape CausePattern
causeShape = genericPremiseSum "CausePattern"
  { "CurveBase": atBare { tag: "curveBase", desc: "the base leg of a settlement along an Exchange's curve" }
  , "TransformationPayment": atBare { tag: "transformationPayment", desc: "a transformation's payment leg" }
  , "TransformationOf": atOneKey "transformationOf" "transformationFills of this Transformation" idShape
  }

aggregateShape :: Shape LegAggregate
aggregateShape = nameShape "LegAggregate" "sumMoved, sumValue or largestUnits" name [ SumMoved, SumValue, LargestUnits ]
  where
  name = case _ of
    SumMoved -> "sumMoved"
    SumValue -> "sumValue"
    LargestUnits -> "largestUnits"

measureShape :: Shape ShareMeasure
measureShape = recordShape "ShareMeasure" { window: windowShape, side: sideShape, resource: optionalOf idShape, cause: causeShape, aggregate: aggregateShape }

legQueryShape :: Shape LegQuery
legQueryShape = recordShape "LegQuery" { party: idShape, window: windowShape, side: sideShape, resource: optionalOf idShape, cause: causeShape, aggregate: aggregateShape }
