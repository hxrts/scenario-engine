-- | The premise form of a `LegQuery` (docs/03_premise.md §7): what a StrategyRules's `recorded` condition reads
-- | and what a share is a share of. A share's query leaves out the party, which is the Actor that pays it.
module Schema.Premise.Query
  ( legQueryShape
  , ShareMeasure
  , measureShape
  , atParty
  ) where

import Core.Shape (Shape, atBare, atOneKey, camelName, genericNameShape, genericPremiseSum, orList, optionalOf, recordShape)
import Laws.Types (ActorId, ResourceId, idShape)
import Laws.World (CausePattern, LegAggregate, LegQuery, Side, Window)
import Data.Maybe (Maybe)
import Record as Record
import Type.Proxy (Proxy(..))

-- | A query with its party still to come.
type ShareMeasure = { window :: Window, side :: Side, resource :: Maybe ResourceId, cause :: CausePattern, aggregate :: LegAggregate }

-- | A measure of the given Actor's settled Flows.
atParty :: ActorId -> ShareMeasure -> LegQuery
atParty party m = Record.insert (Proxy :: Proxy "party") party m

windowShape :: Shape Window
windowShape = genericNameShape "Window" orList camelName

sideShape :: Shape Side
sideShape = genericNameShape "Side" orList camelName

causeShape :: Shape CausePattern
causeShape = genericPremiseSum "CausePattern"
  { "CurveBase": atBare { tag: "curveBase", desc: "the base leg of a pair an Exchange cleared" }
  , "TransformationPayment": atBare { tag: "transformationPayment", desc: "a transformation's payment leg" }
  , "TransformationOf": atOneKey "transformationOf" "transformationFills of this Transformation" idShape
  }

aggregateShape :: Shape LegAggregate
aggregateShape = genericNameShape "LegAggregate" orList camelName

measureShape :: Shape ShareMeasure
measureShape = recordShape "ShareMeasure" { window: windowShape, side: sideShape, resource: optionalOf idShape, cause: causeShape, aggregate: aggregateShape }

legQueryShape :: Shape LegQuery
legQueryShape = recordShape "LegQuery" { party: idShape, window: windowShape, side: sideShape, resource: optionalOf idShape, cause: causeShape, aggregate: aggregateShape }
