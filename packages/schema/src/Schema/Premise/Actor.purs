-- | The premise format of an Actor's definition (docs/03 §6): its seat eligibility, fallback,
-- | objective, and the criteria and measures the objective names. Each is a `Shape` value; the
-- | premise tags are private to this module. A key the decoder defaults is `Maybe` here, so the
-- | encoder writes exactly the keys an author wrote (presence is kept, see `Core.Shape`).
module Schema.Premise.Actor
  ( DefinitionDoc
  , ObjectiveDoc
  , CriterionDoc
  , MetricDoc
  , BoundDoc
  , definitionShape
  , metricShape
  , boundShape
  , fallbackShape
  , definitionToCore
  ) where

import Prelude

import Core.Num (Q)
import Core.Shape (Shape, bare, boolShape, embedded, fieldOf, kindArg, listOf, literal, object, optionalOf, premiseSum, qShape, stringShape)
import Core.Types (Id(..), idShape)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), fromMaybe)
import Schema.Gameboard (ActorDefinition, Bound(..), Criterion, Fallback(..), Measure(..), MetricRef, Objective, Span(..))

-- | An Actor definition as written. Every key is optional: the defaults are the decoder's.
type DefinitionDoc =
  { required :: Maybe Boolean
  , seat :: Maybe Boolean
  , npc :: Maybe (Array String)
  , fallback :: Maybe Fallback
  , objective :: Maybe ObjectiveDoc
  , description :: Maybe String
  , dependency :: Maybe String
  }

type ObjectiveDoc = { description :: String, criteria :: Array CriterionDoc }

-- | A criterion as written: `over` is optional (`final` by default).
type CriterionDoc = { metric :: MetricDoc, over :: Maybe Span, atLeast :: BoundDoc }

-- | A metric as written: `negated` is optional (false by default).
type MetricDoc = { entities :: Array String, measure :: Measure, negated :: Maybe Boolean }

-- | A bound: a fixed quantity (`Left`), or a metric the Entity must match (`Right`).
type BoundDoc = Either Q MetricDoc

-- Tags ----------------------------------------------------------------------------------------

coverageTag :: String
coverageTag = "coverage"

policyTag :: String
policyTag = "policy"

omissionTag :: String
omissionTag = "omission"

holdingTag :: String
holdingTag = "holding"

capacityTag :: String
capacityTag = "capacity"

shortfallTag :: String
shortfallTag = "shortfall"

shortfallRoundsTag :: String
shortfallRoundsTag = "shortfallRounds"

payeeReceiptsTag :: String
payeeReceiptsTag = "payeeReceipts"

volumeTag :: String
volumeTag = "volume"

finalTag :: String
finalTag = "final"

everyRoundTag :: String
everyRoundTag = "everyRound"

cumulativeTag :: String
cumulativeTag = "cumulative"

-- Shapes --------------------------------------------------------------------------------------

definitionShape :: Shape DefinitionDoc
definitionShape = object "Definition"
  ( mk <$> fieldOf "required" "must be seated (default false)" (optionalOf boolShape) _.required
      <*> fieldOf "seat" "may be seated (default true)" (optionalOf boolShape) _.seat
      <*> fieldOf "npc" "the NPC Policies it may take" (optionalOf (listOf stringShape)) _.npc
      <*> fieldOf "fallback" "what covers it when it has no player" (optionalOf fallbackShape) _.fallback
      <*> fieldOf "objective" "its measurable objective" (optionalOf objectiveShape) _.objective
      <*> fieldOf "description" "what the Actor does (prose)" (optionalOf stringShape) _.description
      <*> fieldOf "dependency" "what the Actor depends on (prose)" (optionalOf stringShape) _.dependency
  )
  where
  mk r s n f o d p = { required: r, seat: s, npc: n, fallback: f, objective: o, description: d, dependency: p }

objectiveShape :: Shape ObjectiveDoc
objectiveShape = object "Objective"
  ( mk <$> fieldOf "description" "what the objective is" stringShape _.description
      <*> fieldOf "criteria" "the criteria it is met by" (listOf criterionShape) _.criteria
  )
  where
  mk d c = { description: d, criteria: c }

criterionShape :: Shape CriterionDoc
criterionShape = object "Criterion"
  ( mk <$> fieldOf "metric" "the measured quantity" metricShape _.metric
      <*> fieldOf "over" "the span of rounds (default final)" (optionalOf spanShape) _.over
      <*> fieldOf "atLeast" "the bound" boundShape _.atLeast
  )
  where
  mk m o a = { metric: m, over: o, atLeast: a }

metricShape :: Shape MetricDoc
metricShape = object "Metric"
  ( mk <$> fieldOf "entities" "the Entities summed" (listOf stringShape) _.entities
      <*> fieldOf "measure" "the measure" measureShape _.measure
      <*> fieldOf "negated" "true for \"at most\" (default false)" (optionalOf boolShape) _.negated
  )
  where
  mk e m n = { entities: e, measure: m, negated: n }

boundShape :: Shape BoundDoc
boundShape = premiseSum "Bound"
  [ literal qShape (either Just (const Nothing)) Left
  , embedded metricShape (either (const Nothing) Just) Right
  ]

-- | What covers an Actor when it has no player: an Aggregate's accounts, an in-world Policy, or nothing.
fallbackShape :: Shape Fallback
fallbackShape = premiseSum "Fallback"
  [ kindArg { tag: coverageTag, desc: "the Aggregate that retains the accounts" } idShape
      ( case _ of
          ImplicitCoverage a -> Just a
          _ -> Nothing
      )
      ImplicitCoverage
  , kindArg { tag: policyTag, desc: "the same accounts, driven by an in-world Policy" } idShape
      ( case _ of
          InstitutionalPolicy p -> Just p
          _ -> Nothing
      )
      InstitutionalPolicy
  , bare { tag: omissionTag, desc: "the Actor's accounts are permitted to lapse" } PermittedOmission
  ]

-- | A measure: a holding, a capacity, a shortfall, a count of shortfall rounds, payee receipts, or a volume.
measureShape :: Shape Measure
measureShape = premiseSum "Measure"
  [ kindArg { tag: holdingTag, desc: "an Entity's holding of a Resource" } idShape
      ( case _ of
          HoldingOf r -> Just r
          _ -> Nothing
      )
      HoldingOf
  , kindArg { tag: capacityTag, desc: "an Entity's amount of a capacity" } idShape
      ( case _ of
          CapacityAmount c -> Just c
          _ -> Nothing
      )
      CapacityAmount
  , kindArg { tag: shortfallTag, desc: "the shortfall of a Resource in a round" } idShape
      ( case _ of
          ShortfallOf r -> Just r
          _ -> Nothing
      )
      ShortfallOf
  , kindArg { tag: shortfallRoundsTag, desc: "the rounds the Entity was short of a Resource" } idShape
      ( case _ of
          ShortfallRounds r -> Just r
          _ -> Nothing
      )
      ShortfallRounds
  , bare { tag: payeeReceiptsTag, desc: "Money received as a facility payee" } PayeeReceipts
  , kindArg { tag: volumeTag, desc: "the volume run through a transformation" } idShape
      ( case _ of
          VolumeOf t -> Just t
          _ -> Nothing
      )
      VolumeOf
  ]

-- | The span a criterion is evaluated over.
spanShape :: Shape Span
spanShape = premiseSum "Span"
  [ bare { tag: finalTag, desc: "the last round" } Final
  , bare { tag: everyRoundTag, desc: "every round" } EveryRound
  , bare { tag: cumulativeTag, desc: "the whole game, summed" } Cumulative
  ]

-- Conversions ---------------------------------------------------------------------------------

-- | An Actor definition with its defaults filled in. An Actor with no definition has none.
definitionToCore :: DefinitionDoc -> ActorDefinition
definitionToCore d =
  { required: fromMaybe false d.required
  , seat: fromMaybe true d.seat
  , npc: map Id (fromMaybe [] d.npc)
  , fallback: d.fallback
  , objective: map objectiveToCore d.objective
  , description: fromMaybe "" d.description
  , dependency: fromMaybe "" d.dependency
  }

objectiveToCore :: ObjectiveDoc -> Objective
objectiveToCore o = { description: o.description, criteria: map criterionToCore o.criteria }

criterionToCore :: CriterionDoc -> Criterion
criterionToCore c = { metric: metricToCore c.metric, over: fromMaybe Final c.over, atLeast: boundToCore c.atLeast }

metricToCore :: MetricDoc -> MetricRef
metricToCore m = { entities: map Id m.entities, measure: m.measure, negated: fromMaybe false m.negated }

boundToCore :: BoundDoc -> Bound
boundToCore = either Fixed (Of <<< metricToCore)
