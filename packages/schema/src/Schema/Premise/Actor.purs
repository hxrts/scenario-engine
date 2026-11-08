-- | The premise format of an Actor's definition (docs/03 §6): its seat eligibility, fallback,
-- | objective, and the criteria and measures the objective names. Each is a `Shape` value over the
-- | core type (`Schema.Gameboard`); a key the decoder defaults is omitted when it holds its default.
-- | The premise tags are private to this module.
module Schema.Premise.Actor
  ( definitionShape
  , metricShape
  , boundShape
  , fallbackShape
  ) where

import Prelude

import Core.Num (qShape)
import Core.Shape (Shape, arrayOf, atBare, atEmbedded, atKindArg, atLiteral, boolShape, defaultsTo, fieldOf, genericPremiseSum, listOf, object, optionalOf, stringShape)
import Core.Types (idShape)
import Schema.Gameboard (ActorDefinition, Bound, Criterion, Fallback, Measure, MetricRef, Objective, Span(..))

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

definitionShape :: Shape ActorDefinition
definitionShape = object "Definition"
  ( mk <$> fieldOf "required" "must be seated (default false)" (defaultsTo false boolShape) _.required
      <*> fieldOf "seat" "may be seated (default true)" (defaultsTo true boolShape) _.seat
      <*> fieldOf "npc" "the NPC Policies it may take" (arrayOf idShape) _.npc
      <*> fieldOf "fallback" "what covers it when it has no player" (optionalOf fallbackShape) _.fallback
      <*> fieldOf "objective" "its measurable objective" (optionalOf objectiveShape) _.objective
      <*> fieldOf "description" "what the Actor does (prose)" (defaultsTo "" stringShape) _.description
      <*> fieldOf "dependency" "what the Actor depends on (prose)" (defaultsTo "" stringShape) _.dependency
  )
  where
  mk r s n f o d p = { required: r, seat: s, npc: n, fallback: f, objective: o, description: d, dependency: p }

objectiveShape :: Shape Objective
objectiveShape = object "Objective"
  ( mk <$> fieldOf "description" "what the objective is" stringShape _.description
      <*> fieldOf "criteria" "the criteria it is met by" (listOf criterionShape) _.criteria
  )
  where
  mk d c = { description: d, criteria: c }

criterionShape :: Shape Criterion
criterionShape = object "Criterion"
  ( mk <$> fieldOf "metric" "the measured quantity" metricShape _.metric
      <*> fieldOf "over" "the span of rounds (default final)" (defaultsTo Final spanShape) _.over
      <*> fieldOf "atLeast" "the bound" boundShape _.atLeast
  )
  where
  mk m o a = { metric: m, over: o, atLeast: a }

metricShape :: Shape MetricRef
metricShape = object "Metric"
  ( mk <$> fieldOf "entities" "the Entities summed" (listOf idShape) _.entities
      <*> fieldOf "measure" "the measure" measureShape _.measure
      <*> fieldOf "negated" "true for \"at most\" (default false)" (defaultsTo false boolShape) _.negated
  )
  where
  mk e m n = { entities: e, measure: m, negated: n }

boundShape :: Shape Bound
boundShape = genericPremiseSum "Bound"
  { "Fixed": atLiteral qShape
  , "Of": atEmbedded metricShape
  }

-- | What covers an Actor when it has no player: an Aggregate's accounts, an in-world Policy, or nothing.
fallbackShape :: Shape Fallback
fallbackShape = genericPremiseSum "Fallback"
  { "ImplicitCoverage": atKindArg { tag: coverageTag, desc: "the Aggregate that retains the accounts" } idShape
  , "InstitutionalPolicy": atKindArg { tag: policyTag, desc: "the same accounts, driven by an in-world Policy" } idShape
  , "PermittedOmission": atBare { tag: omissionTag, desc: "the Actor's accounts are permitted to lapse" }
  }

-- | A measure: a holding, a capacity, a shortfall, a count of shortfall rounds, payee receipts, or a volume.
measureShape :: Shape Measure
measureShape = genericPremiseSum "Measure"
  { "HoldingOf": atKindArg { tag: holdingTag, desc: "an Entity's holding of a Resource" } idShape
  , "CapacityAmount": atKindArg { tag: capacityTag, desc: "an Entity's amount of a capacity" } idShape
  , "ShortfallOf": atKindArg { tag: shortfallTag, desc: "the shortfall of a Resource in a round" } idShape
  , "ShortfallRounds": atKindArg { tag: shortfallRoundsTag, desc: "the rounds the Entity was short of a Resource" } idShape
  , "PayeeReceipts": atBare { tag: payeeReceiptsTag, desc: "Money received as a facility payee" }
  , "VolumeOf": atKindArg { tag: volumeTag, desc: "the volume run through a transformation" } idShape
  }

-- | The span a criterion is evaluated over.
spanShape :: Shape Span
spanShape = genericPremiseSum "Span"
  { "Final": atBare { tag: finalTag, desc: "the last round" }
  , "EveryRound": atBare { tag: everyRoundTag, desc: "every round" }
  , "Cumulative": atBare { tag: cumulativeTag, desc: "the whole game, summed" }
  }
