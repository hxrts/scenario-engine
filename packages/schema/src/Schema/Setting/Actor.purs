-- | The setting format of an Actor's definition (docs/03 §6): its seat eligibility, fallback,
-- | objective, and the criteria and measures the objective names. Each is a `Shape` value over the
-- | core type (`Schema.Gameboard`); a key the decoder defaults is omitted when it holds its default.
-- | The setting tags are private to this module.
module Schema.Setting.Actor
  ( definitionShape
  , metricShape
  , boundShape
  , fallbackShape
  ) where

import Core.Num (qShape)
import Core.Shape (Shape, arrayOf, atBare, atEmbedded, atKindArg, atLiteral, boolShape, defaultsTo, field, genericSettingSum, listOf, optionalOf, recordOf, stringShape, (&.))
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
definitionShape = recordOf "Definition"
  ( field @"required" "must be seated (default false)" (defaultsTo false boolShape)
      &. field @"seat" "may be seated (default true)" (defaultsTo true boolShape)
      &. field @"npc" "the NPC Policies it may take" (arrayOf idShape)
      &. field @"fallback" "what covers it when it has no player" (optionalOf fallbackShape)
      &. field @"objective" "its measurable objective" (optionalOf objectiveShape)
      &. field @"description" "what the Actor does (prose)" (defaultsTo "" stringShape)
      &. field @"dependency" "what the Actor depends on (prose)" (defaultsTo "" stringShape)
  )

objectiveShape :: Shape Objective
objectiveShape = recordOf "Objective"
  ( field @"description" "what the objective is" stringShape
      &. field @"criteria" "the criteria it is met by" (listOf criterionShape)
  )

criterionShape :: Shape Criterion
criterionShape = recordOf "Criterion"
  ( field @"metric" "the measured quantity" metricShape
      &. field @"over" "the span of rounds (default final)" (defaultsTo Final spanShape)
      &. field @"atLeast" "the bound" boundShape
  )

metricShape :: Shape MetricRef
metricShape = recordOf "Metric"
  ( field @"entities" "the Entities summed" (listOf idShape)
      &. field @"measure" "the measure" measureShape
      &. field @"negated" "true for \"at most\" (default false)" (defaultsTo false boolShape)
  )

boundShape :: Shape Bound
boundShape = genericSettingSum "Bound"
  { "Fixed": atLiteral qShape
  , "Of": atEmbedded metricShape
  }

-- | What covers an Actor when it has no player: an Aggregate's accounts, an in-world Policy, or nothing.
fallbackShape :: Shape Fallback
fallbackShape = genericSettingSum "Fallback"
  { "ImplicitCoverage": atKindArg { tag: coverageTag, desc: "the Aggregate that retains the accounts" } idShape
  , "InstitutionalPolicy": atKindArg { tag: policyTag, desc: "the same accounts, driven by an in-world Policy" } idShape
  , "PermittedOmission": atBare { tag: omissionTag, desc: "the Actor's accounts are permitted to lapse" }
  }

-- | A measure: a holding, a capacity, a shortfall, a count of shortfall rounds, payee receipts, or a volume.
measureShape :: Shape Measure
measureShape = genericSettingSum "Measure"
  { "HoldingOf": atKindArg { tag: holdingTag, desc: "an Entity's holding of a Resource" } idShape
  , "CapacityAmount": atKindArg { tag: capacityTag, desc: "an Entity's amount of a capacity" } idShape
  , "ShortfallOf": atKindArg { tag: shortfallTag, desc: "the shortfall of a Resource in a round" } idShape
  , "ShortfallRounds": atKindArg { tag: shortfallRoundsTag, desc: "the rounds the Entity was short of a Resource" } idShape
  , "PayeeReceipts": atBare { tag: payeeReceiptsTag, desc: "Money received as a facility payee" }
  , "VolumeOf": atKindArg { tag: volumeTag, desc: "the volume run through a transformation" } idShape
  }

-- | The span a criterion is evaluated over.
spanShape :: Shape Span
spanShape = genericSettingSum "Span"
  { "Final": atBare { tag: finalTag, desc: "the last round" }
  , "EveryRound": atBare { tag: everyRoundTag, desc: "every round" }
  , "Cumulative": atBare { tag: cumulativeTag, desc: "the whole game, summed" }
  }
