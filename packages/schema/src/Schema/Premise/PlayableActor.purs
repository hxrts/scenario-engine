-- | The premise format of an Actor's definition (docs/03 §6): its seat eligibility, fallback,
-- | objective, and the criteria and measures the objective names. Each is a `Shape` value over the
-- | core type (`Schema.Composition`); a key the decoder defaults is omitted when it holds its default.
-- | The premise tags are private to this module.
module Schema.Premise.PlayableActor
  ( definitionShape
  , criterionShape
  , metricShape
  , boundShape
  , fallbackShape
  ) where

import Core.Num (qShape)
import Core.Shape (Shape, arrayOf, atBare, atEmbedded, atKindArg, atLiteral, boolShape, defaultsTo, field, genericPremiseSum, listOf, optionalOf, recordOf, stringShape, (&.))
import Laws.Types (idShape)
import Schema.Composition (ActorDefinition, Bound, Criterion, Fallback, Measure, MetricRef, Objective, Span(..))

-- Tags ----------------------------------------------------------------------------------------

coverageTag :: String
coverageTag = "coverage"

strategyTag :: String
strategyTag = "strategy"

omissionTag :: String
omissionTag = "omission"

holdingTag :: String
holdingTag = "holding"

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
      &. field @"nonPlayableActor" "the NonPlayableActor Strategies it may take" (arrayOf idShape)
      &. field @"routine" "a Strategy that runs beside whoever plays the seat" (optionalOf idShape)
      &. field @"fallback" "what covers it when it has no player" (optionalOf fallbackShape)
      &. field @"objective" "its measurable objective" (optionalOf objectiveShape)
      &. field @"description" "what the PlayableActor does (prose)" (defaultsTo "" stringShape)
      &. field @"dependency" "what the PlayableActor depends on (prose)" (defaultsTo "" stringShape)
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
  ( field @"actors" "the Actors summed" (listOf idShape)
      &. field @"measure" "the measure" measureShape
      &. field @"negated" "true for \"at most\" (default false)" (defaultsTo false boolShape)
  )

boundShape :: Shape Bound
boundShape = genericPremiseSum "Bound"
  { "Fixed": atLiteral qShape
  , "Of": atEmbedded metricShape
  }

-- | What covers an Actor when it has no player: an Aggregate's accounts, an in-world Strategy, or nothing.
fallbackShape :: Shape Fallback
fallbackShape = genericPremiseSum "Fallback"
  { "ImplicitCoverage": atKindArg { tag: coverageTag, desc: "the Aggregate that retains the accounts" } idShape
  , "InstitutionalStrategy": atKindArg { tag: strategyTag, desc: "the same accounts, driven by an in-world Strategy" } idShape
  , "PermittedOmission": atBare { tag: omissionTag, desc: "the PlayableActor's accounts are permitted to lapse" }
  }

-- | A measure: a holding, a shortfall, a count of shortfall rounds, payee receipts, or a volume.
measureShape :: Shape Measure
measureShape = genericPremiseSum "Measure"
  { "HoldingOf": atKindArg { tag: holdingTag, desc: "an Actor's holding of a Resource" } idShape
  , "ShortfallOf": atKindArg { tag: shortfallTag, desc: "the shortfall of a Resource in a round" } idShape
  , "ShortfallRounds": atKindArg { tag: shortfallRoundsTag, desc: "the rounds the Actor was short of a Resource" } idShape
  , "PayeeReceipts": atBare { tag: payeeReceiptsTag, desc: "Money received as a transformation payee" }
  , "VolumeOf": atKindArg { tag: volumeTag, desc: "the volume run through a transformation" } idShape
  }

-- | The span a criterion is evaluated over.
spanShape :: Shape Span
spanShape = genericPremiseSum "Span"
  { "Final": atBare { tag: finalTag, desc: "the last round" }
  , "EveryRound": atBare { tag: everyRoundTag, desc: "every round" }
  , "Cumulative": atBare { tag: cumulativeTag, desc: "the whole game, summed" }
  }
