-- | Facade for the scenario DSL: a small typed vocabulary for authoring scenarios and golden
-- | fixtures in PureScript, compiled to the JSON that `board` and `policy` decode. The
-- | vocabulary is split by concern across `DSL.Base` (quantities, built JSON, tagged strings),
-- | `DSL.World`, `DSL.Decision` and `DSL.Fixture`; importers write `import DSL` and see the
-- | same names as before. Every term is a thin wrapper around that JSON, so the format stays
-- | defined in one place: the decoders.
-- |
-- | Record builders take any subset of their allowed fields, given as a row `Union`: a
-- | misspelt or mistyped field fails to compile, and only the fields given are emitted. An
-- | absent field takes the decoder's default, or is a decode error if the decoder requires
-- | it. Tagged strings (capabilities, fallbacks, measures, spans, share sources) are built
-- | from the constants in `Board.Tags`, the same constants the decoders match on.
module DSL
  ( module DSL.Base
  , module DSL.World
  , module DSL.Decision
  , module DSL.Fixture
  ) where

import DSL.Base
  ( Qty
  , class ToQty
  , q
  , Keyed
  , keyed
  , (:=)
  , entry
  , Built
  , builtJson
  , Capability
  , trade
  , transfer
  , transform
  , enactTariff
  , enactAccessBan
  , enactWithholding
  , Tagged
  , free
  , restricted
  , nonTransferable
  , coverage
  , institutional
  , omission
  , holdingOf
  , capacityOf
  , shortfallOf
  , shortfallRounds
  , payeeReceipts
  , volumeOf
  , final
  , everyRound
  , cumulative
  , sales
  , payee
  )
import DSL.World (class ToBound, aggregate, assignment, atLeast, bound, collective, cost, criterion, definition, effect, entity, estimator, facility, finishes, flow, market, metric, neverShort, objective, own, production, requirement, resource, scheme, share)
import DSL.Decision (Clause, Cond, Expr, SignRule, Template, choose, deal, signIf, enactAccessBanT, enactWithholdingT, repealT, enactTariffT, eq, factor, ge, gt, holding, le, lit, max', mul, noAction, not', param, price, procedureT, program, reciprocate, restrictedLastRound, restrictionInForce, round, rule, signAll, signFacility, signProcedures, sub, tradeT, transformT, transformedLastRound)
import DSL.Fixture (Intent, accessBanRule, delivery, enactAccessBanI, enactTariffI, enactWithholdingI, expect, intentJson, procedureOutcome, proposal, ref, refAt, representation, roundOf, tariffRule, tradeI, transferI, transformI, withholdingRule)
