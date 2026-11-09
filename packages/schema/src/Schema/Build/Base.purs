-- | The vocabulary of the setting DSL's world and fixtures: keyed lists (`"M" := 260`), and the
-- | typed values a setting names — capabilities, Measures, spans, fallbacks, share sources. Each is
-- | a core value, so a setting is written with the same types the engine reads; nothing here builds
-- | JSON (the shapes in `Schema.Setting.*` do).
module Schema.Build.Base
  ( keyed
  , (:=)
  , entry
  , mayTrade
  , mayTransfer
  , mayTransform
  , mayEnact
  , coverage
  , institutional
  , holdingOf
  , shortfallOf
  , shortfallRounds
  , payeeReceipts
  , volumeOf
  , final
  , everyRound
  , cumulative
  , sales
  , payee
  ) where

import Prelude

import Core.Num (Q)
import Core.Types (Capability(..), EntityTag, Id, MarketTag, PolicyTag, ResourceTag, RuleKindTag, TransformationTag)
import Schema.Build.Named (class Named, named)
import Schema.Setting.Query (ShareMeasure)
import Schema.Recurring as Recurring
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Schema.Build.Quantity (class ToQty, q)
import Schema.Gameboard (Fallback(..), Measure(..), Span(..))

-- | A Map from named values, keyed by id.
keyed :: forall k t a. Named k t => Array (Tuple k a) -> Map (Id t) a
keyed = Map.fromFoldable <<< map (\(Tuple k v) -> Tuple (named k) v)

-- | A holding: `"M" := 260`, `"E" := "0.5"`.
entry :: forall k a. ToQty a => k -> a -> Tuple k Q
entry k v = Tuple k (q v)

infix 6 entry as :=

-- Capabilities -----------------------------------------------------------------------------

mayTrade :: forall a. Named a MarketTag => a -> Capability
mayTrade m = MayTrade (named m)

mayTransfer :: forall a. Named a ResourceTag => a -> Capability
mayTransfer r = MayTransfer (named r)

mayTransform :: forall a. Named a TransformationTag => a -> Capability
mayTransform t = MayTransform (named t)

-- | May enact a Rule of a declared kind.
mayEnact :: forall a. Named a RuleKindTag => a -> Capability
mayEnact = MayEnact <<< named

-- Fallbacks --------------------------------------------------------------------------------

-- | The Actor's accounts and activity stay in an Aggregate.
coverage :: forall a. Named a EntityTag => a -> Fallback
coverage a = ImplicitCoverage (named a)

-- | The same accounts, driven by an in-world Policy.
institutional :: forall a. Named a PolicyTag => a -> Fallback
institutional p = InstitutionalPolicy (named p)

-- Measures and spans -----------------------------------------------------------------------

holdingOf :: forall a. Named a ResourceTag => a -> Measure
holdingOf r = HoldingOf (named r)

shortfallOf :: forall a. Named a ResourceTag => a -> Measure
shortfallOf r = ShortfallOf (named r)

shortfallRounds :: forall a. Named a ResourceTag => a -> Measure
shortfallRounds r = ShortfallRounds (named r)

payeeReceipts :: Measure
payeeReceipts = PayeeReceipts

volumeOf :: forall a. Named a TransformationTag => a -> Measure
volumeOf t = VolumeOf (named t)

final :: Span
final = Final

everyRound :: Span
everyRound = EveryRound

cumulative :: Span
cumulative = Cumulative

-- Share sources ----------------------------------------------------------------------------

sales :: forall a. Named a ResourceTag => a -> ShareMeasure
sales r = Recurring.salesOf (named r)

payee :: ShareMeasure
payee = Recurring.payeeReceipts
