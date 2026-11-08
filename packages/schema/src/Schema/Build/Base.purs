-- | The vocabulary of the premise DSL's world and fixtures: keyed lists (`"M" := 260`), and the
-- | typed values a premise names — capabilities, Measures, spans, fallbacks, share sources. Each is
-- | a core value, so a premise is written with the same types the engine reads; nothing here builds
-- | JSON (the shapes in `Schema.Premise.*` do).
module Schema.Build.Base
  ( module Exports
  , keyed
  , (:=)
  , entry
  , mayTrade
  , mayTransfer
  , mayTransform
  , mayEnact
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
  ) where

import Prelude

import Core.Num (Q)
import Core.Types (Capability(..), Id(..))
import Schema.Recurring (ShareSource)
import Schema.Recurring as Recurring
import Core.World as W
import Data.Tuple (Tuple(..))
import Schema.Build.Quantity (class ToQty, q)
import Schema.Gameboard (Fallback(..), Measure(..), Span(..))
import Schema.Premise.World (Keyed) as Exports
import Schema.Premise.World (Keyed)

keyed :: forall a. Array (Tuple String a) -> Keyed a
keyed = identity

-- | A holding: `"M" := 260`, `"E" := "0.5"`.
entry :: forall a. ToQty a => String -> a -> Tuple String Q
entry k v = Tuple k (q v)

infix 6 entry as :=

-- Capabilities -----------------------------------------------------------------------------

mayTrade :: String -> Capability
mayTrade m = MayTrade (Id m)

mayTransfer :: String -> Capability
mayTransfer r = MayTransfer (Id r)

mayTransform :: String -> Capability
mayTransform t = MayTransform (Id t)

-- | May enact a Rule of a declared kind.
mayEnact :: String -> Capability
mayEnact = MayEnact <<< Id

-- Fallbacks --------------------------------------------------------------------------------

-- | The Actor's accounts and activity stay in an Aggregate.
coverage :: String -> Fallback
coverage a = ImplicitCoverage (Id a)

-- | The same accounts, driven by an in-world Policy.
institutional :: String -> Fallback
institutional p = InstitutionalPolicy (Id p)

omission :: Fallback
omission = PermittedOmission

-- Measures and spans -----------------------------------------------------------------------

holdingOf :: String -> Measure
holdingOf r = HoldingOf (Id r)

capacityOf :: String -> Measure
capacityOf c = CapacityAmount (Id c)

shortfallOf :: String -> Measure
shortfallOf r = ShortfallOf (Id r)

shortfallRounds :: String -> Measure
shortfallRounds r = ShortfallRounds (Id r)

payeeReceipts :: Measure
payeeReceipts = PayeeReceipts

volumeOf :: String -> Measure
volumeOf t = VolumeOf (Id t)

final :: Span
final = Final

everyRound :: Span
everyRound = EveryRound

cumulative :: Span
cumulative = Cumulative

-- Share sources ----------------------------------------------------------------------------

sales :: String -> ShareSource
sales r = Recurring.SalesOf (Id r)

payee :: ShareSource
payee = Recurring.PayeeReceipts
