-- | Shared building blocks of the scenario DSL: exact quantities, keyed maps, the built-JSON
-- | type, and the tagged strings the decoders split on. The World, Decision and Fixture
-- | modules build on these; nothing here depends on them.
module DSL.Base
  ( Qty
  , class ToQty
  , q
  , Keyed
  , keyed
  , (:=)
  , entry
  , Built(..)
  , builtJson
  , build
  , tag
  , Tagged
  , Capability
  , trade
  , transfer
  , transform
  , enactTariff
  , enactAccessBan
  , enactWithholding
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
  ) where

import Prelude

import Board.Tags as T
import Core.Num as Q
import Data.Argonaut.Core (Json, fromObject, fromString)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Partial.Unsafe (unsafeCrashWith)

-- Quantities and maps ----------------------------------------------------------------

-- | An exact decimal quantity, written as the decoders read it (a string).
newtype Qty = Qty String

instance EncodeJson Qty where
  encodeJson (Qty s) = fromString s

-- | Quantities are written as an `Int` or as a decimal `String` ("0.02"), never a
-- | floating-point `Number`. A string must pass the one decimal parser (`Q.parse`, at most 4
-- | decimals), so a bad literal fails when the scenario is authored, not when it is decoded.
class ToQty a where
  q :: a -> Qty

instance ToQty Qty where
  q = identity

instance ToQty Int where
  q = Qty <<< show

instance ToQty String where
  q s = case Q.parse s of
    Just _ -> Qty s
    Nothing -> unsafeCrashWith ("not a fixed-point quantity (sign, digits, at most 4 decimals): " <> s)

-- | A JSON object from key–value pairs: holdings, catalogs, parameters, seats.
newtype Keyed a = Keyed (Array (Tuple String a))

instance EncodeJson a => EncodeJson (Keyed a) where
  encodeJson (Keyed kvs) = fromObject (Object.fromFoldable (map (\(Tuple k v) -> Tuple k (encodeJson v)) kvs))

derive newtype instance Semigroup (Keyed a)

keyed :: forall a. Array (Tuple String a) -> Keyed a
keyed = Keyed

-- | A holding: `"M" := 260`, `"E" := "0.5"`.
entry :: forall a. ToQty a => String -> a -> Tuple String Qty
entry k v = Tuple k (q v)

infix 6 entry as :=

-- | A built JSON fragment of one kind (a market, a facility, an entity, …).
newtype Built (kind :: Symbol) = Built Json

-- | The JSON a builder emits, for decoding in tests and tooling.
builtJson :: forall kind. Built kind -> Json
builtJson (Built j) = j

instance EncodeJson (Built kind) where
  encodeJson (Built j) = j

-- | Encode a record whose fields the caller's `Union` constraint has already checked.
build :: forall kind r. EncodeJson (Record r) => Record r -> Built kind
build = Built <<< encodeJson

-- | A one-key JSON object: the tagged form the decoders read.
tag :: String -> Json -> Json
tag k v = fromObject (Object.singleton k v)

-- Tagged strings ---------------------------------------------------------------------

-- | A string the decoders split on `:` (capabilities, fallbacks, measures, …).
newtype Tagged (kind :: Symbol) = Tagged String

instance EncodeJson (Tagged kind) where
  encodeJson (Tagged s) = fromString s

type Capability = Tagged "capability"

trade :: String -> Capability
trade m = Tagged (T.tag T.tradeKind m)

transfer :: String -> Capability
transfer r = Tagged (T.tag T.transferKind r)

transform :: String -> Capability
transform t = Tagged (T.tag T.transformKind t)

enactTariff :: Capability
enactTariff = Tagged (T.tag T.enactKind T.tariffKind)

enactAccessBan :: Capability
enactAccessBan = Tagged (T.tag T.enactKind T.accessBanKind)

enactWithholding :: Capability
enactWithholding = Tagged (T.tag T.enactKind T.withholdingKind)

free :: Tagged "transferability"
free = Tagged "Free"

restricted :: Tagged "transferability"
restricted = Tagged "Restricted"

nonTransferable :: Tagged "transferability"
nonTransferable = Tagged "NonTransferable"

-- | The Actor's accounts and activity stay in an Aggregate.
coverage :: String -> Tagged "fallback"
coverage a = Tagged (T.tag T.coverageKind a)

-- | The same accounts, driven by an in-world Policy.
institutional :: String -> Tagged "fallback"
institutional p = Tagged (T.tag T.policyKind p)

omission :: Tagged "fallback"
omission = Tagged T.omissionKind

holdingOf :: String -> Tagged "measure"
holdingOf r = Tagged (T.tag T.holdingKind r)

capacityOf :: String -> Tagged "measure"
capacityOf c = Tagged (T.tag T.capacityKind c)

shortfallOf :: String -> Tagged "measure"
shortfallOf r = Tagged (T.tag T.shortfallKind r)

shortfallRounds :: String -> Tagged "measure"
shortfallRounds r = Tagged (T.tag T.shortfallRoundsKind r)

payeeReceipts :: Tagged "measure"
payeeReceipts = Tagged T.payeeReceiptsKind

volumeOf :: String -> Tagged "measure"
volumeOf t = Tagged (T.tag T.volumeKind t)

final :: Tagged "span"
final = Tagged T.finalKind

everyRound :: Tagged "span"
everyRound = Tagged T.everyRoundKind

cumulative :: Tagged "span"
cumulative = Tagged T.cumulativeKind

sales :: String -> Tagged "share source"
sales r = Tagged (T.tag T.salesKind r)

payee :: Tagged "share source"
payee = Tagged T.payeeKind
