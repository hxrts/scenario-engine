-- | Golden fixtures and their expectations (docs/05_test_instance.md §21–§27): canonical
-- | Intent literals, Proposals and their signatures, exact expected results, multi-round
-- | fixtures, Representation bounds, and the rules in force from the start of a world. Every
-- | expected value is a literal, never computed.
module Schema.Build.Fixture
  ( Intent
  , intentJson
  , tradeI
  , transferI
  , transformI
  , enactTariffI
  , enactAccessBanI
  , enactWithholdingI
  , proposal
  , expect
  , ref
  , refAt
  , delivery
  , procedureOutcome
  , roundOf
  , representation
  , accessBanRule
  , tariffRule
  , withholdingRule
  ) where

import Prelude

import Schema.Tags as BT
import Schema.Build.Base (Built(..), Keyed, Qty, build, tag)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Prim.Row (class Union)

-- | A canonical Intent, as a fixture's Proposals carry it.
newtype Intent = Intent Json

intentJson :: Intent -> Json
intentJson (Intent j) = j

instance EncodeJson Intent where
  encodeJson (Intent j) = j

tradeI
  :: forall r rest
   . Union r rest (market :: String, quantity :: Qty, limit :: Qty)
  => EncodeJson (Record r)
  => Record r
  -> Intent
tradeI = Intent <<< tag "trade" <<< encodeJson

transferI
  :: forall r rest
   . Union r rest (from :: String, to :: String, resource :: String, quantity :: Qty)
  => EncodeJson (Record r)
  => Record r
  -> Intent
transferI = Intent <<< tag "transfer" <<< encodeJson

transformI
  :: forall r rest
   . Union r rest (by :: String, transformation :: String, quantity :: Qty)
  => EncodeJson (Record r)
  => Record r
  -> Intent
transformI = Intent <<< tag "transform" <<< encodeJson

enactTariffI
  :: forall r rest
   . Union r rest (jurisdiction :: String, origin :: String, rate :: Qty)
  => EncodeJson (Record r)
  => Record r
  -> Intent
enactTariffI r = Intent (tag BT.enactKind (tag BT.tariffKind (encodeJson r)))

enactAccessBanI
  :: forall r rest
   . Union r rest (jurisdiction :: String, target :: String)
  => EncodeJson (Record r)
  => Record r
  -> Intent
enactAccessBanI r = Intent (tag BT.enactKind (tag BT.accessBanKind (encodeJson r)))

enactWithholdingI
  :: forall r rest
   . Union r rest (capacity :: String, units :: Qty)
  => EncodeJson (Record r)
  => Record r
  -> Intent
enactWithholdingI r = Intent (tag BT.enactKind (tag BT.withholdingKind (encodeJson r)))

-- | A Proposal and the signatures it gathered. Ids follow from the author and order.
proposal
  :: forall r rest
   . Union r rest (author :: String, intents :: Array Intent, signers :: Array String, signatures :: Array String, procedure :: String)
  => EncodeJson (Record r)
  => Record r
  -> Built "proposal"
proposal = build

-- | A Proposal named in an expectation: its author's first Proposal in the round.
ref :: String -> Built "proposal ref"
ref author = Built (encodeJson author)

-- | The Proposal at a position among its author's Proposals in the round.
refAt :: String -> Int -> Built "proposal ref"
refAt author index = Built (encodeJson { author, index })

-- | Exact expected results after a round. Only the fields given are checked: `deliveries: []`
-- | asserts that nothing was delivered, while an absent field asserts nothing.
expect
  :: forall r rest
   . Union r rest
       ( accounts :: Keyed (Keyed Qty)
       , reserves :: Keyed (Keyed Qty)
       , deliveries :: Array (Built "delivery")
       , rejected :: Array (Built "proposal ref")
       , lapsed :: Array (Built "proposal ref")
       , factors :: Keyed (Keyed Qty)
       , scheduled :: Int
       , procedures :: Array (Built "procedure outcome")
       )
  => EncodeJson (Record r)
  => Record r
  -> Built "expect"
expect = build

delivery
  :: forall r rest
   . Union r rest (entity :: String, resource :: String, quantity :: Qty)
  => EncodeJson (Record r)
  => Record r
  -> Built "delivery"
delivery = build

-- | The outcome of a procedure Proposal: its author's first Proposal unless `index` says otherwise.
procedureOutcome
  :: forall r rest
   . Union r rest (author :: String, index :: Int, support :: Qty, executed :: Boolean)
  => EncodeJson (Record r)
  => Record r
  -> Built "procedure outcome"
procedureOutcome = build

-- | One round of a multi-round fixture.
roundOf
  :: forall r rest
   . Union r rest (proposals :: Array (Built "proposal"), expect :: Built "expect")
  => EncodeJson (Record r)
  => Record r
  -> Built "round"
roundOf = build

-- | A Representation bound in a fixture's world (flattened, as the engine stores it).
representation
  :: forall r rest
   . Union r rest (id :: String, actor :: String, aggregate :: String, domain :: String, weight :: Qty, reference :: Qty, shrinkage :: Qty, bound :: Qty, noiseSpread :: Qty)
  => EncodeJson (Record r)
  => Record r
  -> Built "representation"
representation = build

-- | Rules in force from the start of a world.
accessBanRule :: { jurisdiction :: String, target :: String } -> Built "rule"
accessBanRule r = Built (encodeJson { accessBan: r })

tariffRule :: { jurisdiction :: String, origin :: String, rate :: Qty } -> Built "rule"
tariffRule r = Built (encodeJson { tariff: r })

withholdingRule :: { capacity :: String, units :: Qty } -> Built "rule"
withholdingRule r = Built (encodeJson { withholding: r })
