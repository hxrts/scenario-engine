-- | Dials and the cuts of Meridian: the default dials are the reference numbers, a dial changes what it names,
-- | and Meridian's Premises compose back to the reference world, value for value.
module Test.Premises.Dials (spec) where

import Prelude

import Core.Num as Q
import Core.Shape (Format(..), encodeWith)
import Data.Argonaut.Core (stringify)
import Data.Array (find)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Premises.Compose (refOf)
import Premises.Load (composedOf)
import Premises.Meridian (defaultDials, composition, compositionWith, meridian)
import Premises.Meridian.Names (n)
import Schema.Premise.Composition (compositionShape)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = describe "Dials and Meridian's Premises" do
  it "the default dials reproduce the reference PremiseComposition" do
    let text g = stringify (encodeWith Authored compositionShape g)
    text (compositionWith defaultDials) `shouldEqual` text composition
  it "a dial changes what it names" do
    let
      doubled = compositionWith defaultDials { flow = \f -> f { amount = Q.add f.amount f.amount } }
      firstFlow = find (\f -> f.from == n.usWorkers && f.to == n.us)
    map _.amount (firstFlow doubled.authored.circulation.flows) `shouldEqual` Just (Q.fromInt 30)
  it "Meridian's Premises compose back to the reference world, value for value" do
    let source = meridian defaultDials
    map _.id source.premises `shouldEqual` [ "meridian-energy", "meridian-shipping", "meridian-defence", "meridian-soft-power", "meridian-actors" ]
    map _.version source.premises `shouldEqual` [ "v0", "v0", "v0", "v0", "v0" ]
    case composedOf { id: source.id, version: source.version, premises: source.premises } of
      Left e -> fail e
      Right c -> do
        -- parameters keep their authored order only in the DSL; the Json comparison ignores key order
        (encodeWith Authored compositionShape c == encodeWith Authored compositionShape composition) `shouldEqual` true
        c.premises `shouldEqual` map refOf source.premises
