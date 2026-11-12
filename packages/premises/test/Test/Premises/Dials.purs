-- | Dials and the cuts of Pacific: the default dials are the reference numbers, a dial changes what it names,
-- | and Pacific's Premises compose back to the reference world, value for value.
module Test.Premises.Dials (spec) where

import Prelude

import Core.Shape (Format(..), encodeWith)
import Data.Argonaut.Core (stringify)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Foreign.Object as Object
import Premises.Load (composedOf)
import Premises.Pacific (composition, compositionWith, defaultDials, pacific)
import Schema.Build.Quantity (q)
import Schema.Premise.Composition (compositionShape)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = describe "Dials and Pacific's Premises" do
  it "the default dials are the reference composition" do
    text (compositionWith defaultDials) `shouldEqual` text composition
  it "a dial changes only what it names: the tariff rate is the parameter the tariffs read" do
    let doubled = compositionWith defaultDials { tariffRate = q 2 }
    Object.lookup "tariff-rate" doubled.parameters `shouldEqual` Just (q 2)
    (text doubled /= text composition) `shouldEqual` true
  it "Pacific's Premises compose back to the reference world, value for value" do
    let source = pacific defaultDials
    map _.id source.premises `shouldEqual` [ "us-regime", "china-regime", "international-clearing" ]
    case composedOf { id: source.id, version: source.version, premises: source.premises } of
      Left e -> fail e
      Right c -> (encodeWith Authored compositionShape c == encodeWith Authored compositionShape composition) `shouldEqual` true
  where
  text c = stringify (encodeWith Authored compositionShape c)
