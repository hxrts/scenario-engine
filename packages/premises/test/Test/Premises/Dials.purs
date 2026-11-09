-- | Dials and PremiseModules: the default dials are the reference numbers, a dial changes what it names, and
-- | two PremiseModules may not define one key.
module Test.Premises.Dials (spec) where

import Prelude

import Core.Num as Q
import Core.Shape (Format(..), encodeWith)
import Data.Argonaut.Core (stringify)
import Data.Array (find)
import Data.Either (Either(..), isLeft)
import Data.Foldable (fold)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Premises.Kit.Compose (assemble, emptyParts, premiseModule)
import Premises.Meridian (defaultDials, possibleWorld, meridian, premiseModules)
import Premises.Meridian.Names (n)
import Schema.Build.World (noResource, resourcesOf)
import Data.Tuple (Tuple(..))
import Schema.Premise.PossibleWorld (possibleWorldShape)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = describe "Dials and PremiseModules" do
  it "the default dials reproduce the reference PossibleWorld" do
    let text g = stringify (encodeWith Authored possibleWorldShape g)
    text (meridian defaultDials).possibleWorld `shouldEqual` text possibleWorld
  it "a dial changes what it names" do
    let
      doubled = meridian defaultDials { flow = \f -> f { amount = Q.add f.amount f.amount } }
      firstFlow = find (\f -> f.from == n.usWorkers && f.to == n.us)
    map _.amount (firstFlow doubled.possibleWorld.authored.circulation.flows) `shouldEqual` Just (Q.fromInt 30)
  it "Meridian's PremiseModules combine; a key two PremiseModules define fails" do
    case assemble (fold premiseModules) of
      Left e -> fail e
      Right p -> Map.member n.mil p.resources `shouldEqual` true
    let gold = premiseModule emptyParts { resources = resourcesOf [ Tuple "gold" noResource ] }
    isLeft (assemble (gold <> gold)) `shouldEqual` true
