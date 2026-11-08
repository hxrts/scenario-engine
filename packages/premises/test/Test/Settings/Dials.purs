-- | Dials and Premises: the default dials are the reference numbers, a dial changes what it names, and
-- | two Premises may not define one key.
module Test.Settings.Dials (spec) where

import Prelude

import Core.Num as Q
import Core.Shape (Format(..), encodeWith)
import Core.Types (Id(..))
import Data.Argonaut.Core (stringify)
import Data.Array (find)
import Data.Either (Either(..), isLeft)
import Data.Foldable (fold)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Settings.Kit (assemble, emptyParts, premise)
import Settings.Meridian (defaultDials, gameboard, meridian, premises)
import Settings.Meridian.Names (n)
import Schema.Build.World (noResource, resourcesOf)
import Data.Tuple (Tuple(..))
import Schema.Setting.Gameboard (gameboardShape)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = describe "Dials and Premises" do
  it "the default dials reproduce the reference Gameboard" do
    let text g = stringify (encodeWith Authored gameboardShape g)
    text (meridian defaultDials).gameboard `shouldEqual` text gameboard
  it "a dial changes what it names" do
    let
      doubled = meridian defaultDials { flow = \f -> f { amount = Q.add f.amount f.amount } }
      firstFlow = find (\f -> f.from == Id n.usWorkers && f.to == Id n.us)
    map _.amount (firstFlow doubled.gameboard.authored.circulation.flows) `shouldEqual` Just (Q.fromInt 30)
  it "Meridian's Premises combine; a key two Premises define fails" do
    case assemble (fold premises) of
      Left e -> fail e
      Right p -> Map.member (Id n.mil) p.resources `shouldEqual` true
    let gold = premise emptyParts { resources = resourcesOf [ Tuple "gold" noResource ] }
    isLeft (assemble (gold <> gold)) `shouldEqual` true
