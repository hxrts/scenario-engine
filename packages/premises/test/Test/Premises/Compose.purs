-- | Filtering a premise, and the law that a premise split into its section files merges back whole.
module Test.Premises.Compose (spec) where

import Testkit.Meridian (orThrow)
import Prelude

import World.Error (ValidationError(..))
import Core.Shape (Format(..), encodeWith)
import Laws.Types (Id(..), RefKind(..), idText)
import Data.Argonaut.Core (stringify)
import Data.Array (elem, filter)
import Data.Either (Either(..))
import Data.Foldable (any)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Assembly.Error (GameError(..))
import Assembly.Patch (engineOnly)
import Assembly.Restrict (everything, restrict)
import Premises.Load (fromFiles)
import Premises.Compose as Premise
import Premises.Meridian (defaultDials, possibleWorld, meridian)
import Premises.Meridian.Names (n)
import Schema.Premise.PossibleWorld (configShape, possibleWorldShape)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = do
  describe "A premise's section files" do
    it "merge back to the same PossibleWorld" do
      case fromFiles (map (\(Tuple name json) -> { name, json }) (Premise.files (meridian defaultDials))) of
        Left e -> fail e
        Right d -> stringify (encodeWith Authored possibleWorldShape d.possibleWorld) `shouldEqual` stringify (encodeWith Authored possibleWorldShape possibleWorld)

  describe "restrict" do
    it "keeping everything is the identity" do
      withPremise \p -> do
        r <- orThrow (restrict (everything p.possibleWorld) p)
        stringify (encodeWith Authored possibleWorldShape r.possibleWorld) `shouldEqual` stringify (encodeWith Authored possibleWorldShape p.possibleWorld)
        stringify (encodeWith Authored configShape r.config) `shouldEqual` stringify (encodeWith Authored configShape p.config)
        Object.keys r.presets `shouldEqual` Object.keys p.presets

    it "a dropped Actor takes what belongs to it, and the rest still checks" do
      withPremise \p ->
        let
          dropped = n.ng
          s = (everything p.possibleWorld) { actors = filter (_ /= dropped) (everything p.possibleWorld).actors }
        in
          do
            r <- orThrow (restrict s p)
            Map.member dropped r.possibleWorld.world.actors `shouldEqual` false
            Map.member dropped r.possibleWorld.playableActors `shouldEqual` false
            Map.member dropped r.config.seats `shouldEqual` false
            any (\m -> dropped `elem` m.access) r.possibleWorld.world.exchanges `shouldEqual` false
            any (\f -> f.to == dropped || f.from == dropped) r.possibleWorld.authored.circulation.flows `shouldEqual` false

    it "accounts can be narrowed for one Actor" do
      withPremise \p ->
        let
          s = (everything p.possibleWorld) { accounts = Map.singleton n.us [ n.money ] }
        in
          do
            r <- orThrow (restrict s p)
            map (Map.keys <<< _.accounts) (Map.lookup n.us r.possibleWorld.world.actors) `shouldEqual` Just (Map.keys (Map.singleton n.money unit))

    it "a kept thing that refers to a dropped one is an error" do
      withPremise \p ->
        let
          s = (everything p.possibleWorld) { resources = filter (_ /= n.energy) (everything p.possibleWorld).resources }
        in
          case restrict s p of
            Left (Invalid es) -> (UnknownReference RefResource (idText n.energy) `elem` es) `shouldEqual` true
            Left e -> fail (show e)
            Right _ -> fail "an Exchange whose base Resource was dropped should not pass"

    it "an Aggregate's member cannot be dropped from under it" do
      withPremise \p ->
        let
          s = (everything p.possibleWorld) { actors = filter (_ /= n.aid) (everything p.possibleWorld).actors }
        in
          case restrict s p of
            Left (Invalid es) -> (UnknownReference RefActor (idText n.aid) `elem` es) `shouldEqual` true
            Left e -> fail (show e)
            Right _ -> fail "a kept Aggregate's dropped member should not pass"

    it "a flow gate goes with the Transformation that carries it" do
      withPremise \p ->
        let
          s = (everything p.possibleWorld) { transformations = filter (_ /= n.energyShipping) (everything p.possibleWorld).transformations }
        in
          do
            r <- orThrow (restrict s (engineOnly p))
            r.possibleWorld.world.flowGates `shouldEqual` []

    it "a name the premise does not have is an error" do
      withPremise \p ->
        let
          s = (everything p.possibleWorld) { actors = [ Id "NOBODY" ] }
        in
          case restrict s p of
            Left (Invalid es) -> es `shouldEqual` [ UnknownReference RefActor "NOBODY" ]
            Left e -> fail (show e)
            Right _ -> fail "an unknown Actor should not pass"
  where
  withPremise k = case Premise.parts (meridian defaultDials) of
    Left e -> fail e
    Right p -> k p
