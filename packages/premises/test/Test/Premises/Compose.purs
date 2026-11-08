-- | Filtering a premise, and the law that a premise split into its section files merges back whole.
module Test.Premises.Compose (spec) where

import Prelude

import Board.Error (ValidationError(..))
import Core.Shape (Format(..), encodeWith)
import Core.Types (Id(..), RefKind(..))
import Data.Argonaut.Core (stringify)
import Data.Array (elem, filter)
import Data.Either (Either(..))
import Data.Foldable (any)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Game.Error (GameError(..))
import Game.Restrict (everything, restrict)
import Premises.Load (fromFiles)
import Premises.Compose as Composition
import Premises.Meridian (gameboard, meridian)
import Premises.Meridian.Names as N
import Schema.Premise.Gameboard (configShape, gameboardShape)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = do
  describe "A premise's section files" do
    it "merge back to the same Gameboard" do
      case fromFiles (map (\(Tuple name json) -> { name, json }) (Composition.files meridian)) of
        Left e -> fail e
        Right d -> stringify (encodeWith Premise gameboardShape d.gameboard) `shouldEqual` stringify (encodeWith Premise gameboardShape gameboard)

  describe "restrict" do
    it "keeping everything is the identity" do
      withPremise \p -> case restrict (everything p.gameboard) p of
        Left e -> fail (show e)
        Right r -> do
          stringify (encodeWith Premise gameboardShape r.gameboard) `shouldEqual` stringify (encodeWith Premise gameboardShape p.gameboard)
          stringify (encodeWith Premise configShape r.config) `shouldEqual` stringify (encodeWith Premise configShape p.config)
          Object.keys r.presets `shouldEqual` Object.keys p.presets

    it "a dropped Entity takes what belongs to it, and the rest still checks" do
      withPremise \p ->
        let
          dropped = Id N.ng
          s = (everything p.gameboard) { entities = filter (_ /= dropped) (everything p.gameboard).entities }
        in
          case restrict s p of
            Left e -> fail (show e)
            Right r -> do
              Map.member dropped r.gameboard.world.entities `shouldEqual` false
              Map.member dropped r.gameboard.actors `shouldEqual` false
              Map.member dropped r.config.seats `shouldEqual` false
              any (\m -> dropped `elem` m.access) r.gameboard.world.markets `shouldEqual` false
              any (\f -> f.to == dropped || f.from == dropped) r.gameboard.authored.circulation.flows `shouldEqual` false

    it "accounts can be narrowed for one Entity" do
      withPremise \p ->
        let
          s = (everything p.gameboard) { accounts = Map.singleton (Id N.us) [ Id N.money ] }
        in
          case restrict s p of
            Left e -> fail (show e)
            Right r -> map (Map.keys <<< _.accounts) (Map.lookup (Id N.us) r.gameboard.world.entities) `shouldEqual` Just (Map.keys (Map.singleton (Id N.money) unit))

    it "a kept thing that refers to a dropped one is an error" do
      withPremise \p ->
        let
          s = (everything p.gameboard) { resources = filter (_ /= Id N.energy) (everything p.gameboard).resources }
        in
          case restrict s p of
            Left (Invalid _ es) -> (UnknownReference ResourceRef N.energy `elem` es) `shouldEqual` true
            Left e -> fail (show e)
            Right _ -> fail "a Market whose base Resource was dropped should not pass"

    it "an Aggregate's member cannot be dropped from under it" do
      withPremise \p ->
        let
          s = (everything p.gameboard) { entities = filter (_ /= Id N.aid) (everything p.gameboard).entities }
        in
          case restrict s p of
            Left (Invalid _ es) -> (UnknownReference EntityRef N.aid `elem` es) `shouldEqual` true
            Left e -> fail (show e)
            Right _ -> fail "a kept Aggregate's dropped member should not pass"

    it "a name the premise does not have is an error" do
      withPremise \p ->
        let
          s = (everything p.gameboard) { entities = [ Id "NOBODY" ] }
        in
          case restrict s p of
            Left (Invalid _ es) -> es `shouldEqual` [ UnknownReference EntityRef "NOBODY" ]
            Left e -> fail (show e)
            Right _ -> fail "an unknown Entity should not pass"
  where
  withPremise k = case Composition.parts meridian of
    Left e -> fail e
    Right p -> k p
