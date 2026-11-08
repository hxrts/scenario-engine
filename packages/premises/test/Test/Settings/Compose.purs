-- | Filtering a setting, and the law that a setting split into its section files merges back whole.
module Test.Settings.Compose (spec) where

import Testkit.Meridian (orThrow)
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
import Game.Patch (engineOnly)
import Game.Restrict (everything, restrict)
import Settings.Load (fromFiles)
import Settings.Compose as Setting
import Settings.Meridian (defaultDials, gameboard, meridian)
import Settings.Meridian.Names (n)
import Schema.Setting.Gameboard (configShape, gameboardShape)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = do
  describe "A setting's section files" do
    it "merge back to the same Gameboard" do
      case fromFiles (map (\(Tuple name json) -> { name, json }) (Setting.files (meridian defaultDials))) of
        Left e -> fail e
        Right d -> stringify (encodeWith Authored gameboardShape d.gameboard) `shouldEqual` stringify (encodeWith Authored gameboardShape gameboard)

  describe "restrict" do
    it "keeping everything is the identity" do
      withSetting \p -> do
        r <- orThrow (restrict (everything p.gameboard) p)
        stringify (encodeWith Authored gameboardShape r.gameboard) `shouldEqual` stringify (encodeWith Authored gameboardShape p.gameboard)
        stringify (encodeWith Authored configShape r.config) `shouldEqual` stringify (encodeWith Authored configShape p.config)
        Object.keys r.presets `shouldEqual` Object.keys p.presets

    it "a dropped Entity takes what belongs to it, and the rest still checks" do
      withSetting \p ->
        let
          dropped = Id n.ng
          s = (everything p.gameboard) { entities = filter (_ /= dropped) (everything p.gameboard).entities }
        in
          do
            r <- orThrow (restrict s p)
            Map.member dropped r.gameboard.world.entities `shouldEqual` false
            Map.member dropped r.gameboard.actors `shouldEqual` false
            Map.member dropped r.config.seats `shouldEqual` false
            any (\m -> dropped `elem` m.access) r.gameboard.world.markets `shouldEqual` false
            any (\f -> f.to == dropped || f.from == dropped) r.gameboard.authored.circulation.flows `shouldEqual` false

    it "accounts can be narrowed for one Entity" do
      withSetting \p ->
        let
          s = (everything p.gameboard) { accounts = Map.singleton (Id n.us) [ Id n.money ] }
        in
          do
            r <- orThrow (restrict s p)
            map (Map.keys <<< _.accounts) (Map.lookup (Id n.us) r.gameboard.world.entities) `shouldEqual` Just (Map.keys (Map.singleton (Id n.money) unit))

    it "a kept thing that refers to a dropped one is an error" do
      withSetting \p ->
        let
          s = (everything p.gameboard) { resources = filter (_ /= Id n.energy) (everything p.gameboard).resources }
        in
          case restrict s p of
            Left (Invalid es) -> (UnknownReference RefResource n.energy `elem` es) `shouldEqual` true
            Left e -> fail (show e)
            Right _ -> fail "a Market whose base Resource was dropped should not pass"

    it "an Aggregate's member cannot be dropped from under it" do
      withSetting \p ->
        let
          s = (everything p.gameboard) { entities = filter (_ /= Id n.aid) (everything p.gameboard).entities }
        in
          case restrict s p of
            Left (Invalid es) -> (UnknownReference RefEntity n.aid `elem` es) `shouldEqual` true
            Left e -> fail (show e)
            Right _ -> fail "a kept Aggregate's dropped member should not pass"

    it "a flow gate goes with the Transformation that carries it" do
      withSetting \p ->
        let
          s = (everything p.gameboard) { transformations = filter (_ /= Id n.energyShipping) (everything p.gameboard).transformations }
        in
          do
            r <- orThrow (restrict s (engineOnly p))
            r.gameboard.world.flowGates `shouldEqual` []

    it "a name the setting does not have is an error" do
      withSetting \p ->
        let
          s = (everything p.gameboard) { entities = [ Id "NOBODY" ] }
        in
          case restrict s p of
            Left (Invalid es) -> es `shouldEqual` [ UnknownReference RefEntity "NOBODY" ]
            Left e -> fail (show e)
            Right _ -> fail "an unknown Entity should not pass"
  where
  withSetting k = case Setting.parts (meridian defaultDials) of
    Left e -> fail e
    Right p -> k p
