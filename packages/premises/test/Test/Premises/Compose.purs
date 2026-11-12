-- | Composing Premises (docs/03 §12a), with literal expectations, and filtering a composition.
module Test.Premises.Compose (spec) where

import Testkit.Meridian (orThrow)
import Prelude hiding (compose)

import World.Error (ValidationError(..))
import Core.Shape (Format(..), encodeWith)
import Laws.Types (Id(..), RefKind(..), idText)
import Data.Argonaut.Core (Json, stringify, toArray, toObject, toString)
import Data.Array (elem, filter, length, mapMaybe)
import Data.Either (Either(..))
import Data.Foldable (any)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Assembly.Error (AssemblyError(..))
import Assembly.Patch (engineOnly)
import Assembly.Restrict (everything, restrict)
import Premises.Load (composedOf, loadFrom, parts)
import Premises.Compose (ComposeError(..), CompositionSource, Premise, compose, files, renderComposeError)
import Premises.Tree as Tree
import Premises.Kit.Presets (presetsOf, standardThresholds)
import Core.Draw (Seed(..))
import Core.Num as Q
import Data.Argonaut.Parser (jsonParser)
import Data.Identity (Identity(..))
import Data.Newtype (unwrap)
import Partial.Unsafe (unsafeCrashWith)
import Premises.Meridian (defaultDials, composition, meridian)
import Premises.Meridian.Names (n)
import Premises.Pacific as Pacific
import Schema.Premise.Composition (configShape, compositionShape)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

json :: String -> Json
json text = case jsonParser text of
  Right j -> j
  Left e -> unsafeCrashWith e

premise :: String -> String -> Premise
premise id text =
  { id
  , version: "v0"
  , sections: case toObject (json text) of
      Just o -> o
      Nothing -> unsafeCrashWith "a section object"
  }

-- A small Premise, written for no other.
corn :: Premise
corn = premise "corn"
  """{
  "numeraire": "G",
  "horizon": 3,
  "roundRange": [ 2, 4 ],
  "resources": { "G": { "transferability": "Free" }, "C": { "transferability": "Free" } },
  "exchanges": { "corn": { "base": "C", "quote": "G", "cost": { "intercept": "2", "slope": "0.01", "lo": "-50", "hi": "50" }, "reserves": { "G": "500", "C": "200" }, "access": [ "A", "B" ] } },
  "schemes": [],
  "actors": { "A": { "accounts": { "G": "100", "C": "10" }, "capabilities": [ "transfer:G" ] }, "B": { "accounts": { "G": "80", "C": "20" }, "capabilities": [] } }
}"""

composeOf :: Array Premise -> Either ComposeError (Object.Object Json)
composeOf premises = compose { id: "t", version: "v0", premises }

-- The Resource names a composition holds, as far as composing goes.
composedKeys :: Array Premise -> Either ComposeError (Array String)
composedKeys premises = composeOf premises <#> \m -> fromMaybe [] (Object.keys <$> (toObject =<< Object.lookup "resources" m))

composedActor :: String -> Array Premise -> Either ComposeError { accounts :: Array (Tuple String String), capabilities :: Int }
composedActor name premises = composeOf premises <#> \m ->
  let
    actor = fromMaybe Object.empty (toObject =<< Object.lookup name =<< toObject =<< Object.lookup "actors" m)
    accounts = fromMaybe Object.empty (toObject =<< Object.lookup "accounts" actor)
    qty j = fromMaybe "" (toString j)
  in
    { accounts: map (map qty) (Object.toAscUnfoldable accounts), capabilities: maybe 0 length (toArray =<< Object.lookup "capabilities" actor) }

flowsFrom :: Object.Object Json -> Array String
flowsFrom m = maybe [] (mapMaybe (\f -> toObject f >>= Object.lookup "from" >>= toString)) (toArray =<< Object.lookup "flows" =<< toObject =<< Object.lookup "circulation" m)

source :: Array Premise -> CompositionSource
source premises =
  { id: "t"
  , version: "v0"
  , premises
  , config: { seed: Seed 1, rounds: 3, seats: Map.empty, nonPlayableActors: Map.empty, fallbacks: Map.empty, representations: [], preset: Nothing }
  , presets: presetsOf { staple: Id "G", money: Id "G", build: Nothing, invest: Nothing, deploy: Nothing, thresholds: standardThresholds }
  }

-- File operations over compiled files in memory.
memoryOps :: Array (Tuple String Json) -> Tree.FileOps Identity
memoryOps compiled =
  { env: \_ -> Identity Nothing
  , list: \_ -> Identity (Right [])
  , read: \path -> Identity (maybe (Left { path, reason: "missing" }) (Right <<< stringify) (Object.lookup path byPath))
  }
  where
  byPath = Object.fromFoldable (map (\(Tuple p j) -> Tuple ("packages/premises/" <> p) j) compiled)

spec :: Spec Unit
spec = do
  describe "compose" do
    it "two Premises defining one Resource differently fail, naming both and the Resource" do
      composedKeys [ premise "a" """{ "resources": { "G": { "transferability": "Free" } } }""", premise "b" """{ "resources": { "G": { "transferability": "NonTransferable" } } }""" ]
        `shouldEqual` Left (Conflict { kind: "Resource", name: "G", first: "a@v0", second: "b@v0" })
      renderComposeError (Conflict { kind: "Resource", name: "G", first: "a@v0", second: "b@v0" }) `shouldEqual` "Premises a@v0 and b@v0 define the Resource G differently"

    it "identical definitions in two Premises are one definition" do
      composedKeys [ premise "a" """{ "resources": { "G": { "transferability": "Free" } } }""", premise "b" """{ "resources": { "G": { "transferability": "Free" } } }""" ]
        `shouldEqual` Right [ "G" ]

    it "a single value two Premises give differently fails; given alike it is one value" do
      composedKeys [ premise "a" """{ "numeraire": "G" }""", premise "b" """{ "numeraire": "M" }""" ]
        `shouldEqual` Left (Conflict { kind: "numeraire", name: "of the composition", first: "a@v0", second: "b@v0" })
      composedKeys [ premise "a" """{ "numeraire": "G" }""", premise "b" """{ "numeraire": "G" }""" ]
        `shouldEqual` Right []

    it "a list item two Premises both write is one item; later items follow earlier ones" do
      let flow from = "{ \"from\": \"" <> from <> "\", \"to\": \"T\", \"resource\": \"G\", \"amount\": \"1\" }"
      case composeOf [ premise "a" ("{ \"circulation\": { \"flows\": [ " <> flow "A" <> ", " <> flow "B" <> " ] } }"), premise "b" ("{ \"circulation\": { \"flows\": [ " <> flow "B" <> ", " <> flow "C" <> " ] } }") ] of
        Left e -> fail (show e)
        Right merged -> flowsFrom merged `shouldEqual` [ "A", "B", "C" ]

    it "a section no composition has is refused, naming the Premise" do
      composedKeys [ premise "a" """{ "colours": {} }""" ] `shouldEqual` Left (UnknownSection { premise: "a@v0", key: "colours" })

    it "extending a name no Premise defines fails, naming the Premise" do
      composedKeys [ premise "a" """{ "extends": { "actors": { "NOBODY": { "accounts": { "G": "1" } } } } }""" ]
        `shouldEqual` Left (UnknownExtension { premise: "a@v0", kind: "Actor", name: "NOBODY" })
      composedKeys [ premise "a" """{ "extends": { "exchanges": { "nowhere": { "access": [ "A" ] } } } }""" ]
        `shouldEqual` Left (UnknownExtension { premise: "a@v0", kind: "Exchange", name: "nowhere" })

    it "extending an Actor adds holdings and Capabilities; a holding given differently fails, naming both" do
      let definer = premise "a" """{ "actors": { "A": { "accounts": { "G": "100" }, "capabilities": [] } } }"""
      composedActor "A" [ definer, premise "b" """{ "extends": { "actors": { "A": { "accounts": { "G": "100", "E": "5" }, "capabilities": [ { "mayTransfer": "E" } ] } } } }""" ]
        `shouldEqual` Right { accounts: [ Tuple "E" "5", Tuple "G" "100" ], capabilities: 1 }
      composedActor "A" [ definer, premise "b" """{ "extends": { "actors": { "A": { "accounts": { "G": "7" } } } } }""" ]
        `shouldEqual` Left (Conflict { kind: "holding at A of", name: "G", first: "a@v0", second: "b@v0" })

    it "a name no Premise in the composition defines is an error of the composed result" do
      map (const unit) (parts (source [ corn, premise "stray" """{ "resources": { "E": { "transferability": "Free" } }, "circulation": { "flows": [ { "from": "A", "to": "Z", "resource": "E", "amount": "1" } ] } }""" ]))
        `shouldEqual` Left (show (Invalid [ UnknownReference RefActor "Z" ]))
      map (const unit) (parts (source [ premise "alone" """{ "numeraire": "G", "horizon": 1, "roundRange": [ 1, 3 ], "schemes": [], "actors": {}, "exchanges": { "corn": { "base": "C", "quote": "G", "cost": { "intercept": "1", "slope": "0", "lo": "-1", "hi": "1" }, "reserves": {}, "access": [] } }, "resources": { "G": { "transferability": "Free" } } }""" ]))
        `shouldEqual` Left (show (Invalid [ UnknownReference RefResource "C" ]))

    it "two Premises not written for each other compose to one economy" do
      let energy = premise "energy" """{ "resources": { "E": { "transferability": "Free" } }, "extends": { "actors": { "A": { "accounts": { "E": "12" } } } }, "circulation": { "flows": [ { "from": "A", "to": "B", "resource": "E", "amount": "3" } ] } }"""
      case composedOf { id: "mixed", version: "v1", premises: [ corn, energy ] } of
        Left e -> fail e
        Right c -> do
          c.premises `shouldEqual` [ { id: "corn", version: "v0" }, { id: "energy", version: "v0" } ]
          c.id `shouldEqual` "mixed"
          map idText (Map.keys c.world.resources # Set.toUnfoldable) `shouldEqual` [ "C", "E", "G" ]
          let holding r = Q.format <$> (Map.lookup (Id "A") c.world.actors >>= Map.lookup (Id r) <<< _.accounts)
          holding "C" `shouldEqual` Just "10.0000"
          holding "E" `shouldEqual` Just "12.0000"
          holding "G" `shouldEqual` Just "100.0000"
          map (\f -> Tuple (idText f.from) (Q.format f.amount)) c.authored.circulation.flows `shouldEqual` [ Tuple "A" "3.0000" ]

    it "files on disk load and compose as they compile; a conflict on disk names both Premises" do
      let
        run = unwrap <<< loadFrom (memoryOps (files (meridian defaultDials)))
        bad = (meridian defaultDials) { premises = [ corn, corn { id = "corn2", sections = Object.insert "numeraire" (json "\"C\"") corn.sections } ] }
      case run "meridian/v0" of
        Left e -> fail e
        Right loaded -> (encodeWith Authored compositionShape loaded.composition == encodeWith Authored compositionShape composition) `shouldEqual` true
      case unwrap (loadFrom (memoryOps (files bad)) "meridian/v0") of
        Left e -> e `shouldEqual` "composition meridian: Premises corn@v0 and corn2@v0 define the numeraire of the composition differently"
        Right _ -> fail "a conflict must fail the load"

  describe "Pacific (docs/05 §8)" do
    let pacific = Pacific.pacific Pacific.defaultDials
    it "its two Premises compose with no conflict and check" do
      map (const unit) (parts pacific) `shouldEqual` Right unit
    it "bilateral-economy alone is a whole economy of states and firms, with no Rules and only the firms' investments" do
      case parts pacific { premises = filter (\x -> x.id == "bilateral-economy") pacific.premises, presets = [], config = pacific.config { preset = Nothing } } of
        Left e -> fail e
        Right p -> do
          Map.size p.composition.world.transformations `shouldEqual` 4
          Map.size p.composition.world.ruleKinds `shouldEqual` 0
    it "private-sector alone fails only on what the other Premises define" do
      case composedOf { id: pacific.id, version: pacific.version, premises: filter (\x -> x.id == "private-sector") pacific.premises } of
        Left e -> e `shouldEqual` "composition pacific: Premise private-sector@v0 extends the Actor CN, which no Premise in the composition defines"
        Right _ -> fail "the private-sector Premise extends Actors it does not define"
    it "strategic-dependence alone fails only on the Actors the other Premise defines" do
      case composedOf { id: pacific.id, version: pacific.version, premises: filter (\x -> x.id == "strategic-dependence") pacific.premises } of
        Left e -> e `shouldEqual` "composition pacific: Premise strategic-dependence@v0 extends the Actor CN, which no Premise in the composition defines"
        Right _ -> fail "the strategic Premise extends Actors it does not define"
    it "strategic-dependence adds each country's ban and investment by extension" do
      case parts pacific of
        Left e -> fail e
        Right p -> map _.capabilities (Map.lookup (Id "US") p.composition.world.actors) `shouldEqual` map _.capabilities (Map.lookup (Id "US") Pacific.composition.world.actors)
    it "a dial changes only its number: China's lead time 3 is a delay of 3" do
      let d = Pacific.compositionWith Pacific.defaultDials { cnInvestment = Pacific.defaultDials.cnInvestment { delay = 3 } }
      map _.delay (Map.lookup (Id "cn-technology-investment") d.world.transformations) `shouldEqual` Just 3
      map _.delay (Map.lookup (Id "us-industrial-investment") d.world.transformations) `shouldEqual` Just 1

  describe "restrict" do
    it "keeping everything is the identity" do
      withPremise \p -> do
        r <- orThrow (restrict (everything p.composition) p)
        stringify (encodeWith Authored compositionShape r.composition) `shouldEqual` stringify (encodeWith Authored compositionShape p.composition)
        stringify (encodeWith Authored configShape r.config) `shouldEqual` stringify (encodeWith Authored configShape p.config)
        Object.keys r.presets `shouldEqual` Object.keys p.presets

    it "a dropped Actor takes what belongs to it, and the rest still checks" do
      withPremise \p ->
        let
          dropped = n.ng
          s = (everything p.composition) { actors = filter (_ /= dropped) (everything p.composition).actors }
        in
          do
            r <- orThrow (restrict s p)
            Map.member dropped r.composition.world.actors `shouldEqual` false
            Map.member dropped r.composition.explicitActors `shouldEqual` false
            Map.member dropped r.config.seats `shouldEqual` false
            any (\m -> dropped `elem` m.access) r.composition.world.exchanges `shouldEqual` false
            any (\f -> f.to == dropped || f.from == dropped) r.composition.authored.circulation.flows `shouldEqual` false

    it "accounts can be narrowed for one Actor" do
      withPremise \p ->
        let
          s = (everything p.composition) { accounts = Map.singleton n.us [ n.money ] }
        in
          do
            r <- orThrow (restrict s p)
            map (Map.keys <<< _.accounts) (Map.lookup n.us r.composition.world.actors) `shouldEqual` Just (Map.keys (Map.singleton n.money unit))

    it "a kept thing that refers to a dropped one is an error" do
      withPremise \p ->
        let
          s = (everything p.composition) { resources = filter (_ /= n.energy) (everything p.composition).resources }
        in
          case restrict s p of
            Left (Invalid es) -> (UnknownReference RefResource (idText n.energy) `elem` es) `shouldEqual` true
            Left e -> fail (show e)
            Right _ -> fail "an Exchange whose base Resource was dropped should not pass"

    it "an Aggregate's member cannot be dropped from under it" do
      withPremise \p ->
        let
          s = (everything p.composition) { actors = filter (_ /= n.aid) (everything p.composition).actors }
        in
          case restrict s p of
            Left (Invalid es) -> (UnknownReference RefActor (idText n.aid) `elem` es) `shouldEqual` true
            Left e -> fail (show e)
            Right _ -> fail "a kept Aggregate's dropped member should not pass"

    it "a flow gate goes with the Transformation that carries it" do
      withPremise \p ->
        let
          s = (everything p.composition) { transformations = filter (_ /= n.energyShipping) (everything p.composition).transformations }
        in
          do
            r <- orThrow (restrict s (engineOnly p))
            r.composition.world.flowGates `shouldEqual` []

    it "a name the premise does not have is an error" do
      withPremise \p ->
        let
          s = (everything p.composition) { actors = [ Id "NOBODY" ] }
        in
          case restrict s p of
            Left (Invalid es) -> es `shouldEqual` [ UnknownReference RefActor "NOBODY" ]
            Left e -> fail (show e)
            Right _ -> fail "an unknown Actor should not pass"
  where
  withPremise k = case parts (meridian defaultDials) of
    Left e -> fail e
    Right p -> k p
