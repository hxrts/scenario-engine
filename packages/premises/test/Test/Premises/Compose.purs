-- | Composing Premises (docs/03 §12a), with literal expectations, and filtering a composition.
module Test.Premises.Compose (spec) where

import Testkit.Run (orThrow)
import Prelude hiding (compose)

import World.Error (ValidationError(..))
import Core.Shape (Format(..), encodeWith)
import Laws.Types (Id(..), RefKind(..), idText)
import Data.Argonaut.Core (Json, stringify, toArray, toObject, toString)
import Data.Array (elem, filter, find, length, mapMaybe)
import Data.Either (Either(..), isLeft)

import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Assembly.Error (AssemblyError(..))

import Assembly.Restrict (everything, restrict)
import Premises.Load (composedOf, loadFrom, parts)
import Premises.Compose (ComposeError(..), CompositionSource, Premise, compose, files, renderComposeError)
import Premises.Tree as Tree
import Core.Draw (Seed(..))
import Core.Num as Q
import Data.Argonaut.Parser (jsonParser)
import Data.Identity (Identity(..))
import Data.Newtype (unwrap)
import Partial.Unsafe (unsafeCrashWith)
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
  , presets: []
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
        run = unwrap <<< loadFrom (memoryOps (files (Pacific.pacific Pacific.defaultDials)))
        bad = (Pacific.pacific Pacific.defaultDials) { premises = [ corn, corn { id = "corn2", sections = Object.insert "numeraire" (json "\"C\"") corn.sections } ] }
      case run "pacific/v0" of
        Left e -> fail e
        Right loaded -> (encodeWith Authored compositionShape loaded.composition == encodeWith Authored compositionShape Pacific.composition) `shouldEqual` true
      case unwrap (loadFrom (memoryOps (files bad)) "pacific/v0") of
        Left e -> e `shouldEqual` "composition pacific: Premises corn@v0 and corn2@v0 define the numeraire of the composition differently"
        Right _ -> fail "a conflict must fail the load"

  describe "Pacific (docs/05 §8)" do
    let pacific = Pacific.pacific Pacific.defaultDials
    let
      only ids = pacific.premises # filter (\x -> x.id `elem` ids)
      sectionOf id key = (Object.lookup key <<< _.sections) =<< (find (\x -> x.id == id) pacific.premises)
      sizeOf id key = maybe 0 (\j -> maybe (maybe 0 length (toArray j)) (\o -> length (Object.keys o)) (toObject j)) (sectionOf id key)
    it "its three Premises, one per economic object, compose with no conflict and check" do
      map _.id pacific.premises `shouldEqual` [ "us-regime", "china-regime", "international-clearing" ]
      map (const unit) (parts pacific) `shouldEqual` Right unit
    it "both regimes are one builder's: their Premises have the same sections, with as many of each kind" do
      let kinds = [ "actors", "transformations", "laws", "templates", "strategies", "parameters" ]
      map (sizeOf "us-regime") kinds `shouldEqual` [ 3, 6, 8, 4, 3, 6 ]
      map (sizeOf "china-regime") kinds `shouldEqual` [ 3, 6, 8, 12, 3, 6 ]
    it "the clearing system holds every Exchange, every kind of Rule, the Rules in force and world savings; no regime does" do
      map (sizeOf "international-clearing") [ "exchanges", "ruleKinds", "rules", "groups", "actors" ] `shouldEqual` [ 5, 10, 1, 4, 1 ]
      map (\id -> map (sizeOf id) [ "exchanges", "ruleKinds", "rules", "groups" ]) [ "us-regime", "china-regime" ] `shouldEqual` [ [ 0, 0, 0, 0 ], [ 0, 0, 0, 0 ] ]
    it "a regime alone does not load: its Exchanges, Rules and Resources are the clearing system's" do
      isLeft (parts pacific { premises = only [ "us-regime" ], presets = [], config = pacific.config { preset = Nothing } }) `shouldEqual` true
      isLeft (parts pacific { premises = only [ "us-regime", "international-clearing" ], presets = [], config = pacific.config { preset = Nothing } }) `shouldEqual` true
    it "the composed US state holds the capabilities its regime gives it" do
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

    it "a kept thing that refers to a dropped one is an error" do
      withPremise \p ->
        let
          s = (everything p.composition) { transformations = [], resources = filter (_ /= Id "T") (everything p.composition).resources }
        in
          case restrict s p of
            Left (Invalid es) -> (UnknownReference RefResource "T" `elem` es) `shouldEqual` true
            Left e -> fail (show e)
            Right _ -> fail "an Exchange whose base Resource was dropped should not pass"

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
  withPremise k = case parts (Pacific.pacific Pacific.defaultDials) of
    Left e -> fail e
    Right p -> k p
