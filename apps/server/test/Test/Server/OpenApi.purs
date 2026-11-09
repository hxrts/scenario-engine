-- | Pins: the JSON Schema interpreter of `Shape`, and the OpenAPI documents of the two external services
-- | (docs/06_simulator.md §8). Literal schemas are written by hand; live replies are checked against the
-- | published documents by a small validator of the keywords the interpreter emits.
module Test.Server.OpenApi (spec) where

import Prelude

import Handlers.Api.Route (Service(..), submissionShape, endpointAccess, isOpen, route, wire)

import Data.Array (find, elem, filter, all, any, length, sort, zipWith)

import Core.Shape (Format(..), Shape, decodeJsonWith, Desc(..), TagForm(..), jsonSchema, schemaComponents)
import Data.Either (isRight, either)
import Views.Keys (viewShape)
import Handlers.Api.Contract (SessionStarted, SessionState)

import Data.Argonaut.Core (Json, isNull, jsonNull, stringify, toArray, toBoolean, toNumber, toObject, toString)
import Data.Argonaut.Parser (jsonParser)
import Data.Int (fromNumber)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.String (Pattern(..), contains, stripPrefix)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, delay)
import Effect.Class (liftEffect)
import Effects.Api (ApiRequest(..), Reply)
import Foreign.Object as Object
import Server.OpenApi (documentsOf, document, published, serviceName, services)
import Server.Stack (implement, newServerCache)
import Test.Server.Host (testHost)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Testkit.Meridian (anyRounds, premise)
import Testkit.Store (open, storeOver)

parsed :: String -> Json
parsed = either (const jsonNull) identity <<< jsonParser

at :: String -> Json -> Maybe Json
at key j = toObject j >>= Object.lookup key

-- | Whether a value is accepted by a schema, resolving references against the document.
accepts :: Json -> Json -> Json -> Boolean
accepts root schema value = case at "$ref" schema >>= toString of
  Just ref -> maybe false (\s -> accepts root s value) (resolve ref)
  Nothing -> all identity
    [ check "type" (typeOk <=< toString)
    , check "const" (\c -> Just (stringify c == stringify value))
    , check "anyOf" (\c -> any (\s -> accepts root s value) <$> toArray c)
    , check "oneOf" (\c -> (\xs -> length (filterOk xs) == 1) <$> toArray c)
    , check "items" (\s -> all (accepts root s) <$> toArray value)
    , check "prefixItems" (\c -> (\xs ys -> all identity (zipWith (accepts root) xs ys)) <$> toArray c <*> toArray value)
    , check "minItems" (\n -> (\a m -> length a >= m) <$> toArray value <*> (toNumber n >>= fromNumber))
    , check "maxItems" (\n -> (\a m -> length a <= m) <$> toArray value <*> (toNumber n >>= fromNumber))
    , check "required" (\c -> (\ks o -> all (\k -> maybe false (\n -> Object.member n o) (toString k)) ks) <$> toArray c <*> toObject value)
    , check "properties" (\c -> (\ps o -> all (\(Tuple k s) -> maybe true (accepts root s) (Object.lookup k o)) (Object.toUnfoldable ps :: Array _)) <$> toObject c <*> toObject value)
    , check "additionalProperties" (\s -> all (accepts root s) <<< Object.values <$> toObject value)
    ]
  where
  check key f = maybe true (fromMaybe false <<< f) (at key schema)
  filterOk xs = xs # map (\s -> accepts root s value) # (\bs -> bs >>= \b -> if b then [ b ] else [])
  resolve ref = stripPrefix (Pattern "#/components/schemas/") ref >>= \n -> at "components" root >>= at "schemas" >>= at n
  typeOk t = Just case t of
    "object" -> isJust (toObject value)
    "array" -> isJust (toArray value)
    "string" -> isJust (toString value)
    "boolean" -> isJust (toBoolean value)
    "null" -> isNull value
    "integer" -> isJust (toNumber value >>= fromNumber)
    _ -> false

-- | Every reference a schema holds.
refsIn :: Json -> Array String
refsIn j = own <> maybe [] (\o -> Object.values o >>= refsIn) (toObject j) <> maybe [] (\a -> a >>= refsIn) (toArray j)
  where
  own = maybe [] pure (at "$ref" j >>= toString)

-- | The reply schema of an operation in a published document.
replySchema :: Json -> String -> String -> Maybe Json
replySchema doc path method = at "paths" doc >>= at path >>= at method >>= at "responses" >>= at "200" >>= at "content" >>= at "application/json" >>= at "schema"

pathsOf :: Service -> Array String
pathsOf s = sort (maybe [] Object.keys (toObject =<< at "paths" (document s)))

api :: Aff (String -> String -> String -> Aff Reply)
api = do
  ctx <- anyRounds <$> premise
  db <- liftEffect (open ":memory:")
  cache <- liftEffect newServerCache
  let
    store = storeOver db
    host = testHost store
  pure \method path body -> liftEffect (route (implement [ ctx ] store cache host) { method, path, query: Object.empty, body })

waitFor :: (String -> String -> String -> Aff Reply) -> String -> Aff Unit
waitFor call text = go 200
  where
  go :: Int -> Aff Unit
  go 0 = pure unit
  go n = do
    r <- call "GET" "/api/sessions/run-1" ""
    if contains (Pattern text) r.body then pure unit else delay (Milliseconds 25.0) *> go (n - 1)

spec :: Spec Unit
spec = describe "JSON Schema and OpenAPI" do
  it "a record's schema lists its properties and requires what is not optional" do
    stringify (jsonSchema (DRec "R" [ { key: "a", desc: "", shape: DPrim "Int" }, { key: "b", desc: "", shape: DOpt (DPrim "String") } ]))
      `shouldEqual` "{\"title\":\"R\",\"type\":\"object\",\"properties\":{\"a\":{\"type\":\"integer\"},\"b\":{\"anyOf\":[{\"type\":\"string\"},{\"type\":\"null\"}]}},\"required\":[\"a\"]}"
  it "a tagged sum's schema is one of its constructors" do
    let payload = DPrim "String"
    stringify (jsonSchema (DSum "S" Tagged [ { tag: "A", desc: "", payload, form: Tagged } ]))
      `shouldEqual` "{\"title\":\"S\",\"oneOf\":[{\"type\":\"object\",\"properties\":{\"tag\":{\"const\":\"A\"},\"values\":{\"type\":\"array\",\"prefixItems\":[{\"type\":\"string\"}],\"minItems\":1,\"maxItems\":1}},\"required\":[\"tag\",\"values\"]}]}"
  it "a recursive description refers to a named component" do
    let tree = DRec "Tree" [ { key: "kids", desc: "", shape: DArray (DRef "Tree") } ]
    stringify (jsonSchema (DRef "Tree")) `shouldEqual` "{\"$ref\":\"#/components/schemas/Tree\"}"
    map (\(Tuple n _) -> n) (schemaComponents tree) `shouldEqual` [ "Tree" ]
  it "each document names exactly its service's routes, all open, at the paths the route table writes" do
    pathsOf Transcription `shouldEqual` [ "/api/sessions/{id}", "/api/sessions/{id}/transcripts" ]
    pathsOf Narrative `shouldEqual` [ "/api/runs/{id}/rounds/{round}/report" ]
    (wire (GetSession "x")).url `shouldEqual` "/api/sessions/x"
    (wire (RoundReport { id: "x", round: 2 })).url `shouldEqual` "/api/runs/x/rounds/2/report"
  it "the integration contract's example payloads read through their Shapes" do
    let
      reads :: forall a. Shape a -> String -> Boolean
      reads shape text = isRight (decodeJsonWith Snapshot shape (parsed text))
    reads (viewShape :: Shape SessionState) "{\"window\":0,\"submitted\":[],\"status\":\"running\",\"round\":1,\"awaiting\":[\"US\"]}" `shouldEqual` true
    reads (viewShape :: Shape SessionStarted) "{\"id\":\"run-1\",\"balanceSheets\":[{\"actor\":\"US\",\"kind\":\"Playable Actor\",\"path\":\"/balancesheet/run-1/abc123\"}]}" `shouldEqual` true
    isRight (decodeJsonWith Authored submissionShape (parsed "{\"seat\":\"US\",\"round\":1,\"text\":\"We sell ten Energy to Germany.\"}")) `shouldEqual` true
    map _.name (filter (\e -> isOpen e.access && e.name `elem` [ "GetSession", "SubmitTranscript", "RoundReport" ]) endpointAccess) # sort # shouldEqual [ "GetSession", "RoundReport", "SubmitTranscript" ]
    map _.path (filter (\e -> e.name == "StartSession") endpointAccess) `shouldEqual` [ "/api/sessions" ]
  it "each route's documented refusals are the ones it makes, and status lists its values" do
    let
      codes op = maybe [] _.errors (find ((_ == op) <<< _.operation) published)
    codes "GetSession" `shouldEqual` [ 404 ]
    codes "SubmitTranscript" `shouldEqual` [ 400, 404, 409 ]
    codes "RoundReport" `shouldEqual` [ 404, 500 ]
    contains (Pattern "\"enum\":[\"running\",\"completed\",\"failed\"]") (stringify (document Transcription)) `shouldEqual` true
    length published `shouldEqual` length (filter (isJust <<< _.publish) endpointAccess)
  it "an unknown Premise has no documents, and its error names the known ones" do
    ctx <- anyRounds <$> premise
    either identity (const "") (documentsOf [ ctx ] { premise: "nope", server: "x" }) `shouldEqual` "no premise named nope; known premises: meridian"
    (isRight (documentsOf [ ctx ] { premise: "meridian", server: "x" })) `shouldEqual` true
  it "no reference in a document dangles" do
    services # traverse_ \s -> do
      let doc = document s
      refsIn doc # traverse_ \ref -> (ref <> " resolves: " <> show (resolves doc ref)) `shouldEqual` (ref <> " resolves: true")
  it "the documents are served without a session, and live replies validate" do
    call <- api
    services # traverse_ \s -> do
      r <- call "GET" ("/api/openapi/" <> serviceName s <> ".json") ""
      r.status `shouldEqual` 200
      r.body `shouldEqual` stringify (document s)
    (_.status <$> call "GET" "/api/openapi/nothing.json" "") >>= (_ `shouldEqual` 404)
    _ <- call "POST" "/api/sessions" "{\"humans\":[\"US\"],\"rounds\":4,\"playerSeed\":1}"
    waitFor call "\"awaiting\":[\"US\"]"
    state <- call "GET" "/api/sessions/run-1" ""
    let transcription = document Transcription
    (accepts transcription <$> replySchema transcription "/api/sessions/{id}" "get" <*> pure (parsed state.body)) `shouldEqual` Just true
    (accepts transcription <$> replySchema transcription "/api/sessions/{id}" "get" <*> pure (parsed "{\"round\":\"one\"}")) `shouldEqual` Just false
    submitted <- call "POST" "/api/sessions/run-1/transcripts" "{\"seat\":\"US\",\"round\":1,\"text\":\"nothing\"}"
    (accepts transcription <$> replySchema transcription "/api/sessions/{id}/transcripts" "post" <*> pure (parsed submitted.body)) `shouldEqual` Just true
    waitFor call "\"round\":2"
    report <- call "GET" "/api/runs/run-1/rounds/1/report" ""
    report.status `shouldEqual` 200
    let narrative = document Narrative
    (accepts narrative <$> replySchema narrative "/api/runs/{id}/rounds/{round}/report" "get" <*> pure (parsed report.body)) `shouldEqual` Just true
  where
  resolves doc ref = isJust (stripPrefix (Pattern "#/components/schemas/") ref >>= \n -> at "components" doc >>= at "schemas" >>= at n)
