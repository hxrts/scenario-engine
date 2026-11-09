-- | OpenAPI documents for the two independently built services (docs/06_simulator.md §8), generated from the
-- | route table's paths and methods and from the contract's Shapes and refusals of each request and reply, so a document cannot
-- | describe a route differently from how it behaves.
module Server.OpenApi
  ( services
  , serviceName
  , serviceOf
  , document
  , defaultServer
  , documentsOf
  , published
  ) where

import Prelude

import Data.Array (any, nub, sort, concatMap, filter, find, mapMaybe, nubByEq)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust, maybe)
import Laws.Types (RefKind)
import Laws.World (namesOfKind)
import Server.Compositions (Compositions)

import Core.Shape (Desc, allValues, jsonSchemaIn, schemaComponentsIn)
import Data.Argonaut.Core (Json, fromArray, fromBoolean, fromObject, fromString)
import Data.String (Pattern(..), joinWith, stripPrefix, toLower)
import Data.Tuple (Tuple(..), fst)
import Foreign.Object as Object
import Handlers.Api.Contract (operations)
import Handlers.Api.Route (Service(..), Doc, endpointAccess, errorShape)

services :: Array Service
services = [ Transcription, Narrative ]

serviceName :: Service -> String
serviceName = case _ of
  Transcription -> "transcription"
  Narrative -> "narrative"

serviceOf :: String -> Maybe Service
serviceOf name = find (\s -> serviceName s <> ".json" == toLower name) services

type Published = { service :: Service, operation :: String, summary :: String, reply :: Desc, status :: Int, errors :: Array Int }

-- | Every operation the route table publishes, with its entry of the contract.
published :: Array Published
published = mapMaybe entry endpointAccess
  where
  entry e = do
    p <- e.publish
    c <- find (\o -> o.name == e.name) operations
    -- what reading the request can refuse is the route's own
    pure { service: p.service, operation: e.name, summary: p.summary, reply: c.reply, status: c.status, errors: sort (nub (c.refusals <> requestErrors e.doc)) }
  requestErrors doc = (if isJust doc.body then [ 400 ] else []) <> (if any _.integer doc.params then [ 404 ] else [])

-- | Where a document says the service is served, unless it was written for another server.
defaultServer :: String
defaultServer = "https://scenario.hxrts.com"

-- | The OpenAPI 3.1 document of one service, naming no Composition's ids: what `/api/openapi/*.json` serves.
document :: Service -> Json
document = documentIn { server: defaultServer, names: const [] }

-- | Each service's document for one Composition and server: a reference of a kind the Composition's World names lists those
-- | names. An unknown Composition is refused with the known ones.
documentsOf :: Compositions -> { composition :: String, server :: String } -> Either String (Array (Tuple Service Json))
documentsOf compositions { composition, server } = case find (\p -> p.composition.id == composition) compositions of
  Nothing -> Left ("no composition named " <> composition <> "; known compositions: " <> joinWith ", " (map _.composition.id compositions))
  Just parts -> Right (map (\s -> Tuple s (documentIn { server, names: namesIn parts.composition.world } s)) services)
  where
  namesIn world kind = maybe [] (namesOfKind world) (find (\k -> show k == kind) (allValues :: Array RefKind))

type Where = { server :: String, names :: String -> Array String }

documentIn :: Where -> Service -> Json
documentIn at service = obj
  [ Tuple "openapi" (fromString "3.1.0")
  , Tuple "info" (obj [ Tuple "title" (fromString ("Scenario Engine " <> serviceName service <> " API")), Tuple "version" (fromString "1") ])
  , Tuple "servers" (fromArray [ obj [ Tuple "url" (fromString at.server) ] ])
  , Tuple "paths" (obj (mapMaybe (pathOf at) ops))
  , Tuple "components" (obj [ Tuple "schemas" (obj components) ])
  ]
  where
  ops = filter (\p -> p.service == service) published
  components = nubByEq (\a b -> fst a == fst b) (concatMap (\p -> schemaComponentsIn at.names p.reply <> maybe [] (schemaComponentsIn at.names) (requestOf p)) ops)

obj :: Array (Tuple String Json) -> Json
obj = fromObject <<< Object.fromFoldable

requestOf :: Published -> Maybe Desc
requestOf p = find (\e -> e.name == p.operation) endpointAccess >>= _.doc.body

pathOf :: Where -> Published -> Maybe (Tuple String Json)
pathOf at p = find (\e -> e.name == p.operation) endpointAccess <#> pathFrom at p

pathFrom :: forall r. Where -> Published -> { method :: String, pattern :: Array String, doc :: Doc | r } -> Tuple String Json
pathFrom at p { method, pattern, doc } = Tuple path (obj [ Tuple (toLower method) operation ])
  where
  path = "/" <> joinWith "/" (map brace pattern)
  brace s = maybe s (\n -> "{" <> n <> "}") (stripPrefix (Pattern ":") s)
  parameters = map parameter doc.params
  parameter { name: n, integer } = obj
    [ Tuple "name" (fromString n)
    , Tuple "in" (fromString "path")
    , Tuple "required" (fromBoolean true)
    , Tuple "schema" (obj [ Tuple "type" (fromString (if integer then "integer" else "string")) ])
    ]
  body = maybe [] (\d -> [ Tuple "requestBody" (obj [ Tuple "required" (fromBoolean true), Tuple "content" (json d) ]) ]) doc.body
  json d = obj [ Tuple "application/json" (obj [ Tuple "schema" (jsonSchemaIn at.names d) ]) ]
  errors = map (\c -> Tuple c (obj [ Tuple "description" (fromString "An error"), Tuple "content" (json errorShape.desc) ])) (map show p.errors)
  operation = obj
    ( [ Tuple "operationId" (fromString p.operation)
      , Tuple "summary" (fromString p.summary)
      , Tuple "parameters" (fromArray parameters)
      , Tuple "responses" (obj ([ Tuple (show p.status) (obj [ Tuple "description" (fromString "OK"), Tuple "content" (json p.reply) ]) ] <> errors))
      ] <> body
    )
