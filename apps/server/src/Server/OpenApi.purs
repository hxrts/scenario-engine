-- | OpenAPI documents for the two independently built services (docs/06_simulator.md §8), generated from the
-- | route table's paths and methods and from the Shapes of each request and reply, so a document cannot
-- | describe a route differently from how it behaves.
module Server.OpenApi
  ( Service(..)
  , services
  , serviceName
  , serviceOf
  , document
  , published
  ) where

import Prelude

import Core.Shape (Desc, jsonSchema, schemaComponents)
import Data.Argonaut.Core (Json, fromArray, fromBoolean, fromObject, fromString)
import Data.Array (concatMap, filter, find, mapMaybe, nubByEq)
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..), joinWith, stripPrefix, toLower)
import Data.Tuple (Tuple(..), fst)
import Foreign.Object as Object
import Handlers.Api.Route (endpointAccess, errorShape, submissionShape)
import Views.Keys as Keys

-- | The services built outside this repository.
data Service = Transcription | Narrative

derive instance Eq Service

services :: Array Service
services = [ Transcription, Narrative ]

serviceName :: Service -> String
serviceName = case _ of
  Transcription -> "transcription"
  Narrative -> "narrative"

serviceOf :: String -> Maybe Service
serviceOf name = find (\s -> serviceName s <> ".json" == toLower name) services

type Published = { service :: Service, operation :: String, summary :: String, request :: Maybe Desc, reply :: Desc }

-- | Each published operation, by its constructor in `ApiRequest`.
published :: Array Published
published =
  [ { service: Transcription, operation: "GetSession", summary: "The round a session is on, and which seats have spoken.", request: Nothing, reply: Keys.sessionState.desc }
  , { service: Transcription, operation: "SubmitTranscript", summary: "Submit one seat's text for the round that is open.", request: Just submissionShape.desc, reply: Keys.sessionState.desc }
  , { service: Narrative, operation: "RoundReport", summary: "What one round produced, to write a report from.", request: Nothing, reply: Keys.roundReport.desc }
  ]

-- | The OpenAPI 3.1 document of one service.
document :: Service -> Json
document service = obj
  [ Tuple "openapi" (fromString "3.1.0")
  , Tuple "info" (obj [ Tuple "title" (fromString ("Scenario Engine " <> serviceName service <> " API")), Tuple "version" (fromString "1") ])
  , Tuple "servers" (fromArray [ obj [ Tuple "url" (fromString "https://scenario.hxrts.com") ] ])
  , Tuple "paths" (obj (mapMaybe pathOf ops))
  , Tuple "components" (obj [ Tuple "schemas" (obj components) ])
  ]
  where
  ops = filter (\p -> p.service == service) published
  components = nubByEq (\a b -> fst a == fst b) (concatMap (\p -> schemaComponents p.reply <> maybe [] schemaComponents p.request) ops)

obj :: Array (Tuple String Json) -> Json
obj = fromObject <<< Object.fromFoldable

pathOf :: Published -> Maybe (Tuple String Json)
pathOf p = find (\e -> e.name == p.operation) endpointAccess <#> pathFrom p

pathFrom :: forall r. Published -> { method :: String, pattern :: Array String | r } -> Tuple String Json
pathFrom p { method, pattern } = Tuple path (obj [ Tuple (toLower method) operation ])
  where
  path = "/" <> joinWith "/" (map brace pattern)
  brace s = maybe s (\n -> "{" <> n <> "}") (stripPrefix (Pattern ":") s)
  parameters = mapMaybe (\s -> stripPrefix (Pattern ":") s <#> parameter) pattern
  parameter n = obj
    [ Tuple "name" (fromString n)
    , Tuple "in" (fromString "path")
    , Tuple "required" (fromBoolean true)
    , Tuple "schema" (obj [ Tuple "type" (fromString (if n == "round" then "integer" else "string")) ])
    ]
  body = maybe [] (\d -> [ Tuple "requestBody" (obj [ Tuple "required" (fromBoolean true), Tuple "content" (json d) ]) ]) p.request
  json d = obj [ Tuple "application/json" (obj [ Tuple "schema" (jsonSchema d) ]) ]
  errors = map (\c -> Tuple c (obj [ Tuple "description" (fromString "An error"), Tuple "content" (json errorShape.desc) ])) [ "400", "404", "409" ]
  operation = obj
    ( [ Tuple "operationId" (fromString p.operation)
      , Tuple "summary" (fromString p.summary)
      , Tuple "parameters" (fromArray parameters)
      , Tuple "responses" (obj ([ Tuple "200" (obj [ Tuple "description" (fromString "OK"), Tuple "content" (json p.reply) ]) ] <> errors))
      ] <> body
    )
