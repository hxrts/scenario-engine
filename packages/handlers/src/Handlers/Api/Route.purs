-- | The HTTP route table of the `API` row (docs/06_simulator.md §8, work/4.md §7.4). It is the one
-- | place the paths and methods of the API are written: `parse` turns a request into an `ApiRequest`,
-- | and `wire` turns an `ApiRequest` back into its method, URL, and body, for the client. The status
-- | rules are here too: a path that is not a route, and a body that does not decode, reply with an
-- | error before the operation runs. An operation's own semantics (what it reads, writes, or refuses)
-- | are the implementation's, supplied as `route`'s argument.
module Handlers.Api.Route
  ( Request
  , Implementation
  , Wire
  , apiPrefix
  , route
  , parse
  , wire
  , failWith
  ) where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (catMaybes, filter)
import Data.Either (Either(..), either)
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), joinWith, split)
import Effect (Effect)
import Effects.Api (ApiRequest(..), Launch, Reply)
import Foreign.Object (Object)
import Foreign.Object as Object
import JSURI (encodeURIComponent)
import Schema.Decode.Common (readJson)

-- | The path prefix every operation's URL starts with: the HTTP device hands these requests to the route.
apiPrefix :: String
apiPrefix = "/api/"

-- | A request as the HTTP device delivers it: method, path, decoded query, and body text.
type Request = { method :: String, path :: String, query :: Object String, body :: String }

-- | The operations' semantics, supplied by the composition root: one function over the typed request.
type Implementation = ApiRequest -> Effect Reply

-- | A request as the client sends it: method, URL (with its query), and body text.
type Wire = { method :: String, url :: String, body :: String }

-- | An error reply: the status, and the message in the body's `error` field.
failWith :: Int -> String -> Reply
failWith status error = { status, body: stringify (encodeJson { error }) }

-- | Answer a request: parse it (an error reply when it is not a route or its body does not decode),
-- | then run the operation.
route :: Implementation -> Request -> Effect Reply
route impl req = either pure impl (parse req)

-- | A request as an operation, or the error reply it earns before the operation runs.
parse :: Request -> Either Reply ApiRequest
parse req = case req.method, segments of
  "GET", [ "api", "runs" ] -> Right (ListRuns { archived: param "archived" "" == "true" })
  "GET", [ "api", "scenario" ] -> Right Scenario
  "GET", [ "api", "glossary" ] -> Right Glossary
  "POST", [ "api", "runs" ] -> map LaunchRun (decodeBody req.body :: Either Reply Launch)
  "POST", [ "api", "runs", id, "derive" ] -> Right (Derive { id, body: req.body })
  "POST", [ "api", "runs", id, "resume" ] -> map (\b -> Resume { id, rounds: b.rounds }) (decodeBody req.body :: Either Reply { rounds :: Int })
  "POST", [ "api", "runs", id, "archive" ] -> Right (Archive id)
  "POST", [ "api", "runs", id, "unarchive" ] -> Right (Unarchive id)
  "DELETE", [ "api", "runs", id ] -> Right (Delete { id, confirm: param "confirm" "" })
  "GET", [ "api", "runs", id, "entities" ] -> Right (Entities id)
  "GET", [ "api", "runs", id, "entity", entity ] -> Right (Entity id entity)
  "GET", [ "api", "runs", id, view ] -> Right (RunView { id, view, round: Object.lookup "round" req.query })
  "GET", [ "api", "compare" ] -> Right (Compare { runs: filter (_ /= "") (split (Pattern ",") (param "runs" "")), market: Object.lookup "market" req.query })
  "GET", [ "api", "agency" ] -> Right (Agency { playerSeed: Object.lookup "playerSeed" req.query, preset: Object.lookup "preset" req.query, alternative: Object.lookup "alternative" req.query })
  _, _ -> Left (failWith 404 req.path)
  where
  segments = filter (_ /= "") (split (Pattern "/") req.path)
  param k d = fromMaybe d (Object.lookup k req.query)

-- | A body decoded as the typed record it must be; a body that does not decode is a 400.
decodeBody :: forall a. DecodeJson a => String -> Either Reply a
decodeBody text = either (\e -> Left (failWith 400 e)) Right (readJson text)

-- | An operation as the client sends it. Ids and query values are percent-encoded.
wire :: ApiRequest -> Wire
wire = case _ of
  ListRuns { archived } -> get ("/api/runs" <> query (if archived then [ "archived=true" ] else []))
  Scenario -> get "/api/scenario"
  Glossary -> get "/api/glossary"
  LaunchRun l -> post "/api/runs" (stringify (encodeJson l))
  Entities id -> get (run id <> "/entities")
  Entity id e -> get (run id <> "/entity/" <> enc e)
  RunView { id, view, round } -> get (run id <> "/" <> view <> query (catMaybes [ optional "round" round ]))
  Compare { runs, market } -> get ("/api/compare" <> query ([ "runs=" <> joinWith "," (map enc runs) ] <> catMaybes [ optional "market" market ]))
  Agency { playerSeed, preset, alternative } -> get ("/api/agency" <> query (catMaybes [ optional "playerSeed" playerSeed, optional "preset" preset, optional "alternative" alternative ]))
  Derive { id, body } -> post (run id <> "/derive") body
  Resume { id, rounds } -> post (run id <> "/resume") (stringify (encodeJson { rounds }))
  Archive id -> post (run id <> "/archive") "{}"
  Unarchive id -> post (run id <> "/unarchive") "{}"
  Delete { id, confirm } -> { method: "DELETE", url: run id <> query [ "confirm=" <> enc confirm ], body: "" }
  where
  get url = { method: "GET", url, body: "" }
  post url body = { method: "POST", url, body }
  run id = "/api/runs/" <> enc id
  enc s = fromMaybe s (encodeURIComponent s)
  optional k = map (\v -> k <> "=" <> enc v)
  query [] = ""
  query xs = "?" <> joinWith "&" xs
