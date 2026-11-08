-- | The HTTP route table of the `API` row (docs/06_simulator.md §8). It is the one
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
  , reply
  , failWith
  , allViews
  , viewName
  , parseView
  , errorShape
  ) where

import Prelude

import Core.Shape as Shape
import Core.Shape (Shape, encodeWith, fieldOf, intShape, object, optionalOf, prim, stringShape)
import Data.Argonaut.Core (Json, fromNumber, isNull, jsonNull, stringify)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Array (catMaybes, filter, find)
import Data.Either (Either(..), either)
import Data.Bifunctor (lmap)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.String (Pattern(..), joinWith, split)
import Effect (Effect)
import Effects.Api (ApiRequest(..), DeriveRequest, Launch, Reply, View(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import JSURI (decodeURIComponent, encodeURIComponent)
import Schema.Decode.Common (decodeError)

-- | The path prefix every operation's URL starts with: the HTTP device hands these requests to the route.
apiPrefix :: String
apiPrefix = "/api/"

-- | A request as the HTTP device delivers it: method, path, decoded query, and body text.
type Request = { method :: String, path :: String, query :: Object String, body :: String }

-- | The operations' semantics, supplied by the composition root: one function over the typed request.
type Implementation = ApiRequest -> Effect Reply

-- | A request as the client sends it: method, URL (with its query), and body text.
type Wire = { method :: String, url :: String, body :: String }

-- | A reply: the status, and the body as JSON.
reply :: forall a. EncodeJson a => Int -> a -> Reply
reply status a = { status, body: stringify (encodeJson a) }

-- | An error reply: the status, and the message in the body's `error` field.
failWith :: Int -> String -> Reply
failWith status error = { status, body: stringify (encodeWith Shape.Snapshot errorShape { error }) }

-- | Every view of a stored run.
allViews :: Array View
allViews = [ Overview, Balances, Markets, Facilities, Economy, Representation, Procedures, Objectives, Standings, NetWorth, Actions, Drivers, Balance, Events ]

-- | A view's path segment.
viewName :: View -> String
viewName = case _ of
  Overview -> "overview"
  Balances -> "balances"
  Markets -> "markets"
  Facilities -> "facilities"
  Economy -> "economy"
  Representation -> "representation"
  Procedures -> "procedures"
  Objectives -> "objectives"
  Standings -> "standings"
  NetWorth -> "networth"
  Actions -> "actions"
  Drivers -> "drivers"
  Balance -> "balance"
  Events -> "events"

-- | The view a path segment names.
parseView :: String -> Maybe View
parseView s = find (\v -> viewName v == s) allViews

-- | An error reply's body.
errorShape :: Shape { error :: String }
errorShape = object "ErrorBody" ((\error -> { error }) <$> fieldOf "error" "what went wrong" stringShape _.error)

-- | Answer a request: parse it (an error reply when it is not a route or its body does not decode),
-- | then run the operation.
route :: Implementation -> Request -> Effect Reply
route impl req = either pure impl (parse req)

-- | A request as an operation, or the error reply it earns before the operation runs.
parse :: Request -> Either Reply ApiRequest
parse req = case req.method, segments of
  "GET", [ "api", "runs" ] -> Right (ListRuns { archived: param "archived" "" == "true" })
  "GET", [ "api", "premise" ] -> Right Premise
  "GET", [ "api", "glossary" ] -> Right Glossary
  "POST", [ "api", "runs" ] -> map LaunchRun (decodeBody launchShape req.body)
  "POST", [ "api", "runs", id, "derive" ] -> map (\body -> Derive { id, body }) (decodeBody deriveShape req.body)
  "POST", [ "api", "runs", id, "resume" ] -> map (\b -> Resume { id, rounds: b.rounds }) (decodeBody resumeShape req.body)
  "POST", [ "api", "runs", id, "archive" ] -> Right (Archive id)
  "POST", [ "api", "runs", id, "unarchive" ] -> Right (Unarchive id)
  "DELETE", [ "api", "runs", id ] -> Right (Delete { id, confirm: param "confirm" "" })
  "GET", [ "api", "runs", id, "entities" ] -> Right (Entities id)
  "GET", [ "api", "runs", id, "entity", entity ] -> Right (Entity id entity)
  "GET", [ "api", "runs", id, view ] -> case parseView view of
    Just v -> Right (RunView { id, view: v, round: Object.lookup "round" req.query })
    Nothing -> Left (failWith 404 view)
  "GET", [ "api", "compare" ] -> Right (Compare { runs: filter (_ /= "") (split (Pattern ",") (param "runs" "")), market: Object.lookup "market" req.query })
  "GET", [ "api", "agency" ] -> Right (Agency { playerSeed: Object.lookup "playerSeed" req.query, preset: Object.lookup "preset" req.query, alternative: Object.lookup "alternative" req.query })
  _, _ -> Left (failWith 404 req.path)
  where
  -- the client percent-encodes each segment (`wire`), so each is decoded after the path is split
  segments = map (\s -> fromMaybe s (decodeURIComponent s)) (filter (_ /= "") (split (Pattern "/") req.path))
  param k d = fromMaybe d (Object.lookup k req.query)

-- | A body decoded through its shape; a body that does not decode is a 400.
decodeBody :: forall a. Shape a -> String -> Either Reply a
decodeBody s text = either (Left <<< failWith 400) Right (decodeError (Shape.parse Shape.Premise s text))

-- | The body of `POST /api/runs`.
launchShape :: Shape Launch
launchShape = object "Launch" ado
  name <- fieldOf "name" "the new run's name" stringShape _.name
  preset <- fieldOf "preset" "the preset every seat plays" stringShape _.preset
  playerSeed <- fieldOf "playerSeed" "the player seed; absent or null draws one" (optionalOf intShape) _.playerSeed
  rounds <- fieldOf "rounds" "the round count; absent keeps the configuration's" (optionalOf intShape) _.rounds
  in { name, preset, playerSeed, rounds }

-- | The body of `POST /api/runs/:id/resume`.
resumeShape :: Shape { rounds :: Int }
resumeShape = object "Resume" ((\rounds -> { rounds }) <$> fieldOf "rounds" "the new total round count" intShape _.rounds)

-- | `playerSeed` in a derive body: absent keeps the source's seed, null draws a new one, a number names one.
seedChoiceShape :: Shape (Maybe (Maybe Int))
seedChoiceShape = (prim "seed" encode decode) { absent = isNothing, missing = Just Nothing }
  where
  encode = maybe jsonNull (maybe jsonNull (fromNumber <<< toNumber))
  decode j
    | isNull j = Right (Just Nothing)
    | otherwise = lmap (const "playerSeed must be an integer or null") (decodeJson j) <#> (Just <<< Just)

-- | The body of `POST /api/runs/:id/derive`. Absent fields keep the source's values (`Game.Recipe`).
deriveShape :: Shape DeriveRequest
deriveShape = object "DeriveRequest" ado
  kind <- fieldOf "kind" "how the run is derived" stringShape _.kind
  name <- fieldOf "name" "the new run's name" (optionalOf stringShape) _.name
  preset <- fieldOf "preset" "a variant's preset" (optionalOf stringShape) _.preset
  playerSeed <- fieldOf "playerSeed" "a variant's player seed" seedChoiceShape _.playerSeed
  rounds <- fieldOf "rounds" "a variant's round count" (optionalOf intShape) _.rounds
  overrides <- fieldOf "overrides" "a counterfactual's overrides" (optionalOf (prim "overrides" identity (Right :: Json -> Either String Json))) _.overrides
  in { kind, name, preset, playerSeed, rounds, overrides }

-- | An operation as the client sends it. Ids and query values are percent-encoded.
wire :: ApiRequest -> Wire
wire = case _ of
  ListRuns { archived } -> get ("/api/runs" <> query (if archived then [ "archived=true" ] else []))
  Premise -> get "/api/premise"
  Glossary -> get "/api/glossary"
  LaunchRun l -> post "/api/runs" (stringify (encodeWith Shape.Premise launchShape l))
  Entities id -> get (run id <> "/entities")
  Entity id e -> get (run id <> "/entity/" <> enc e)
  RunView { id, view, round } -> get (run id <> "/" <> viewName view <> query (catMaybes [ optional "round" round ]))
  Compare { runs, market } -> get ("/api/compare" <> query ([ "runs=" <> joinWith "," (map enc runs) ] <> catMaybes [ optional "market" market ]))
  Agency { playerSeed, preset, alternative } -> get ("/api/agency" <> query (catMaybes [ optional "playerSeed" playerSeed, optional "preset" preset, optional "alternative" alternative ]))
  Derive { id, body } -> post (run id <> "/derive") (stringify (encodeWith Shape.Premise deriveShape body))
  Resume { id, rounds } -> post (run id <> "/resume") (stringify (encodeWith Shape.Premise resumeShape { rounds }))
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
