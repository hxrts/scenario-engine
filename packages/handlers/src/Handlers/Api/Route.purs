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
  , replyIn
  , failWith
  , endpointNames
  , errorShape
  , Access(..)
  , accessOf
  , endpointAccess
  ) where

import Prelude

import Control.Alt ((<|>))
import Schema.Run (deriveKindShape)
import Schema.View (View, parseView, viewName)
import Core.Shape as Shape
import Core.Shape (class Payload, Format(..), Shape, arrayOf, decodeJsonWith, encodeWith, field, fieldOf, fromPayload, intShape, object, optionalOf, prim, recordOf, stringShape, toPayload)
import Data.Argonaut.Core (Json, fromNumber, isNull, jsonNull, stringify)
import Data.Array (catMaybes, filter, findMap, length, zipWith, find)
import Data.Either (Either(..), either)
import Data.Bifunctor (lmap)
import Data.Int (toNumber)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe, isJust)
import Data.Generic.Rep (class Generic, Constructor(..), Sum(..), from, to)
import Data.String (Pattern(..), joinWith, split, stripPrefix)
import Data.Foldable (lookup)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Prim.Row as Row
import Record as Record
import Type.Proxy (Proxy(..))
import Effect (Effect)
import Effects.Signatures (RunSet)
import Assembly.Experiment (experimentRequestShape)
import Effects.Api (AgencyQuery, ApiRequest, DeriveRequest, Launch, LaunchBatch, Reply, StartSession, Submission)
import Foreign.Object (Object)
import Foreign.Object as Object
import JSURI (decodeURIComponent, encodeURIComponent)
import Schema.Decode (decodeError)

-- | The path prefix every operation's URL starts with: the HTTP device hands these requests to the route.
apiPrefix :: String
apiPrefix = "/api/"

-- | A request as the HTTP device delivers it: method, path, decoded query, and body text.
type Request = { method :: String, path :: String, query :: Object String, body :: String }

-- | The operations' semantics, supplied by the composition root: one function over the typed request.
type Implementation = ApiRequest -> Effect Reply

-- | A request as the client sends it: method, URL (with its query), and body text.
type Wire = { method :: String, url :: String, body :: String }

-- | A reply: the status, and the body through its shape, in the round-end World form the replies are written in.
reply :: forall a. Shape a -> Int -> a -> Reply
reply = replyIn Shape.Snapshot

-- | A reply in a given format. The premise format leaves an absent optional field out.
replyIn :: forall a. Shape.Format -> Shape a -> Int -> a -> Reply
replyIn format s status a = { status, body: stringify (encodeWith format s a) }

-- | An error reply: the status, and the message in the body's `error` field.
failWith :: Int -> String -> Reply
failWith status error = { status, body: stringify (encodeWith Shape.Snapshot errorShape { error }) }

-- | An error reply's body.
errorShape :: Shape { error :: String }
errorShape = recordOf "ErrorBody"
  ( field @"error" "what went wrong" stringShape
  )

-- | Answer a request: parse it (an error reply when it is not a route or its body does not decode),
-- | then run the operation.
route :: Implementation -> Request -> Effect Reply
route impl req = either pure impl (parse req)

-- | What a request brings to its endpoint: the path's captures by name, the decoded query, the body.
type Incoming = { captures :: Object String, query :: Object String, body :: String }

-- | What an endpoint's payload adds to a request: captures by name, query pairs with each value
-- | percent-encoded (the values of a pair are joined by commas), and the body when it has one.
type Outgoing = { captures :: Array (Tuple String String), query :: Array (Tuple String String), body :: Maybe String }

-- | One bidirectional part of a request: how a value is read from it, and written to it. Every part of an
-- | endpoint is stated once, so the reader and the writer cannot name different keys.
newtype Part a b = Part { read :: Incoming -> Either Reply b, write :: a -> Outgoing }

instance Functor (Part a) where
  map f (Part p) = Part p { read = map f <<< p.read }

instance Apply (Part a) where
  apply (Part f) (Part x) = Part
    { read: \i -> f.read i <*> x.read i
    , write: \a -> let l = f.write a in let r = x.write a in { captures: l.captures <> r.captures, query: l.query <> r.query, body: l.body <|> r.body }
    }

instance Applicative (Part a) where
  pure b = Part { read: \_ -> Right b, write: \_ -> nothing }

nothing :: Outgoing
nothing = { captures: [], query: [], body: Nothing }

-- | A path capture, as the text the path gives.
capture :: forall a. String -> (a -> String) -> Part a String
capture name get = Part
  { read: \i -> Right (fromMaybe "" (Object.lookup name i.captures))
  , write: \a -> nothing { captures = [ Tuple name (get a) ] }
  }

-- | A capture that is a whole number; any other text is a 404 naming it.
intCapture :: forall a. String -> (a -> Int) -> Part a Int
intCapture name get = Part
  { read: \i -> let s = fromMaybe "" (Object.lookup name i.captures) in maybe (Left (failWith 404 s)) Right (Int.fromString s)
  , write: \a -> nothing { captures = [ Tuple name (show (get a)) ] }
  }

-- | A capture that names a View; any other text is a 404 naming it.
viewCapture :: forall a. String -> (a -> View) -> Part a View
viewCapture name get = Part
  { read: \i -> let s = fromMaybe "" (Object.lookup name i.captures) in maybe (Left (failWith 404 s)) Right (parseView s)
  , write: \a -> nothing { captures = [ Tuple name (viewName (get a)) ] }
  }

-- | An optional query value.
optionalParam :: forall a. String -> (a -> Maybe String) -> Part a (Maybe String)
optionalParam key get = Part
  { read: \i -> Right (Object.lookup key i.query)
  , write: \a -> nothing { query = maybe [] (\v -> [ Tuple key (enc v) ]) (get a) }
  }

-- | An optional whole-number query value; any other text is a 400 naming it.
optionalIntParam :: forall a. String -> (a -> Maybe Int) -> Part a (Maybe Int)
optionalIntParam key get = Part
  { read: \i -> case Object.lookup key i.query of
      Nothing -> Right Nothing
      Just s -> maybe (Left (failWith 400 (key <> " must be an integer, got " <> s))) (Right <<< Just) (Int.fromString s)
  , write: \a -> nothing { query = maybe [] (\v -> [ Tuple key (enc (show v)) ]) (get a) }
  }

-- | A query value that is empty when it is missing, and always sent.
param :: forall a. String -> (a -> String) -> Part a String
param key get = Part
  { read: \i -> Right (fromMaybe "" (Object.lookup key i.query))
  , write: \a -> nothing { query = [ Tuple key (enc (get a)) ] }
  }

-- | A query flag: on when its value is `true`, and sent only when on.
flag :: forall a. String -> (a -> Boolean) -> Part a Boolean
flag key get = Part
  { read: \i -> Right (Object.lookup key i.query == Just "true")
  , write: \a -> nothing { query = if get a then [ Tuple key "true" ] else [] }
  }

-- | A comma-separated query value; blank items are dropped.
commaParam :: forall a. String -> (a -> Array String) -> Part a (Array String)
commaParam key get = Part
  { read: \i -> Right (filter (_ /= "") (split (Pattern ",") (fromMaybe "" (Object.lookup key i.query))))
  , write: \a -> nothing { query = [ Tuple key (joinWith "," (map enc (get a))) ] }
  }

-- | The body, through its shape; a body that does not decode is a 400.
bodyOf :: forall a b. Shape b -> (a -> b) -> Part a b
bodyOf s get = Part
  { read: \i -> decodeBody s i.body
  , write: \a -> nothing { body = Just (stringify (encodeWith Shape.Authored s (get a))) }
  }

-- | An endpoint: its method, its path (`:name` is a capture), and how its operation's payload is
-- | read from and written to a request.
type Endpoint b = { method :: String, path :: String, part :: Part b b, access :: Access }

-- | Who may call an endpoint. Every endpoint is `Admin` unless the table says otherwise, so a new route
-- | is private until someone decides it is not.
data Access = Admin | Open

derive instance Eq Access

-- | A request for one endpoint of the table, without its payload's fields.
type Entry a = { name :: String, method :: String, path :: Array String, access :: Access, read :: Incoming -> Either Reply a }

segmentsOf :: String -> Array String
segmentsOf = filter (_ /= "") <<< split (Pattern "/")

-- | The endpoints of a sum of operations, one per constructor: the record of them, and writing an
-- | operation by matching on its representation. A new constructor without an endpoint does not compile.
class EndpointRow :: Type -> Row Type -> Constraint
class EndpointRow rep row | rep -> row

instance (EndpointRow a l, EndpointRow b r, Row.Union l r row) => EndpointRow (Sum a b) row

instance (Payload args p, Row.Cons name (Endpoint p) () row) => EndpointRow (Constructor name args) row

class Endpoints :: Type -> Row Type -> Constraint
class Endpoints rep row where
  rowsOf :: forall a. Proxy rep -> Record row -> (rep -> a) -> Array (Entry a)
  sentOf :: Record row -> rep -> Written

instance (Endpoints a row, Endpoints b row) => Endpoints (Sum a b) row where
  rowsOf _ r inj = rowsOf (Proxy :: Proxy a) r (inj <<< Inl) <> rowsOf (Proxy :: Proxy b) r (inj <<< Inr)
  sentOf r = case _ of
    Inl x -> sentOf r x
    Inr x -> sentOf r x

instance (IsSymbol name, Payload args p, Row.Cons name (Endpoint p) _t row) => Endpoints (Constructor name args) row where
  rowsOf _ r inj =
    [ { name: reflectSymbol name, method: e.method, path: segmentsOf e.path, access: e.access, read: map (inj <<< Constructor <<< fromPayload) <<< p.read } ]
    where
    name = Proxy :: Proxy name
    e = Record.get name r
    Part p = e.part
  sentOf r (Constructor a) = { method: e.method, path: e.path, out: p.write (toPayload a) }
    where
    e = Record.get (Proxy :: Proxy name) r
    Part p = e.part

-- | An endpoint's method and path, with what the operation wrote.
type Written = { method :: String, path :: String, out :: Outgoing }

-- | The URL and body of a request, from what its payload wrote.
sent :: Written -> Wire
sent { method, path, out } =
  { method
  , url: "/" <> joinWith "/" (map segment (segmentsOf path)) <> queryOf out.query
  , body: fromMaybe (if method == "POST" then "{}" else "") out.body
  }
  where
  segment s = case stripPrefix (Pattern ":") s of
    Just name -> enc (fromMaybe "" (lookup name out.captures))
    Nothing -> s
  queryOf [] = ""
  queryOf xs = "?" <> joinWith "&" (map (\(Tuple k v) -> k <> "=" <> v) xs)

-- | Percent-encode one path segment or query value.
enc :: String -> String
enc s = fromMaybe s (encodeURIComponent s)

-- | The captures of a path that matches a request's segments.
matchPath :: Array String -> Array String -> Maybe (Object String)
matchPath pattern segments
  | length pattern /= length segments = Nothing
  | otherwise = (Object.fromFoldable <<< catMaybes) <$> sequence (zipWith capturing pattern segments)
      where
      capturing p s = case stripPrefix (Pattern ":") p of
        Just name -> Just (Just (Tuple name s))
        Nothing -> if p == s then Just Nothing else Nothing

-- | The table the route folds over.
type Table = { rows :: Array (Entry ApiRequest), write :: ApiRequest -> Written }

table :: forall rep row. Generic ApiRequest rep => EndpointRow rep row => Endpoints rep row => Record row -> Table
table r = { rows: rowsOf (Proxy :: Proxy rep) r to, write: sentOf r <<< from }

-- | The endpoint table: one row per operation of `ApiRequest`, and the only place a path is written.
-- | Rows are tried in the order of the type, so `/Actors` and `/Actor/:Actor` come before `/:view`.
endpoints :: Table
endpoints = table
  { "ListRuns": get "/api/runs" ((\archived -> { archived }) <$> flag "archived" _.archived)
  , "Premise": get "/api/premise" (pure unit)
  , "Glossary": get "/api/glossary" (pure unit)
  , "LaunchRun": post "/api/runs" (bodyOf launchShape identity)
  , "LaunchBatch": post "/api/batch" (bodyOf batchShape identity)
  , "Actors": get "/api/runs/:id/actors" (capture "id" identity)
  , "Actor": get "/api/runs/:id/actor/:actor" (Tuple <$> capture "id" fst <*> capture "actor" snd)
  , "RunView": get "/api/runs/:id/:view"
      ado
        id <- capture "id" _.id
        view <- viewCapture "view" _.view
        round <- optionalIntParam "round" _.round
        in { id, view, round }
  , "BalanceSheetLinks": get "/api/runs/:id/balancesheets" (capture "id" identity)
  , "BalanceSheet": (get "/api/runs/:id/balancesheet/:token" ((\id token -> { id, token }) <$> capture "id" _.id <*> capture "token" _.token)) { access = Open }
  , "RoundReport": (get "/api/runs/:id/rounds/:round/report" ((\id round -> { id, round }) <$> capture "id" _.id <*> intCapture "round" _.round)) { access = Open }
  , "Compare": get "/api/compare"
      ado
        runs <- commaParam "runs" _.runs
        exchange <- optionalParam "exchange" _.exchange
        in { runs, exchange }
  , "Agency": get "/api/agency" agencyQuery
  , "AgencyDetail": get "/api/agency/detail" agencyQuery
  , "RunExperiment": post "/api/experiments" (bodyOf experimentRequestShape identity)
  , "ListRunSets": get "/api/sets" (pure unit)
  , "SetSummary": get "/api/sets/:name/summary" (capture "name" identity)
  , "SaveRunSet": post "/api/sets" (bodyOf runSetShape identity)
  , "RenameRunSet": post "/api/sets/:name/rename" ((\name b -> { name, to: b.name }) <$> capture "name" _.name <*> bodyOf renameShape (\r -> { name: r.to }))
  , "DeleteRunSet": endpoint "DELETE" "/api/sets/:name" (capture "name" identity)
  , "Derive": post "/api/runs/:id/derive" ((\id body -> { id, body }) <$> capture "id" _.id <*> bodyOf deriveShape _.body)
  , "Step": post "/api/runs/:id/step" (capture "id" identity)
  , "Resume": post "/api/runs/:id/resume" ((\id b -> { id, rounds: b.rounds }) <$> capture "id" _.id <*> bodyOf resumeShape (\r -> { rounds: r.rounds }))
  , "StartSession": post "/api/sessions" (bodyOf startSessionShape identity)
  , "GetSession": (get "/api/sessions/:id" (capture "id" identity)) { access = Open }
  , "SubmitTranscript": (post "/api/sessions/:id/transcripts" ((\id submission -> { id, submission }) <$> capture "id" _.id <*> bodyOf submissionShape _.submission)) { access = Open }
  , "Archive": post "/api/runs/:id/archive" (capture "id" identity)
  , "Unarchive": post "/api/runs/:id/unarchive" (capture "id" identity)
  , "Delete": endpoint "DELETE" "/api/runs/:id" ((\id confirm -> { id, confirm }) <$> capture "id" _.id <*> param "confirm" _.confirm)
  }
  where
  agencyQuery :: Part AgencyQuery AgencyQuery
  agencyQuery = ado
    premise <- optionalParam "premise" _.premise
    playerSeed <- optionalParam "playerSeed" _.playerSeed
    preset <- optionalParam "preset" _.preset
    alternative <- optionalParam "alternative" _.alternative
    in { premise, playerSeed, preset, alternative }

  endpoint :: forall b. String -> String -> Part b b -> Endpoint b
  endpoint method path part = { method, path, part, access: Admin }

  get :: forall b. String -> Part b b -> Endpoint b
  get = endpoint "GET"

  post :: forall b. String -> Part b b -> Endpoint b
  post = endpoint "POST"

-- | A request as an operation, or the error reply it earns before the operation runs. The client
-- | percent-encodes each segment (`wire`), so each is decoded after the path is split.
parse :: Request -> Either Reply ApiRequest
parse req = case findMap matching endpoints.rows of
  Just answer -> answer
  Nothing -> Left (failWith 404 req.path)
  where
  segments = map (\s -> fromMaybe s (decodeURIComponent s)) (segmentsOf req.path)
  matching row
    | row.method /= req.method = Nothing
    | otherwise = matchPath row.path segments <#> \captures -> row.read { captures, query: req.query, body: req.body }

-- | Who may make a request, when it names an endpoint.
accessOf :: Request -> Maybe Access
accessOf req = _.access <$> find matches endpoints.rows
  where
  segments = map (\s -> fromMaybe s (decodeURIComponent s)) (segmentsOf req.path)
  matches row = row.method == req.method && isJust (matchPath row.path segments)

-- | An operation as the client sends it. Ids and query values are percent-encoded.
wire :: ApiRequest -> Wire
wire = sent <<< endpoints.write

-- | Every endpoint with who may call it, for the test that walks them.
endpointAccess :: Array { name :: String, method :: String, path :: String, access :: Access }
endpointAccess = map (\r -> { name: r.name, method: r.method, path: "/" <> joinWith "/" (map fill r.path), access: r.access }) endpoints.rows
  where
  fill s = case stripPrefix (Pattern ":") s of
    Just _ -> "1"
    Nothing -> s

-- | The names of the table's endpoints, one per operation of `ApiRequest`, in the order of the type.
endpointNames :: Array String
endpointNames = map _.name endpoints.rows

-- | A body decoded through its shape; a body that does not decode is a 400.
decodeBody :: forall a. Shape a -> String -> Either Reply a
decodeBody s text = either (Left <<< failWith 400) Right (decodeError (Shape.parse Shape.Authored s text))

-- | The body of `POST /api/sessions`.
startSessionShape :: Shape StartSession
startSessionShape = object "StartSession" ado
  premise <- fieldOf "premise" "which premise to play; absent is the first" (optionalOf stringShape) _.premise
  humans <- fieldOf "humans" "the seats a person plays" (arrayOf stringShape) _.humans
  rounds <- fieldOf "rounds" "the round count; absent keeps the premise's" (optionalOf intShape) _.rounds
  window <- fieldOf "window" "seconds a round stays open; absent waits for every seat" (optionalOf intShape) _.window
  playerSeed <- fieldOf "playerSeed" "the player seed; absent draws one" (optionalOf intShape) _.playerSeed
  in { premise, humans, rounds, window, playerSeed }

-- | The body of `POST /api/sessions/:id/transcripts`.
submissionShape :: Shape Submission
submissionShape = object "Submission" ado
  seat <- fieldOf "seat" "the seat speaking" stringShape _.seat
  round <- fieldOf "round" "the round it is for" intShape _.round
  text <- fieldOf "text" "what the seat said" stringShape _.text
  in { seat, round, text }

-- | The body of `POST /api/runs`.
launchShape :: Shape Launch
launchShape = object "Launch" ado
  premise <- fieldOf "premise" "which premise to play; absent is the first" (optionalOf stringShape) _.premise
  name <- fieldOf "name" "the new run's name" stringShape _.name
  preset <- fieldOf "preset" "the preset every seat plays" stringShape _.preset
  playerSeed <- fieldOf "playerSeed" "the player seed; absent or null draws one" (optionalOf intShape) _.playerSeed
  rounds <- fieldOf "rounds" "the round count; absent keeps the configuration's" (optionalOf intShape) _.rounds
  in { premise, name, preset, playerSeed, rounds }

-- | The body of `POST /api/batch`.
batchShape :: Shape LaunchBatch
batchShape = object "LaunchBatch" ado
  premises <- fieldOf "premises" "the Premises to play; absent is every loaded one" (optionalOf (arrayOf stringShape)) _.premises
  preset <- fieldOf "preset" "the preset every seat plays" stringShape _.preset
  playerSeed <- fieldOf "playerSeed" "the player seed; absent or null draws one per run" (optionalOf intShape) _.playerSeed
  rounds <- fieldOf "rounds" "the round count; absent keeps each Premise's" (optionalOf intShape) _.rounds
  in { premises, preset, playerSeed, rounds }

-- | The body of `POST /api/runs/:id/resume`.
resumeShape :: Shape { rounds :: Int }
resumeShape = recordOf "Resume"
  ( field @"rounds" "the new total round count" intShape
  )

-- | `playerSeed` in a derive body: absent keeps the source's seed, null draws a new one, a number names one.
seedChoiceShape :: Shape (Maybe (Maybe Int))
seedChoiceShape = (prim "seed" encode decode) { absent = isNothing, missing = Just Nothing }
  where
  encode = maybe jsonNull (maybe jsonNull (fromNumber <<< toNumber))
  decode j
    | isNull j = Right (Just Nothing)
    | otherwise = lmap (const "playerSeed must be an integer or null") (decodeJsonWith Snapshot intShape j) <#> (Just <<< Just)

-- | The body of `POST /api/runs/:id/derive`. Absent fields keep the source's values (`Assembly.Recipe`).
runSetShape :: Shape RunSet
runSetShape = object "RunSet" ado
  name <- fieldOf "name" "the set's name" stringShape _.name
  baseline <- fieldOf "baseline" "the id of the run the others are compared with" stringShape _.baseline
  runs <- fieldOf "runs" "the ids of the set's runs" (arrayOf stringShape) _.runs
  in { name, baseline, runs }

renameShape :: Shape { name :: String }
renameShape = recordOf "RenameSet"
  ( field @"name" "the set's new name" stringShape
  )

deriveShape :: Shape DeriveRequest
deriveShape = object "DeriveRequest" ado
  kind <- fieldOf "kind" "how the run is derived" deriveKindShape _.kind
  name <- fieldOf "name" "the new run's name" (optionalOf stringShape) _.name
  preset <- fieldOf "preset" "a variant's preset" (optionalOf stringShape) _.preset
  playerSeed <- fieldOf "playerSeed" "a variant's player seed" seedChoiceShape _.playerSeed
  rounds <- fieldOf "rounds" "a variant's round count" (optionalOf intShape) _.rounds
  overrides <- fieldOf "overrides" "a counterfactual's overrides" (optionalOf (prim "overrides" identity (Right :: Json -> Either String Json))) _.overrides
  in { kind, name, preset, playerSeed, rounds, overrides }
