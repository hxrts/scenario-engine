-- | The HTTP route table of the `API` row (docs/07_simulator.md §8). It is the one
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
  , replyIn
  , failWith
  , endpointNames
  , errorShape
  , Access(..)
  , isOpen
  , accessOf
  , Service(..)
  , Publish
  , submissionShape
  , metadataShape
  , Doc
  , endpointAccess
  ) where

import Prelude

import Laws.Types (RefKind(RefActor))
import Core.Shape (class Payload, Desc, Shape, boolShape, decodeJsonWith, defaulted, encodeWith, field, fieldOf, fromPayload, intShape, object, objectValuesOf, optionalOf, recordOf, refersTo, stringShape, toPayload)

import Control.Alt ((<|>))

import Schema.View (View, parseView, viewName)
import Core.Shape as Shape
import Data.Argonaut.Core (fromString, stringify, toString)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Schema.Run (runClassShape)
import Data.Array (catMaybes, filter, findMap, length, zipWith, find)
import Data.Either (Either(..), either)

import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
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

import Effects.Api (ApiRequest, Metadata, Reply, StartLive, Submission)
import Schema.Cast (castShape)
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
newtype Part a b = Part { read :: Incoming -> Either Reply b, write :: a -> Outgoing, doc :: Doc }

-- | What a request carries, as a published description reads it: its path captures and its body.
type Doc = { params :: Array { name :: String, integer :: Boolean }, body :: Maybe Desc }

noDoc :: Doc
noDoc = { params: [], body: Nothing }

instance Functor (Part a) where
  map f (Part p) = Part p { read = map f <<< p.read }

instance Apply (Part a) where
  apply (Part f) (Part x) = Part
    { read: \i -> f.read i <*> x.read i
    , write: \a -> let l = f.write a in let r = x.write a in { captures: l.captures <> r.captures, query: l.query <> r.query, body: l.body <|> r.body }
    , doc: { params: f.doc.params <> x.doc.params, body: f.doc.body <|> x.doc.body }
    }

instance Applicative (Part a) where
  pure b = Part { read: \_ -> Right b, write: \_ -> nothing, doc: noDoc }

nothing :: Outgoing
nothing = { captures: [], query: [], body: Nothing }

-- | A path capture, as the text the path gives.
capture :: forall a. String -> (a -> String) -> Part a String
capture name get = Part
  { read: \i -> Right (fromMaybe "" (Object.lookup name i.captures))
  , write: \a -> nothing { captures = [ Tuple name (get a) ] }
  , doc: noDoc { params = [ { name, integer: false } ] }
  }

-- | A capture that is a whole number; any other text is a 404 naming it.
intCapture :: forall a. String -> (a -> Int) -> Part a Int
intCapture name get = Part
  { read: \i -> let s = fromMaybe "" (Object.lookup name i.captures) in maybe (Left (failWith 404 s)) Right (Int.fromString s)
  , write: \a -> nothing { captures = [ Tuple name (show (get a)) ] }
  , doc: noDoc { params = [ { name, integer: true } ] }
  }

-- | A capture that names a View; any other text is a 404 naming it.
viewCapture :: forall a. String -> (a -> View) -> Part a View
viewCapture name get = Part
  { read: \i -> let s = fromMaybe "" (Object.lookup name i.captures) in maybe (Left (failWith 404 s)) Right (parseView s)
  , write: \a -> nothing { captures = [ Tuple name (viewName (get a)) ] }
  , doc: noDoc { params = [ { name, integer: false } ] }
  }

-- | An optional query value.
optionalParam :: forall a. String -> (a -> Maybe String) -> Part a (Maybe String)
optionalParam key get = Part
  { read: \i -> Right (Object.lookup key i.query)
  , write: \a -> nothing { query = maybe [] (\v -> [ Tuple key (enc v) ]) (get a) }
  , doc: noDoc
  }

-- | An optional whole-number query value; any other text is a 400 naming it.
optionalIntParam :: forall a. String -> (a -> Maybe Int) -> Part a (Maybe Int)
optionalIntParam key get = Part
  { read: \i -> case Object.lookup key i.query of
      Nothing -> Right Nothing
      Just s -> maybe (Left (failWith 400 (key <> " must be an integer, got " <> s))) (Right <<< Just) (Int.fromString s)
  , write: \a -> nothing { query = maybe [] (\v -> [ Tuple key (enc (show v)) ]) (get a) }
  , doc: noDoc
  }

-- | An optional query value read through a shape whose form is a string (a name); any other text is a 400 naming it.
optionalNamedParam :: forall a b. Shape b -> String -> (a -> Maybe b) -> Part a (Maybe b)
optionalNamedParam s key get = Part
  { read: \i -> case Object.lookup key i.query of
      Nothing -> Right Nothing
      Just v -> either (\e -> Left (failWith 400 (key <> ": " <> printJsonDecodeError e))) (Right <<< Just) (decodeJsonWith Shape.Snapshot s (fromString v))
  , write: \a -> nothing { query = maybe [] (\v -> [ Tuple key (enc (fromMaybe "" (toString (encodeWith Shape.Snapshot s v)))) ]) (get a) }
  , doc: noDoc
  }

-- | A comma-separated query value; blank items are dropped.
commaParam :: forall a. String -> (a -> Array String) -> Part a (Array String)
commaParam key get = Part
  { read: \i -> Right (filter (_ /= "") (split (Pattern ",") (fromMaybe "" (Object.lookup key i.query))))
  , write: \a -> nothing { query = [ Tuple key (joinWith "," (map enc (get a))) ] }
  , doc: noDoc
  }

-- | The body, through its shape; a body that does not decode is a 400.
bodyOf :: forall a b. Shape b -> (a -> b) -> Part a b
bodyOf s get = Part
  { read: \i -> decodeBody s i.body
  , write: \a -> nothing { body = Just (stringify (encodeWith Shape.Authored s (get a))) }
  , doc: noDoc { body = Just s.desc }
  }

-- | An endpoint: its method, its path (`:name` is a capture), and how its operation's payload is
-- | read from and written to a request.
type Endpoint b = { method :: String, path :: String, part :: Part b b, access :: Access }

-- | Who may call an endpoint, and how it is published, if it is. Every endpoint is `Admin Nothing` unless the table
-- | says otherwise, so a new route is private and unpublished until someone decides it is not. A published admin route
-- | states in its document that it needs the admin session.
data Access = Admin (Maybe Publish) | Open (Maybe Publish)

isOpen :: Access -> Boolean
isOpen = case _ of
  Admin _ -> false
  Open _ -> true

-- | The services built outside this repository, each with its own OpenAPI document.
data Service = Transcription | Narrative

derive instance Eq Service
derive instance Generic Service _

-- | How a route is published: to which service, and what it does.
type Publish = { service :: Service, summary :: String }

derive instance Eq Access

-- | A request for one endpoint of the table, without its payload's fields.
type Entry a = { name :: String, method :: String, path :: Array String, access :: Access, doc :: Doc, read :: Incoming -> Either Reply a }

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
    [ { name: reflectSymbol name, method: e.method, path: segmentsOf e.path, access: e.access, doc: p.doc, read: map (inj <<< Constructor <<< fromPayload) <<< p.read } ]
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
  { "ListRuns": get "/api/runs" ((\runClass -> { runClass }) <$> optionalNamedParam runClassShape "class" _.runClass)
  , "Premise": get "/api/premise" (pure unit)
  , "Glossary": get "/api/glossary" (pure unit)
  , "Actors": get "/api/runs/:id/actors" (capture "id" identity)
  , "Actor": get "/api/runs/:id/actor/:actor" (Tuple <$> capture "id" fst <*> capture "actor" snd)
  , "RunView": get "/api/runs/:id/:view"
      ado
        id <- capture "id" _.id
        view <- viewCapture "view" _.view
        round <- optionalIntParam "round" _.round
        in { id, view, round }
  , "BalanceSheetLinks": get "/api/runs/:id/balancesheets" (capture "id" identity)
  , "BalanceSheet": (get "/api/runs/:id/balancesheet/:token" ((\id token -> { id, token }) <$> capture "id" _.id <*> capture "token" _.token)) { access = Open Nothing }
  , "OpenApi": (get "/api/openapi/:service" (capture "service" identity)) { access = Open Nothing }
  , "RoundReport": (get "/api/runs/:id/rounds/:round/report" ((\id round -> { id, round }) <$> capture "id" _.id <*> intCapture "round" _.round)) { access = Open (Just { service: Narrative, summary: "What one round produced, to write a report from." }) }
  , "Compare": get "/api/compare"
      ado
        runs <- commaParam "runs" _.runs
        exchange <- optionalParam "exchange" _.exchange
        in { runs, exchange }
  , "StartLive": (post "/api/live" (bodyOf startLiveShape identity)) { access = Admin (Just { service: Transcription, summary: "Start the live run: its cast, rounds, window, seed, tag and metadata. A production run needs its metadata complete. Refused while a run is live, unless replace clears it first." }) }
  , "GetLive": (get "/api/live" (pure unit)) { access = Open (Just { service: Transcription, summary: "The live run: the round open, whether it is paused or ending, and which seats have spoken." }) }
  , "SubmitTranscript": (post "/api/live/transcripts" (bodyOf submissionShape identity)) { access = Open (Just { service: Transcription, summary: "Submit one seat's text for the round that is open." }) }
  , "SubmitManual": (post "/api/live/submissions" (bodyOf submissionShape identity)) { access = Admin (Just { service: Transcription, summary: "A facilitator submits a human seat's text for the round that is open; the record marks it as entered by the facilitator." }) }
  , "AdvanceLive": (post "/api/live/advance" (pure unit)) { access = Admin (Just { service: Transcription, summary: "Close the open round now: every human seat not yet heard says nothing, and the round resolves." }) }
  , "PauseLive": (post "/api/live/pause" (pure unit)) { access = Admin (Just { service: Transcription, summary: "Stop the propose window's clock. Submissions are still taken." }) }
  , "ResumeLive": (post "/api/live/resume" (pure unit)) { access = Admin (Just { service: Transcription, summary: "Restart the propose window's clock." }) }
  , "EndLive": (post "/api/live/end" (pure unit)) { access = Admin (Just { service: Transcription, summary: "Complete the live run after the round being played." }) }
  , "ClearLive": (post "/api/live/clear" (pure unit)) { access = Admin (Just { service: Transcription, summary: "Stop the live run now, record it as cleared with the rounds it played, and free the slot." }) }
  }
  where
  endpoint :: forall b. String -> String -> Part b b -> Endpoint b
  endpoint method path part = { method, path, part, access: Admin Nothing }

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
endpointAccess :: Array { name :: String, method :: String, path :: String, pattern :: Array String, access :: Access, doc :: Doc, publish :: Maybe Publish }
endpointAccess = map (\r -> { name: r.name, method: r.method, path: "/" <> joinWith "/" (map fill r.path), pattern: r.path, access: r.access, doc: r.doc, publish: publishOf r.access }) endpoints.rows
  where
  fill s = case stripPrefix (Pattern ":") s of
    Just _ -> "1"
    Nothing -> s
  publishOf = case _ of
    Open p -> p
    Admin p -> p

-- | The names of the table's endpoints, one per operation of `ApiRequest`, in the order of the type.
endpointNames :: Array String
endpointNames = map _.name endpoints.rows

-- | A body decoded through its shape; a body that does not decode is a 400.
decodeBody :: forall a. Shape a -> String -> Either Reply a
decodeBody s text = either (Left <<< failWith 400) Right (decodeError (Shape.parse Shape.Authored s text))

-- | The body of `POST /api/sessions`.
-- | A live run's metadata.
metadataShape :: Shape Metadata
metadataShape = object "Metadata" ado
  name <- fieldOf "name" "the run's name" (optionalOf stringShape) _.name
  facilitator <- fieldOf "facilitator" "who runs it" (optionalOf stringShape) _.facilitator
  date <- fieldOf "date" "when it is played" (optionalOf stringShape) _.date
  venue <- fieldOf "venue" "where it is played" (optionalOf stringShape) _.venue
  players <- fieldOf "players" "the player of each human seat, by seat id" (defaulted Object.empty Object.isEmpty (objectValuesOf stringShape)) _.players
  notes <- fieldOf "notes" "anything else" (optionalOf stringShape) _.notes
  in { name, facilitator, date, venue, players, notes }

-- | The body of `POST /api/live`.
startLiveShape :: Shape StartLive
startLiveShape = object "StartLive" ado
  composition <- fieldOf "composition" "which composition to play; absent is the first" (optionalOf stringShape) _.composition
  cast <- fieldOf "cast" "each castable Actor's role: human, agent, agent:<model>, preset:<name> or nonplayer:<name>; absent casts every seat as an agent" (optionalOf castShape) _.cast
  rounds <- fieldOf "rounds" "the round count; absent keeps the composition's" (optionalOf intShape) _.rounds
  window <- fieldOf "window" "seconds a round stays open; absent waits for every seat" (optionalOf intShape) _.window
  playerSeed <- fieldOf "playerSeed" "the player seed; absent draws one" (optionalOf intShape) _.playerSeed
  tag <- fieldOf "tag" "mock, or production, which needs its metadata complete" stringShape _.tag
  metadata <- fieldOf "metadata" "the run's name, facilitator, date, venue, players and notes" metadataShape _.metadata
  replace <- fieldOf "replace" "clear a run that is live first" (optionalOf boolShape) _.replace
  in { composition, cast, rounds, window, playerSeed, tag, metadata, replace }

-- | The body of `POST /api/live/transcripts` and `POST /api/live/submissions`.
submissionShape :: Shape Submission
submissionShape = object "Submission" ado
  seat <- fieldOf "seat" "the seat speaking" (refersTo (show RefActor) stringShape) _.seat
  round <- fieldOf "round" "the round it is for" intShape _.round
  text <- fieldOf "text" "what the seat said" stringShape _.text
  in { seat, round, text }
