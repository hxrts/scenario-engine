-- | Runs for the server tests to read. The API no longer launches runs (simulated runs come from the static suite), so
-- | a test stores a preset run through the server's own effect stack, as the suite does: `serve` answers
-- | `POST /api/runs` that way, with the old launch body and reply, and sends every other request to the router.
module Test.Server.Seed (serve, serveOn) where

import Prelude

import Assembly.Assemble (checkRounds, defaultCast)
import Assembly.Composition (CompositionParts)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Core.Shape (Format(..), Shape, fieldOf, intShape, object, optionalOf, parse, stringShape)
import Data.Array (find, head)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effects.Api (Reply)
import Effects.Launch (start)
import Handlers.Api.Route (Request, route)
import Handlers.Sessions (SessionHost)
import Handlers.Store (StoreBackend)
import Server.Stack (Devices, ServerCache, implementOn, runServer, systemDevices)

type Launch = { name :: String, preset :: String, playerSeed :: Maybe Int, rounds :: Maybe Int, composition :: Maybe String }

launchShape :: Shape Launch
launchShape = object "Launch" ado
  composition <- fieldOf "composition" "which composition to play; absent is the first" (optionalOf stringShape) _.composition
  name <- fieldOf "name" "the run's name" stringShape _.name
  preset <- fieldOf "preset" "the preset every seat plays" stringShape _.preset
  playerSeed <- fieldOf "playerSeed" "the player seed; absent draws one" (optionalOf intShape) _.playerSeed
  rounds <- fieldOf "rounds" "the round count; absent keeps the configuration's" (optionalOf intShape) _.rounds
  in { composition, name, preset, playerSeed, rounds }

-- | The router over real system devices, with `POST /api/runs` storing a preset run.
serve :: Array CompositionParts -> StoreBackend -> ServerCache -> SessionHost -> Request -> Effect Reply
serve ctxs store cache sessions = serveOn { system: systemDevices, store, cache, sessions } ctxs

-- | The same over the devices given.
serveOn :: Devices -> Array CompositionParts -> Request -> Effect Reply
serveOn d ctxs req
  | req.method == "POST" && req.path == "/api/runs" = case parse Snapshot launchShape req.body of
      Left e -> pure { status: 400, body: printJsonDecodeError e }
      Right (l :: Launch) -> case maybe (head ctxs) (\c -> find (\p -> p.composition.id == c) ctxs) l.composition of
        Nothing -> pure { status: 400, body: "no such composition" }
        Just p -> case traverse_ (checkRounds p.composition) l.rounds of
          Left e -> pure { status: 400, body: show e }
          Right _ -> runServer d (start "server" p { name: l.name, cast: defaultCast p l.preset, preset: Just l.preset, playerSeed: l.playerSeed, rounds: l.rounds, overrides: [], cacheKey: Nothing }) <#> case _ of
            Left e -> { status: 400, body: e }
            Right x -> { status: 200, body: "{\"playerSeed\":" <> show x.playerSeed <> ",\"id\":\"" <> x.id <> "\"}" }
  | otherwise = route (implementOn d ctxs) req
