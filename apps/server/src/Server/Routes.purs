-- | The route table: each request's method and path segments pick one handler from `Server.Read`,
-- | `Server.Launch`, or `Server.Derive`; `GET /api/scenario` is the launch form's defaults and `GET /api/glossary`
-- | the glossary the dashboard shows. Anything else is a 404.
module Server.Routes (route) where

import Prelude

import Handlers.Store (StoreBackend)
import Data.Array (filter)
import Data.Either (Either(..), either)
import Data.String (Pattern(..), split)
import Effect (Effect)
import Simulator.Simulate (Context)
import Analysis.Views.Glossary (glossary)
import Analysis.Views.Overview as Overview
import Http (Request, Response)
import Server.Derive (deriveRun, deleteOne, resumeRun, setArchive)
import Server.Errors (failWith, ok)
import Server.Launch (launch, readLaunch, scenarioInfo)
import Server.Read (agencyView, compareView, entitiesView, entityView, getView, param, runSummaries)

route :: Context -> StoreBackend -> Request -> Effect Response
route ctx store req = case req.method, filter (_ /= "") (split (Pattern "/") req.path) of
  "GET", [ "api", "runs" ] -> runSummaries (param req.query "archived" "" == "true") store >>= ok <<< Overview.runList
  "GET", [ "api", "scenario" ] -> scenarioInfo ctx store >>= ok
  "GET", [ "api", "glossary" ] -> ok glossary
  "POST", [ "api", "runs" ] -> case readLaunch req.body of
    Left e -> failWith 400 e
    Right l -> launch ctx store l >>= either (failWith 400) ok
  "POST", [ "api", "runs", id, "derive" ] -> deriveRun ctx store id req.body
  "POST", [ "api", "runs", id, "resume" ] -> resumeRun ctx store id req.body
  "POST", [ "api", "runs", id, "archive" ] -> setArchive store id true
  "POST", [ "api", "runs", id, "unarchive" ] -> setArchive store id false
  "DELETE", [ "api", "runs", id ] -> deleteOne store id (param req.query "confirm" "")
  "GET", [ "api", "runs", id, "entities" ] -> entitiesView store id
  "GET", [ "api", "runs", id, "entity", entity ] -> entityView store id entity
  "GET", [ "api", "runs", id, view ] -> getView ctx store req.query id view
  "GET", [ "api", "compare" ] -> compareView ctx store req.query
  "GET", [ "api", "agency" ] -> agencyView ctx req.query
  _, _ -> failWith 404 req.path
