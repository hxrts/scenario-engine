-- | The route table: each request's method and path segments pick one handler from `Server.Read`,
-- | `Server.Launch`, or `Server.Derive`. Anything else is a 404.
module Server.Routes (route) where

import Prelude

import Adapters.Store (Db)
import Data.Array (filter)
import Data.Either (Either(..), either)
import Data.String (Pattern(..), split)
import Effect (Effect)
import Harness.Simulate (Context)
import Harness.Views.Overview as Overview
import Http (Request, Response)
import Server.Derive (deriveRun, deleteOne, resumeRun, setArchive)
import Server.Errors (failWith, ok)
import Server.Launch (launch, readLaunch)
import Server.Read (agencyView, compareView, entityView, getView, param, runSummaries)

route :: Context -> Db -> Request -> Effect Response
route ctx db req = case req.method, filter (_ /= "") (split (Pattern "/") req.path) of
  "GET", [ "api", "runs" ] -> runSummaries (param req.query "archived" "" == "true") db >>= ok <<< Overview.runList
  "POST", [ "api", "runs" ] -> case readLaunch req.body of
    Left e -> failWith 400 e
    Right l -> launch ctx db l >>= either (failWith 400) ok
  "POST", [ "api", "runs", id, "derive" ] -> deriveRun ctx db id req.body
  "POST", [ "api", "runs", id, "resume" ] -> resumeRun ctx db id req.body
  "POST", [ "api", "runs", id, "archive" ] -> setArchive db id true
  "POST", [ "api", "runs", id, "unarchive" ] -> setArchive db id false
  "DELETE", [ "api", "runs", id ] -> deleteOne db id (param req.query "confirm" "")
  "GET", [ "api", "runs", id, "entity", entity ] -> entityView db id entity
  "GET", [ "api", "runs", id, view ] -> getView ctx db req.query id view
  "GET", [ "api", "compare" ] -> compareView db req.query
  "GET", [ "api", "agency" ] -> agencyView ctx req.query
  _, _ -> failWith 404 req.path
