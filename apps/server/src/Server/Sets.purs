-- | Named Run sets (docs/06_simulator.md §10.5): a name, the ids of immutable runs, and the baseline. A set
-- | never copies run data, and a run in a set cannot be deleted while the set names it.
module Server.Sets (setSummary, setsView, saveSet, renameRunSet, deleteRunSet, setsNaming) where

import Prelude

import Data.Array (any, elem, filter, find, notElem, null)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence, traverse)
import Data.String (trim)
import Effects.Observe (listRuns, listSets)
import Effects.Signatures (OBSERVE, RunSet)
import Effects.Store (createSet, deleteSet, renameSet)
import Run (Run)
import Server.Errors (Answer, badRequest, conflict, describe, internal, notFound, ok)
import Server.Read (loadRun)
import Server.Compositions (Compositions, ofRun)
import Views.Batch (batchSummary)
import Server.Rows (READS, SERVER)
import Type.Row (type (+))
import Views.Common (Table)
import Handlers.Api.Contract (AnyRefusal)

setsView :: forall r. Run (READS r) (Answer AnyRefusal { sets :: Array RunSet })
setsView = listSets >>= \sets -> ok { sets }

-- | `GET /api/sets/:name/summary`: the final measures across a set's runs. A set's run that cannot be read is a 500.
setSummary :: forall r. Compositions -> String -> Run (READS r) (Answer AnyRefusal Table)
setSummary compositions name = find (\s -> s.name == name) <$> listSets >>= case _ of
  Nothing -> notFound ("no scenario set " <> name)
  Just set -> traverse loadRun set.runs <#> sequence >>= case _ of
    Left e -> internal ("a run of " <> name <> " cannot be read: " <> describe e)
    Right runs -> ok (batchSummary (ofRun compositions) runs)

-- | The sets that name a run.
setsNaming :: forall r. String -> Run (OBSERVE + r) (Array RunSet)
setsNaming id = filter (elem id <<< _.runs) <$> listSets

saveSet :: forall o. RunSet -> Run (SERVER o) (Answer AnyRefusal RunSet)
saveSet set = do
  known <- map _.id <$> listRuns
  existing <- listSets
  case unit of
    _ | trim set.name == "" -> badRequest "a scenario set needs a name"
    _ | any (\s -> s.name == set.name) existing -> conflict ("a scenario set named " <> set.name <> " already exists")
    _ | null set.runs -> badRequest "a scenario set needs at least one run"
    _ | notElem set.baseline set.runs -> badRequest ("the baseline " <> set.baseline <> " is not one of the set's runs")
    _ | missing <- filter (\id -> notElem id known) set.runs, not (null missing) -> badRequest ("no run " <> show missing)
    _ -> createSet set *> ok set

renameRunSet :: forall o. { name :: String, to :: String } -> Run (SERVER o) (Answer AnyRefusal { name :: String })
renameRunSet r = do
  existing <- listSets
  if trim r.to == "" then badRequest "a scenario set needs a name"
  else if any (\s -> s.name == r.to) existing then conflict ("a scenario set named " <> r.to <> " already exists")
  else do
    n <- renameSet r.name r.to
    if n == 0 then notFound ("no scenario set " <> r.name) else ok { name: r.to }

deleteRunSet :: forall o. String -> Run (SERVER o) (Answer AnyRefusal { name :: String })
deleteRunSet name = do
  n <- deleteSet name
  if n == 0 then notFound ("no scenario set " <> name) else ok { name }
