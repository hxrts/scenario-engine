-- | Named scenario sets (docs/06_simulator.md §10.5): a name, the ids of immutable runs, and the baseline. A set
-- | never copies run data, and a run in a set cannot be deleted while the set names it.
module Server.Sets (setsView, saveSet, renameScenarioSet, deleteScenarioSet, setsNaming) where

import Prelude

import Data.Array (any, elem, filter, notElem, null)
import Data.String (trim)
import Effects.Observe (listRuns, listSets)
import Effects.Signatures (OBSERVE, ScenarioSet)
import Effects.Store (createSet, deleteSet, renameSet)
import Run (Run)
import Server.Errors (Answer, badRequest, conflict, notFound, ok)
import Server.Rows (READS, SERVER)
import Type.Row (type (+))
import Views.Keys as Keys

setsView :: forall r. Run (READS r) Answer
setsView = listSets >>= \sets -> ok Keys.sets { sets }

-- | The sets that name a run.
setsNaming :: forall r. String -> Run (OBSERVE + r) (Array ScenarioSet)
setsNaming id = filter (elem id <<< _.runs) <$> listSets

saveSet :: forall o. ScenarioSet -> Run (SERVER o) Answer
saveSet set = do
  known <- map _.id <$> listRuns
  existing <- listSets
  case unit of
    _ | trim set.name == "" -> badRequest "a scenario set needs a name"
    _ | any (\s -> s.name == set.name) existing -> conflict ("a scenario set named " <> set.name <> " already exists")
    _ | null set.runs -> badRequest "a scenario set needs at least one run"
    _ | notElem set.baseline set.runs -> badRequest ("the baseline " <> set.baseline <> " is not one of the set's runs")
    _ | missing <- filter (\id -> notElem id known) set.runs, not (null missing) -> badRequest ("no run " <> show missing)
    _ -> createSet set *> ok Keys.scenarioSet set

renameScenarioSet :: forall o. { name :: String, to :: String } -> Run (SERVER o) Answer
renameScenarioSet r = do
  existing <- listSets
  if trim r.to == "" then badRequest "a scenario set needs a name"
  else if any (\s -> s.name == r.to) existing then conflict ("a scenario set named " <> r.to <> " already exists")
  else do
    n <- renameSet r.name r.to
    if n == 0 then notFound ("no scenario set " <> r.name) else ok Keys.renamedSet { name: r.to }

deleteScenarioSet :: forall o. String -> Run (SERVER o) Answer
deleteScenarioSet name = do
  n <- deleteSet name
  if n == 0 then notFound ("no scenario set " <> name) else ok Keys.renamedSet { name }
