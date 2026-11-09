-- | The glossary the dashboard shows: a plain-language definition for every table column and header
-- | field, and what an empty value means (docs/02_type_system.md §2.5, docs/06_simulator.md §9). It is
-- | generated from the columns the views use; only the two texts for names that are not columns are
-- | written here. Text only, served whole by `GET /api/glossary`; the dashboard looks names up and
-- | fills the `{name}` and `{round}` placeholders. Pure.
module Views.Glossary (Glossary, allColumns, glossary) where

import Prelude

import Core.World (Focus)
import Data.Array (concatMap, nubBy, sortBy)
import Data.Function (on)
import Data.Maybe (maybe)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Views.Actions as Actions
import Views.Agency as Agency
import Views.Assertions as Assertions
import Views.Balance as Balance
import Views.Batch as Batch
import Views.Balances as Balances
import Views.Common (Column, Reason, sharedColumns)
import Views.Compare as Compare
import Views.Drivers as Drivers
import Views.Markets as Markets
import Views.Overview as Overview
import Views.Representation as Representation
import Views.Standings as Standings

-- | The glossary. `definitions` and `reasons` are keyed by column or field name. `unnamed` is the
-- | entry for a name with none, which is a compared run's column (`{name}` is the name); `round` defines
-- | the columns of a per-round table by prefix (`{round}` is the round).
type Glossary =
  { definitions :: Object String
  , reasons :: Object Reason
  , unnamed :: { definition :: String, reason :: Reason }
  , round :: { prefix :: String, definition :: String }
  }

-- | Every column the views use over the loaded settings' focus Resources, in name order, each once.
allColumns :: Array Focus -> Array Column
allColumns foci = nubBy (compare `on` _.name)
  ( sortBy (compare `on` _.name)
      ( sharedColumns <> concatMap identity
          [ Overview.columns, Balances.columns, Standings.columns, Balance.columns, concatMap Drivers.columns foci, Representation.columns, Representation.procedureColumns, Assertions.columns, Agency.columns, Batch.columns, Actions.columns, concatMap Markets.columns foci, Compare.columns ]
      )
  )

glossary :: Array Focus -> Glossary
glossary foci =
  { definitions: Object.fromFoldable (map (\c -> Tuple c.name c.definition) (allColumns foci))
  , reasons: Object.fromFoldable (concatMap (\c -> maybe [] (\r -> [ Tuple c.name r ]) c.emptyReason) (allColumns foci))
  , unnamed:
      { definition: "The value of {name} for this row. This column is a compared run's id."
      , reason: { label: "not recorded", reason: "The server gave no value for {name} in this run." }
      }
  , round: { prefix: "Round ", definition: "The value after round {round}. Round 0 is the start of the run." }
  }
