-- | Test support for the view suites: a simulated run as the Run Store reads it back.
module Test.Views.Support (toStored, toStoredWith) where

import Prelude

import Core.Draw (Seed(..))
import Schema.Run (RunStatus(..), Spec, emptySpec)
import Handlers.Simulate (SimRun)
import Views.Common (StoredRun, storedRun)

-- | The same records and snapshots, with no spec fields.
toStored :: String -> SimRun -> StoredRun
toStored = toStoredWith emptySpec

-- | The same records and snapshots, under the spec fields given.
toStoredWith :: Spec -> String -> SimRun -> StoredRun
toStoredWith spec id r = storedRun
  { id
  , name: id
  , status: Completed
  , manifest: r.manifest
  , playerSeed: case r.playerSeed of
      Seed n -> show n
  , spec
  }
  r.records
  (map (const []) r.records)
