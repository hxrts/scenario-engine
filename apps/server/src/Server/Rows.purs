-- | What the server's programs may ask for. A read program has `READS`, so it cannot write or play.
module Server.Rows (READS, module Exported, SERVER) where

import Views.Compare (CompareView)
import Effects.Launch (LAUNCHES) as Exported
import Effects.Signatures (CACHE, CLOCK, ENTROPY, OBSERVE, PLAY, PROCESS, SESSIONS, STORE)
import Prelude (Unit)
import Type.Row (type (+))
import Views.Common (StoredRun)

type READS r = OBSERVE + CACHE StoredRun CompareView + r

type SERVER r = OBSERVE + CACHE StoredRun CompareView + STORE + PLAY + CLOCK + ENTROPY + PROCESS + SESSIONS + r
