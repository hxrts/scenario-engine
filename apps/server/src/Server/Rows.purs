-- | What the server's programs may ask for. A read program has `READS`, so it cannot write or play.
module Server.Rows (READS, module Exported, SERVER) where

import Effects.Launch (LAUNCHES) as Exported
import Effects.Signatures (CACHE, CLOCK, ENTROPY, OBSERVE, PLAY, PROCESS, SESSIONS, STORE)
import Server.Cache (Profiles)
import Type.Row (type (+))
import Views.Common (StoredRun)

type READS r = OBSERVE + CACHE StoredRun Profiles + r

type SERVER r = OBSERVE + CACHE StoredRun Profiles + STORE + PLAY + CLOCK + ENTROPY + PROCESS + SESSIONS + r
