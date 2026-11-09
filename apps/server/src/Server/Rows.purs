-- | What the server's programs may ask for. A read program has `READS`, so it cannot write or play.
module Server.Rows (READS, LAUNCHES, SERVER) where

import Effects.Signatures (CACHE, CLOCK, ENTROPY, OBSERVE, PLAY, PROCESS, STORE)
import Server.Errors (Answer)
import Type.Row (type (+))
import Views.Common (StoredRun)

type READS r = OBSERVE + CACHE StoredRun Answer + r

type LAUNCHES r = OBSERVE + PLAY + CLOCK + ENTROPY + PROCESS + r

type SERVER r = OBSERVE + CACHE StoredRun Answer + STORE + PLAY + CLOCK + ENTROPY + PROCESS + r
