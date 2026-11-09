-- A read program that reads: must compile, so the failure of writes.purs is about its row.
module Probe.Reads where

import Prelude

import Effects.Observe (listRuns)
import Run (Run)
import Server.Rows (READS)

probe :: forall r. Run (READS r) Unit
probe = void listRuns
