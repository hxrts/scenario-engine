-- A read program that writes: must not compile.
module Probe.Writes where

import Prelude

import Effects.Store (deleteRun)
import Run (Run)
import Server.Rows (READS)

probe :: forall r. Run (READS r) Unit
probe = void (deleteRun "x")
