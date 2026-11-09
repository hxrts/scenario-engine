-- | The engine-level golden fixtures (docs/05_meridian.md §21–§27). Every expected
-- | value is a literal from the worked examples: the DSL shares the world pieces, never
-- | the answers. `just _premises` compiles them to `packages/premises/fixtures/*.json`.
-- |
-- | The fixtures are grouped by family in `Premises.Meridian.Fixtures.*`; this module is the only
-- | list of them, and the golden test and the compiler both enumerate it.
-- |
-- | Fixture C5 has no expectation: it checks the seeded sampling noise (the golden test's
-- | C5 case), and its round is checked only for invariants. Every other fixture states its
-- | balances, outcomes, or both.
module Premises.Meridian.Fixtures (fixtures) where

import Data.Argonaut.Core (Json)
import Data.Tuple (Tuple)
import Premises.Meridian.Fixtures.Circulation (fixtureCirculation)
import Premises.Meridian.Fixtures.ConflictingActions (fixtureF)
import Premises.Meridian.Fixtures.GermanyEu (fixtureB1, fixtureB2, fixtureB2Pool, fixtureB3, fixtureB4Unsigned, fixtureB4)
import Premises.Meridian.Fixtures.ExchangeFill (fixtureA, fixtureA2)
import Premises.Meridian.Fixtures.NonPlayableResponse (fixtureD, fixtureDMinor)
import Premises.Meridian.Fixtures.Reservation (fixtureR, fixtureRAlone)
import Premises.Meridian.Fixtures.SignedCandidateTransactions (fixtureG1, fixtureG2, fixtureG3Ban, fixtureG3, fixtureG4, fixtureG5Unsigned, fixtureG5)
import Premises.Meridian.Fixtures.SmeRepresentation (fixtureC1, fixtureC2, fixtureC3, fixtureC5)

-- | Every fixture by name (the golden tests read these names and their JSON).
fixtures :: Array (Tuple String Json)
fixtures =
  [ fixtureA
  , fixtureA2
  , fixtureB1
  , fixtureB2
  , fixtureB2Pool
  , fixtureB3
  , fixtureB4Unsigned
  , fixtureB4
  , fixtureC1
  , fixtureC2
  , fixtureC3
  , fixtureC5
  , fixtureDMinor
  , fixtureD
  , fixtureR
  , fixtureRAlone
  , fixtureF
  , fixtureG1
  , fixtureG2
  , fixtureG3Ban
  , fixtureG3
  , fixtureG4
  , fixtureG5Unsigned
  , fixtureG5
  , fixtureCirculation
  ]
