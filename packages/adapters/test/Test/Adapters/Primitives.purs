-- | The adapter primitives that need no model and no store (docs/06_simulator.md §5): an injected seed
-- | is used as given, an absent one is drawn from the OS, the clock reads ISO 8601 in UTC, and the
-- | premise reads return the files they name, or a failure that names the path.
module Test.Adapters.Primitives (spec) where

import Prelude

import Premises.Meridian.Names (meridianDir)

import Adapters.Clock (nowIso)
import Adapters.Console (closeInput, readLine)
import Adapters.Filesystem (fileOps)
import Premises.Tree (readRootFile, readPremise)
import Adapters.Random (resolveSeed)
import Core.Draw (Seed(..))
import Data.Array (length, sort)
import Data.Either (Either(..), isLeft)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
import Data.String.CodeUnits as SCU
import Effect.Aff (attempt)
import Effect.Class (liftEffect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = describe "Primitives: seed, clock, premise reads" do
  it "input closed once stays closed: a later prompt fails" do
    liftEffect closeInput
    out <- attempt (readLine "> ")
    isLeft out `shouldEqual` true

  it "an injected seed is used as given" do
    s <- liftEffect (resolveSeed (Just (Seed 7)))
    s `shouldEqual` Seed 7

  it "an absent seed is drawn from the OS, within the seed range" do
    Seed n <- liftEffect (resolveSeed Nothing)
    (n >= 0 && n < 2147483647) `shouldEqual` true

  it "the clock reads ISO 8601 in UTC" do
    t <- liftEffect nowIso
    SCU.length t `shouldEqual` 24
    contains (Pattern "T") t `shouldEqual` true
    contains (Pattern "Z") t `shouldEqual` true

  it "a premise directory reads its files, with the shared presets" do
    s <- liftEffect (readPremise fileOps meridianDir)
    case s of
      Right sc -> do
        map _.name sc.sections `shouldEqual` sort (map _.name sc.sections)
        (length sc.sections > 0) `shouldEqual` true
        sc.presets.name `shouldEqual` "presets.json"
      Left e -> fail e

  it "a missing premise directory is a failure that names its path" do
    s <- liftEffect (readPremise fileOps "no-such-premise")
    case s of
      Left e -> contains (Pattern "no-such-premise") e `shouldEqual` true
      Right _ -> fail "a missing directory must not read"

  it "readText reads a file under the premise root; a missing file names its path" do
    ok <- liftEffect (readRootFile fileOps "presets.json")
    case ok of
      Right _ -> pure unit
      Left e -> fail e
    missing <- liftEffect (readRootFile fileOps "no-such-file.json")
    case missing of
      Left e -> contains (Pattern "no-such-file.json") e `shouldEqual` true
      Right _ -> fail "a missing file must not read"
