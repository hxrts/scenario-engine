-- | The adapter primitives that need no model and no store (docs/06_simulator.md §5): an injected seed
-- | is used as given, an absent one is drawn from the OS, the clock reads ISO 8601 in UTC, and the
-- | premise reads return the files they name, or a failure that names the path.
module Test.Adapters.Primitives (spec) where

import Prelude

import Premises.Meridian.Names (meridianDir)

import Adapters.Clock (nowIso)
import Adapters.Console (closeInput, readLine)
import Adapters.Filesystem (fileOps)
import Premises.Layout (compositionsDir)
import Premises.Tree (readComposition, readRootFile)
import Adapters.Random (drawOsSeed)
import Data.Either (Either(..), isLeft)
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

  it "the OS draw is within the seed range" do
    n <- liftEffect drawOsSeed
    (n >= 0 && n < 2147483647) `shouldEqual` true

  it "the clock reads ISO 8601 in UTC" do
    t <- liftEffect nowIso
    SCU.length t `shouldEqual` 24
    contains (Pattern "T") t `shouldEqual` true
    contains (Pattern "Z") t `shouldEqual` true

  it "a composition directory reads its files" do
    s <- liftEffect (readComposition fileOps meridianDir)
    case s of
      Right sc -> do
        sc.composition.name `shouldEqual` "composition.json"
        sc.instantiation.name `shouldEqual` "instantiation.json"
        sc.presets.name `shouldEqual` "presets.json"
      Left e -> fail e

  it "a missing composition directory is a failure that names its path" do
    s <- liftEffect (readComposition fileOps "no-such-premise")
    case s of
      Left e -> contains (Pattern "no-such-premise") e `shouldEqual` true
      Right _ -> fail "a missing directory must not read"

  it "readText reads a file under the premise root; a missing file names its path" do
    ok <- liftEffect (readRootFile fileOps (compositionsDir <> "/" <> meridianDir <> "/presets.json"))
    case ok of
      Right _ -> pure unit
      Left e -> fail e
    missing <- liftEffect (readRootFile fileOps "no-such-file.json")
    case missing of
      Left e -> contains (Pattern "no-such-file.json") e `shouldEqual` true
      Right _ -> fail "a missing file must not read"
