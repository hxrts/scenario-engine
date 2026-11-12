-- | The static suite (docs/07 §3): what it holds, and what its cache key replays.
module Test.Premises.Suite (spec) where

import Prelude

import Data.Array (filter, length, sort)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)
import Effect.Aff (throwError)
import Effect.Exception (error)
import Foreign.Object as Object
import Premises.Load (parts)
import Premises.Pacific as Pacific
import Premises.Pacific.Names as Names
import Core.Num as Q
import Premises.Suite (EntryKind(..), planSuite, suiteOf)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "The static suite (docs/07 §3)" do
  it "holds one run per preset, then one agency run per seat on its alternative against the default preset's run" do
    p <- pacific
    let entries = suiteOf p
    length entries `shouldEqual` 22
    length (filter (isJust <<< _.preset) entries) `shouldEqual` 18
    map _.kind (filter (\e -> e.preset == Nothing) entries) `shouldEqual`
      [ AgencyRun { actor: Names.p.cn, alternative: "Hawk", baseline: "FreeTrade" }
      , AgencyRun { actor: Names.p.cnFirm, alternative: "Builder", baseline: "FreeTrade" }
      , AgencyRun { actor: Names.p.us, alternative: "Hawk", baseline: "FreeTrade" }
      , AgencyRun { actor: Names.p.usFirm, alternative: "Builder", baseline: "FreeTrade" }
      ]

  it "a fresh store plays every entry; a store holding every key plays none" do
    p <- pacific
    let first = planSuite identity "engine" [ { parts: p, text: "files" } ] []
    length first.play `shouldEqual` 22
    let stored = map (\x -> { id: x.entry.name, key: Just x.key }) first.play
    let again = planSuite identity "engine" [ { parts: p, text: "files" } ] stored
    [ length again.play, again.reused, length again.drop ] `shouldEqual` [ 0, 22, 0 ]

  it "a changed dial or engine replays every run and drops the old ones; a changed preset replays only the runs that use it" do
    p <- pacific
    let stored = map (\x -> { id: x.entry.name, key: Just x.key }) (planSuite identity "engine" [ { parts: p, text: "files" } ] []).play
    -- a dial changes the compiled files, so every run of the composition is replayed
    dialled <- orThrow (parts (Pacific.pacific (Pacific.defaultDials { homePremium = Q.add Pacific.defaultDials.homePremium Q.one })))
    let byDial = planSuite identity "engine" [ { parts: dialled, text: "other files" } ] stored
    [ length byDial.play, length byDial.drop ] `shouldEqual` [ 22, 22 ]
    let byEngine = planSuite identity "engine 2" [ { parts: p, text: "files" } ] stored
    [ length byEngine.play, length byEngine.drop ] `shouldEqual` [ 22, 22 ]
    -- Hawk is played by its own preset run and by the two states' agency runs
    let hawk = p { presets = Object.update (\s -> Just s { rules = [] }) "Hawk" p.presets }
    let byPreset = planSuite identity "engine" [ { parts: hawk, text: "files" } ] stored
    sort (map _.entry.name byPreset.play) `shouldEqual` [ "FreeTrade with CN on Hawk", "FreeTrade with US on Hawk", "Hawk" ]
    sort byPreset.drop `shouldEqual` [ "FreeTrade with CN on Hawk", "FreeTrade with US on Hawk", "Hawk" ]

  it "a preset that leaves the composition drops its run and nothing else" do
    p <- pacific
    let stored = map (\x -> { id: x.entry.name, key: Just x.key }) (planSuite identity "engine" [ { parts: p, text: "files" } ] []).play
    let fewer = planSuite identity "engine" [ { parts: p { presets = Object.delete "Ban" p.presets }, text: "files" } ] stored
    [ length fewer.play, fewer.reused ] `shouldEqual` [ 0, 21 ]
    fewer.drop `shouldEqual` [ "Ban" ]
  where
  pacific = orThrow (parts (Pacific.pacific Pacific.defaultDials))
  orThrow = case _ of
    Right x -> pure x
    Left e -> throwError (error e)
