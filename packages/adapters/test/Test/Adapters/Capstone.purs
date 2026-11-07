-- | The capstone run through the SQLite Run Store (docs/08_coverage.md): the DSL-authored capstone is
-- | simulated, saved, reloaded byte-identically, and replayed from the stored records. The joint pass
-- | over the recorded transcript is in `Test.Handlers.Capstone`.
module Test.Adapters.Capstone (spec) where

import Prelude

import Adapters.Scenarios (readScenario, readText)
import Adapters.Store (open, sqlitePort)
import Board.Gameboard.Instantiate (instantiate)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)
import Foreign.Object as Object
import Data.Array (length)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effects.Round (runGame)
import Handlers.Replay (runReplay)
import Handlers.Store (createRun, persister, reader)
import Policy.InWorld (inWorldOf)
import Scenarios.Load (load, loadPresets, loadRun)
import Simulator.Capstone (Capstone, decodeCapstone)
import Simulator.Simulate (SeatPolicy(..), simulate, simulateWith)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

orThrow :: forall a. Either String a -> Aff a
orThrow = either (throwError <<< error) pure

capstone :: Aff Capstone
capstone = do
  file <- liftEffect (readText "capstone/run.json") >>= orThrow >>= (orThrow <<< loadRun)
  orThrow (decodeCapstone file)

spec :: Spec Unit
spec = describe "Capstone: the Run Store" do
  it "the capstone run round-trips through the store: saved, reloaded byte-identically, and replayed" do
    loaded <- liftEffect (readScenario "meridian/v0") >>= orThrow >>= (orThrow <<< load)
    presets <- liftEffect (readText "presets.json") >>= orThrow >>= (orThrow <<< loadPresets)
    cap <- capstone
    let
      ctx = { gameboard: loaded.gameboard, config: loaded.config, presets }
      spec' = { seats: map Custom cap.seats, playerSeed: cap.playerSeed, overrides: [], rounds: Just cap.rounds }
    db <- liftEffect (open ":memory:")
    let port = sqlitePort db
    stored <- liftEffect $ simulateWith ctx spec' \manifest -> do
      createRun port { id: "capstone", name: "capstone", created: "2026-01-01T00:00:00Z", spec: Object.singleton "preset" "capstone", manifest, source: Nothing, kind: Nothing }
      persister port "capstone" manifest.initial
    _ <- orThrow stored
    let store = reader port
    rows <- liftEffect (store.rounds "capstone")
    length rows `shouldEqual` cap.rounds
    records <- orThrow (traverse _.record rows)
    snapshots <- orThrow (traverse _.snapshot rows)
    pure' <- orThrow (simulate ctx spec')
    map (stringify <<< encodeJson) records `shouldEqual` map (stringify <<< encodeJson <<< _.record) pure'.records
    map (stringify <<< encodeJson) snapshots `shouldEqual` map (stringify <<< encodeJson <<< _.world) pure'.records
    snapshots `shouldEqual` map _.world pure'.records
    case instantiate ctx.gameboard ctx.config, inWorldOf ctx.gameboard of
      Right initial, Right inWorld -> case runReplay cap.rounds records (runGame inWorld cap.rounds initial.world) of
        Left e -> fail e
        Right out -> map _.world out.records `shouldEqual` snapshots
      _, _ -> fail "instantiation"
