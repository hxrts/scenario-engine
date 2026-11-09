-- | Pins: who may reach each endpoint (docs/06_simulator.md §8), walking the whole route table.
module Test.Server.Gate (spec) where

import Prelude

import Effect (Effect)

import Data.Array (filter)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Foreign.Object as Object
import Gate (gated, parseEnv, secretsFrom)
import Handlers.Api.Route (Access(..), endpointAccess)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

secrets :: { session :: Maybe String }
secrets = secretsFrom identity (parseEnv "# a comment\nADMIN_PASSWORD=pw\n")

statusOf
  :: forall t13
   . { session :: Maybe String
     }
  -> { method :: String
     , path :: String
     | t13
     }
  -> String
  -> Effect Int
statusOf s e cookie = _.status <$> gated s (\_ -> pure { status: 200, body: "" }) { method: e.method, path: e.path, query: Object.empty, body: "{}", cookie }

spec :: Spec Unit
spec = describe "Who may reach the API" do
  it "reads .env, ignoring comments and blank lines" do
    secrets.session `shouldEqual` Just "pw:admin"
  it "every endpoint refuses a caller with nothing, unless it is open" do
    for_ (filter (\e -> e.access /= Open) endpointAccess) \e -> do
      status <- liftEffect (statusOf secrets e "")
      { name: e.name, status } `shouldEqual` { name: e.name, status: 401 }
  it "an admin session reaches every endpoint" do
    for_ endpointAccess \e -> do
      -- past the gate, a sample body may still be refused by its endpoint, but not for want of a login
      status <- liftEffect (statusOf secrets e "x=1; admin=pw:admin")
      (status /= 401) `shouldEqual` true
  it "unset secrets lock everything they guard" do
    let locked = secretsFrom identity (parseEnv "")
    for_ (filter (\e -> e.access /= Open) endpointAccess) \e -> do
      status <- liftEffect (statusOf locked e "admin=:admin")
      status `shouldEqual` 401
