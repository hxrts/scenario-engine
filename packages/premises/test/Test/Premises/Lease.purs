-- | A capacity a premise makes tradeable by declaration alone (docs/06 §9): a `Free` capacity Resource,
-- | traded on an Exchange and drawn by a transformation, in the DSL only. Expected values are hand-computed in the comments.
module Test.Premises.Lease (spec) where

import Prelude

import Laws.Engine.Commit (resolveRound)
import Laws.Invariants (checkAfter)
import Laws.Query (holding, reserveOf)
import Laws.Types (Capability(..), Id(..), Intent, IntentF(..), ExchangeTag, ResourceTag, SignedCandidateTransaction, Transferability(..), candidateTransactionId)
import Laws.World (WorldState, seeded)
import Core.Draw (Seed(..))
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafeCrashWith)
import Schema.Build.Base (keyed, (:=))
import Schema.Build.Quantity (q)
import Schema.Build.World (byKey, emptyWorld, actorsOf, fromExecutor, noActor, noTransformation, noExchange, noResource, resourcesOf)
import Premises.Kit.Resources (capacity)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

plant :: Id ResourceTag
plant = Id "plant"

goods :: Id ResourceTag
goods = Id "G"

money :: Id ResourceTag
money = Id "M"

exchange :: Id ExchangeTag
exchange = Id "plant-exchange"

-- | The world: the plant is priced at 2 on its Exchange, the owner offers nothing, a tenant holds 100 M and none of it.
-- | The transformation makes 1 G per unit, drawing 1 unit of the tenant's own plant for the round.
world :: WorldState
world = seeded (Seed 1) emptyWorld
  { numeraire = money
  , resources = resourcesOf
      [ Tuple "M" noResource
      , Tuple "G" noResource
      , Tuple "plant" capacity { transferability = Free }
      ]
  , exchanges = byKey
      [ Tuple "plant-exchange" noExchange
          { base = plant
          , quote = money
          , cost = { intercept: q 2, slope: q 0, domain: { lo: q (-100), hi: q 100 } }
          , reserves = keyed [ plant := 10, money := 100 ]
          , access = [ Id "TENANT" ]
          }
      ]
  , transformations = byKey [ Tuple "run" noTransformation { outputs = keyed [ goods := 1 ], draws = [ fromExecutor plant (q 1) ] } ]
  , actors = actorsOf
      [ Tuple "TENANT" noActor { accounts = keyed [ money := 100 ], capabilities = [ MayTrade exchange, MayTransform (Id "run") ] }
      , Tuple "OWNER" noActor { accounts = keyed [ plant := 5 ], capabilities = [ MayTransfer plant ] }
      ]
  }

submit :: String -> Int -> Array Intent -> SignedCandidateTransaction
submit who round intents = { candidateTransaction: { id: candidateTransactionId round (Id who) 0, author: Id who, intents, signers: [ Id who ], procedure: Nothing }, signatures: [ Id who ] }

play :: WorldState -> Array SignedCandidateTransaction -> WorldState
play w inputs = either (unsafeCrashWith <<< show) (\r -> check r.world (checkAfter w r)) (resolveRound w inputs)
  where
  check after = case _ of
    [] -> after
    failed -> unsafeCrashWith (show failed)

spec :: Spec Unit
spec = describe "A capacity leased through an Exchange (DSL only)" do
  let
    tenant = Id "TENANT"
    bought = play world [ submit "TENANT" 1 [ Trade { exchange, quantity: q 4, limitPrice: Just (q 2) } ] ]
    ran = play bought [ submit "TENANT" 2 [ Transform { by: tenant, transformation: Id "run", quantity: q 3 } ] ]
    capped = play ran [ submit "TENANT" 3 [ Transform { by: tenant, transformation: Id "run", quantity: q 5 } ] ]
  it "is traded on its Exchange like any Resource: 4 bought at 2 costs 8 M" do
    holding bought tenant money `shouldEqual` q 92
    holding bought tenant plant `shouldEqual` q 4
    reserveOf bought exchange plant `shouldEqual` q 6
    reserveOf bought exchange money `shouldEqual` q 108
  it "is drawn by a transformation and comes back: 3 run makes 3 G and leaves the 4 of plant" do
    holding ran tenant goods `shouldEqual` q 3
    holding ran tenant plant `shouldEqual` q 4
    holding ran tenant money `shouldEqual` q 92
  it "caps an order at what the tenant holds: 5 ordered with 4 held fills 4, for 7 G in all" do
    holding capped tenant goods `shouldEqual` q 7
    holding capped tenant plant `shouldEqual` q 4
  it "moves between Actors once Free, which a NonTransferable capacity refuses" do
    let give = [ submit "OWNER" 1 [ Transfer { from: Id "OWNER", to: tenant, resource: plant, quantity: q 2 } ] ]
    holding (play world give) tenant plant `shouldEqual` q 2
    let refused = play (world { resources = map (\d -> if d.id == plant then d { transferability = NonTransferable } else d) world.resources }) give
    holding refused tenant plant `shouldEqual` q 0
