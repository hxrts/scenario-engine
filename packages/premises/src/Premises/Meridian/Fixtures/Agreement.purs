-- | Golden fixture for an agreement entered during play (docs/05_meridian.md §27, Fixture H): a loan that
-- | settles over three rounds. Expected values are literals from the worked example.
module Premises.Meridian.Fixtures.Agreement (fixtureH) where

import Prelude (($))

import Premises.Kit.Prelude

import Core.Kernel.Types (Amount(..), Party(..), Short(..), noNext)
import Assembly.Patch (withTemplates)
import Laws.Types (IntentF(..))
import Premises.Meridian.Fixtures.Common (cut, defaultFixture, fixture, holding, money)
import Premises.Meridian.Names (n)
import Schema.Build.Agreement (agreement, clause, held, spawn, step)
import Schema.Build.Fixture (noRound)

-- | Fixture H: SA lends JP 50 M. Entered in round 1, advanced in round 2, repaid at 55 in round 3.
fixtureH :: Tuple String Json
fixtureH = fixture "H"
  ( defaultFixture
      { cut = cut { actors = [ holding n.sa money [], holding n.jp money [] ], patches = [ loanTemplates ] }
      , rounds = Just
          [ noRound
              { candidateTransactions = Just [ enterLoan ]
              , expect = Just (noExpect { accounts = balances [ Tuple n.sa (keyed [ n.money := 155 ]), Tuple n.jp (keyed [ n.money := 150 ]) ] })
              }
          , noRound { expect = Just (noExpect { accounts = balances [ Tuple n.sa (keyed [ n.money := 105 ]), Tuple n.jp (keyed [ n.money := 200 ]) ] }) }
          , noRound { expect = Just (noExpect { accounts = balances [ Tuple n.sa (keyed [ n.money := 160 ]), Tuple n.jp (keyed [ n.money := 145 ]) ] }) }
          ]
      }
  )
  where
  loanTemplates = withTemplates [ advance, repay ]
  lender = Bound "lender"
  borrower = Bound "borrower"
  both = [ Tuple "borrower" borrower, Tuple "lender" lender ]
  kind = Id "loan"
  advance = step "loan-advance" [ "principal" ] [ "borrower", "lender" ] $ agreement kind []
    [ clause "advance" lender { from: held lender, to: held borrower, resource: n.money, owed: Arg "principal" } Reject []
        noNext { performed = [ spawn "loan-repay" 1 [ Tuple "balance" (Times { ratio: q "1.1", amount: Moved }) ] both ] }
    ]
  repay = step "loan-repay" [ "balance" ] [ "borrower", "lender" ] $ agreement kind []
    [ clause "repay" borrower { from: held borrower, to: held lender, resource: n.money, owed: Arg "balance" } Cap [] noNext ]
  enterLoan = (candidateTransaction { author: n.sa, intents: [ Enter { template: "loan-advance", args: [ { name: "principal", value: q 50 } ], parties: [ { role: "lender", actor: n.sa }, { role: "borrower", actor: n.jp } ] } ] })
    { signatures = [ n.jp ], candidateTransaction { signers = [ n.jp ] } }
