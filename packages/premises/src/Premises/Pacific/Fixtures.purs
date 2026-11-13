-- | Pacific's engine-level golden fixtures (docs/05_pacific.md §7): small worlds cut from Pacific with no
-- | circulation, whose decisions are literal candidate Transactions. They pin the matched-only Exchange and an
-- | export control, the US export tax, a home Exchange, an investment and the competence it trains, a transfer, the
-- | currency dealer, and a foreign loan under the asset fee, through `resolveRound` alone. Every expected value is a
-- | literal from docs/05.
module Premises.Pacific.Fixtures (fixtures) where

import Prelude (negate, (#))

import Premises.Kit.Prelude

import Assembly.Composition (CompositionParts)
import Foreign.Object as Object
import Laws.Types (Direction(..), FlowEffect(..), IntentF(..), LevyBasis(..), LevyPayer(..), LevyRecipient(..), RuleChangeF(..), RuleF(..), SignedCandidateTransaction)
import Premises.Kit.Fixtures (Cut, FixtureSpec, KeptActor)
import Premises.Kit.Fixtures as Kit
import Premises.Pacific (compositionWith, defaultDials, gameSeed, pacific)
import Premises.Pacific.Names (p)
import Premises.Pacific.Private (usBond)
import Premises.Pacific.Strategies (controlRule)
import Schema.Build.Fixture (noRound)
import Schema.Build.World (emptyCirculation)
import Schema.Build.Base (mayEnact, mayTransform)

base :: CompositionParts
base = { composition: compositionWith defaultDials, config: (pacific defaultDials).config { preset = Nothing }, presets: Object.empty }

fixture :: String -> FixtureSpec -> Tuple String Json
fixture = Kit.fixture { seed: gameSeed, base }

-- | Each Actor a fixture keeps: its accounts, and exactly the Capabilities it uses.
us :: KeptActor
us = { key: p.us, accounts: Just [ p.usd, p.technology, p.industry ], capabilities: Just [ mayTrade p.technologyWorld, mayTrade p.industryWorld, mayTrade p.technologyUs, mayTransfer p.usd, mayEnact p.usExportControl, mayEnact p.usAssetFee ] }

usFirm :: KeptActor
usFirm = { key: p.usFirm, accounts: Just [ p.usd, p.rmb, p.technology ], capabilities: Just [ mayTrade p.technologyWorld, mayTrade p.technologyUs, mayTrade p.currency ] }

usPrivate :: KeptActor
usPrivate = { key: p.usPrivate, accounts: Just [ p.usd, p.technology ], capabilities: Just [ mayTrade p.technologyUs ] }

cn :: KeptActor
cn = { key: p.cn, accounts: Just [ p.rmb, p.usd, p.technology, p.industry ], capabilities: Just [ mayTrade p.technologyWorld, mayTrade p.industryWorld, mayTrade p.currency, mayTransfer p.usd, mayTransfer p.rmb ] }

cnFirm :: KeptActor
cnFirm = { key: p.cnFirm, accounts: Just [ p.rmb, p.industry, p.industryCapacity, p.industryCompetence ], capabilities: Just [ mayTrade p.industryWorld, mayTransform p.cnFirmInvestment ] }

cut :: Cut
cut = { actors: [ us, usFirm, cn, cnFirm ], resources: [ p.usd, p.rmb, p.technology, p.industry ], exchanges: [ p.technologyWorld, p.industryWorld, p.technologyUs ], transformations: [], circulation: emptyCirculation, patches: [] }

spec :: FixtureSpec
spec = { cut, candidateTransactions: Nothing, expect: Nothing, rounds: Nothing }

-- | The US firm sells 4 T on the world Exchange, for USD, and China buys them with the USD it holds.
exports :: Array SignedCandidateTransaction
exports =
  [ candidateTransaction { author: p.usFirm, intents: [ trade { exchange: p.technologyWorld, quantity: q (-4), limit: Just (q 1) } ] }
  , candidateTransaction { author: p.cn, intents: [ trade { exchange: p.technologyWorld, quantity: q 4, limit: Just (q 1) } ] }
  ]

-- | P-A: the US firm exports 4 T to China at 1 USD and pays the export tax, 0.5 USD a unit, to the US state.
fixturePA :: Tuple String Json
fixturePA = fixture "P-A" spec
  { candidateTransactions = Just exports
  , expect = Just (noExpect { accounts = balances [ Tuple p.usFirm (keyed [ p.usd := 17, p.technology := 4 ]), Tuple p.cn (keyed [ p.usd := 2, p.technology := 4 ]), Tuple p.us (keyed [ p.usd := 114 ]) ], reserves = balances [ Tuple p.technologyWorld (keyed [ p.usd := 0, p.technology := 4 ]) ] })
  }

-- | P-H: the US firm sells 2 T at home to its own state, at the home price of 1.25 USD, and pays no tax.
fixturePH :: Tuple String Json
fixturePH = fixture "P-H" spec
  { candidateTransactions = Just
      [ candidateTransaction { author: p.usFirm, intents: [ trade { exchange: p.technologyUs, quantity: q (-2), limit: Just (q "1.25") } ] }
      , candidateTransaction { author: p.us, intents: [ trade { exchange: p.technologyUs, quantity: q 2, limit: Just (q "1.25") } ] }
      ]
  , expect = Just (noExpect { accounts = balances [ Tuple p.usFirm (keyed [ p.usd := "17.5", p.technology := 6 ]), Tuple p.us (keyed [ p.usd := "109.5", p.technology := 4 ]) ] })
  }

-- | P-B: the US enacts its export control on technology to China in round 1; in round 2 the same exports move
-- | nothing, so the firm keeps its T and China its USD.
fixturePB :: Tuple String Json
fixturePB = fixture "P-B" spec
  { rounds = Just
      [ noRound { candidateTransactions = Just [ candidateTransaction { author: p.us, intents: [ ChangeRule (Enact (controlRule p.us)) ] } ] }
      , noRound
          { candidateTransactions = Just exports
          , expect = Just (noExpect { accounts = balances [ Tuple p.usFirm (keyed [ p.usd := 15, p.technology := 8 ]), Tuple p.cn (keyed [ p.usd := 6, p.technology := 0 ]) ] })
          }
      ]
  }

-- | P-G: the Chinese firm offers 2 I and nobody buys. Nothing moves.
fixturePG :: Tuple String Json
fixturePG = fixture "P-G" spec
  { candidateTransactions = Just [ candidateTransaction { author: p.cnFirm, intents: [ trade { exchange: p.industryWorld, quantity: q (-2), limit: Just (q 2) } ] } ]
  , expect = Just (noExpect { accounts = balances [ Tuple p.cnFirm (keyed [ p.rmb := 10, p.industry := 2 ]) ], reserves = balances [ Tuple p.industryWorld (keyed [ p.rmb := 0, p.industry := 6 ]) ] })
  }

-- | P-C: the Chinese firm invests one unit, of 2 I; in round 2 it has the new unit of capacity, and the unit of
-- | competence the investment trained on top of its 3, which wears a tenth a round: 3 · 0.9 + 1, and that · 0.9.
fixturePC :: Tuple String Json
fixturePC = fixture "P-C" spec
  { cut = cut { actors = [ cnFirm ], resources = [ p.usd, p.rmb, p.industry, p.industryCapacity, p.industryCompetence ], exchanges = [], transformations = [ p.cnFirmInvestment ] }
  , rounds = Just
      [ noRound
          { candidateTransactions = Just [ candidateTransaction { author: p.cnFirm, intents: [ Transform { by: p.cnFirm, transformation: p.cnFirmInvestment, quantity: q 1 } ] } ]
          , expect = Just (noExpect { accounts = balances [ Tuple p.cnFirm (keyed [ p.rmb := 10, p.industry := 0 ]) ] })
          }
      , noRound { expect = Just (noExpect { accounts = balances [ Tuple p.cnFirm (keyed [ p.industryCapacity := 3, p.industryCompetence := "3.33" ]) ] }) }
      ]
  }

-- | P-R: the US transfers 10 USD to its private sector, which opened with 10, and in round 2 buys 2 T at home from
-- | the US state at 1.25.
fixturePR :: Tuple String Json
fixturePR = fixture "P-R" spec
  { cut = cut { actors = [ us, usPrivate ], resources = [ p.usd, p.technology, p.industry ], exchanges = [ p.technologyUs ] }
  , rounds = Just
      [ noRound { candidateTransactions = Just [ candidateTransaction { author: p.us, intents: [ Transfer { from: p.us, to: p.usPrivate, resource: p.usd, quantity: q 10 } ] } ] }
      , noRound
          { candidateTransactions = Just
              [ candidateTransaction { author: p.us, intents: [ trade { exchange: p.technologyUs, quantity: q (-2), limit: Just (q "1.25") } ] }
              , candidateTransaction { author: p.usPrivate, intents: [ trade { exchange: p.technologyUs, quantity: q 2, limit: Just (q "1.25") } ] }
              ]
          , expect = Just (noExpect { accounts = balances [ Tuple p.us (keyed [ p.usd := "104.5", p.technology := 0 ]), Tuple p.usPrivate (keyed [ p.usd := "17.5", p.technology := 2 ]) ] })
          }
      ]
  }

-- | P-X: the currency dealer. In round 1 the US firm sells 10 USD: the price, 1 RMB per USD, falls half a hundredth
-- | for each, so it averages 0.975 and the firm gets 9.75 RMB. In round 2 China buys 10 USD back from the position
-- | the sale left, and pays the same 9.75 RMB; the dealer is back where it started.
fixturePX :: Tuple String Json
fixturePX = fixture "P-X" spec
  { cut = cut { actors = [ usFirm, cn ], resources = [ p.usd, p.rmb, p.industry ], exchanges = [ p.currency ] }
  , rounds = Just
      [ noRound
          { candidateTransactions = Just [ candidateTransaction { author: p.usFirm, intents: [ trade { exchange: p.currency, quantity: q (-10), limit: Nothing } ] } ]
          , expect = Just (noExpect { accounts = balances [ Tuple p.usFirm (keyed [ p.usd := 5, p.rmb := "13.75" ]) ], reserves = balances [ Tuple p.currency (keyed [ p.usd := 160, p.rmb := "140.25" ]) ] })
          }
      , noRound
          { candidateTransactions = Just [ candidateTransaction { author: p.cn, intents: [ trade { exchange: p.currency, quantity: q 10, limit: Nothing } ] } ]
          , expect = Just (noExpect { accounts = balances [ Tuple p.cn (keyed [ p.usd := 16, p.rmb := "78.25" ]) ], reserves = balances [ Tuple p.currency (keyed [ p.usd := 150, p.rmb := 150 ]) ] })
          }
      ]
  }

-- | P-F: the US enacts its high asset fee, 0.05 USD per USD a Chinese lender Transfers to it, in round 1. In round
-- | 2 China lends the US 4 USD on the US bond and pays 0.2 USD of fee. The bond runs two rounds: nothing moves in
-- | round 3, and in round 4 the US repays 4.2 USD.
fixturePF :: Tuple String Json
fixturePF = fixture "P-F" spec
  { cut = cut { actors = [ us, cn ], resources = [ p.usd, p.industry ], exchanges = [] }
  , rounds = Just
      [ noRound { candidateTransactions = Just [ candidateTransaction { author: p.us, intents: [ ChangeRule (Enact fee) ] } ] }
      , noRound
          { candidateTransactions = Just [ (candidateTransaction { author: p.cn, intents: [ Transfer { from: p.cn, to: p.us, resource: p.usd, quantity: q 4 }, Enter { template: usBond, args: [ { name: "principal", value: q 4 } ], parties: [ { role: "lender", actor: p.cn }, { role: "borrower", actor: p.us } ] } ] }) # \s -> s { candidateTransaction = s.candidateTransaction { signers = [ p.us ] }, signatures = [ p.us ] } ]
          , expect = Just (noExpect { accounts = balances [ Tuple p.cn (keyed [ p.usd := "1.8" ]), Tuple p.us (keyed [ p.usd := "116.2" ]) ] })
          }
      , noRound { expect = Just (noExpect { accounts = balances [ Tuple p.cn (keyed [ p.usd := "1.8" ]), Tuple p.us (keyed [ p.usd := "116.2" ]) ] }) }
      , noRound { expect = Just (noExpect { accounts = balances [ Tuple p.cn (keyed [ p.usd := 6 ]), Tuple p.us (keyed [ p.usd := 112 ]) ] }) }
      ]
  }
  where
  fee = FlowRule { kind: p.usAssetFee, scope: { governor: p.us, counterparty: p.cn, direction: Inbound, over: [ p.usd ] }, effect: Levy { rate: q "0.05", basis: PerUnit, in: p.usd, to: LevyToGovernor, payer: PaidBySender } }

fixtures :: Array (Tuple String Json)
fixtures = [ fixturePA, fixturePH, fixturePB, fixturePG, fixturePC, fixturePR, fixturePX, fixturePF ]
