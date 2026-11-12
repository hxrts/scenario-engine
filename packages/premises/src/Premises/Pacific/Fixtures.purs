-- | Pacific's engine-level golden fixtures (docs/05_pacific.md §9.3): small worlds cut from Pacific with no
-- | circulation, whose decisions are literal candidate Transactions. They pin the matched-only Exchange and a
-- | ban, the US export tax, a home Exchange, Chinese credit and a release through `resolveRound` alone. Every expected value is a literal from docs/05.
module Premises.Pacific.Fixtures (fixtures) where

import Prelude (negate, (#))

import Premises.Kit.Prelude

import Assembly.Composition (CompositionParts)
import Foreign.Object as Object
import Laws.Types (IntentF(..), RuleChangeF(..), SignedCandidateTransaction)
import Premises.Kit.Fixtures (Cut, FixtureSpec, KeptActor)
import Premises.Kit.Fixtures as Kit
import Premises.Pacific (compositionWith, defaultDials, gameSeed, pacific)
import Premises.Pacific.Names (p)
import Premises.Pacific.Strategies (banRule)
import Schema.Build.Fixture (noRound)
import Schema.Build.World (emptyCirculation)
import Schema.Build.Base (mayEnact, mayTransform)

base :: CompositionParts
base = { composition: compositionWith defaultDials, config: (pacific defaultDials).config { preset = Nothing }, presets: Object.empty }

fixture :: String -> FixtureSpec -> Tuple String Json
fixture = Kit.fixture { seed: gameSeed, base }

-- | Each Actor a fixture keeps: its accounts, and exactly the Capabilities it uses.
us :: KeptActor
us = { key: p.us, accounts: Just [ p.money, p.technology, p.industry ], capabilities: Just [ mayTrade p.technologyWorld, mayTrade p.industryWorld, mayTrade p.technologyUs ] }

usFirm :: KeptActor
usFirm = { key: p.usFirm, accounts: Just [ p.money, p.technology ], capabilities: Just [ mayTrade p.technologyWorld, mayTrade p.technologyUs ] }

cn :: KeptActor
cn = { key: p.cn, accounts: Just [ p.money, p.technology, p.industry, p.credit ], capabilities: Just [ mayTrade p.technologyWorld, mayTrade p.industryWorld, mayTrade p.industryCn, mayTransfer p.money, mayEnact p.cnImportBan ] }

cnFirm :: KeptActor
cnFirm = { key: p.cnFirm, accounts: Just [ p.money, p.industry, p.industryCapacity ], capabilities: Just [ mayTrade p.industryWorld, mayTransform p.cnFirmInvestment ] }

cnPrivate :: KeptActor
cnPrivate = { key: p.cnPrivate, accounts: Just [ p.money, p.industry ], capabilities: Just [ mayTrade p.industryCn ] }

cut :: Cut
cut = { actors: [ us, usFirm, cn, cnFirm ], resources: [ p.money, p.technology, p.industry ], exchanges: [ p.technologyWorld, p.industryWorld, p.technologyUs ], transformations: [], circulation: emptyCirculation, patches: [] }

spec :: FixtureSpec
spec = { cut, candidateTransactions: Nothing, expect: Nothing, rounds: Nothing }

-- | The US firm sells 4 T on the world Exchange and China buys them.
exports :: Array SignedCandidateTransaction
exports =
  [ candidateTransaction { author: p.usFirm, intents: [ trade { exchange: p.technologyWorld, quantity: q (-4), limit: Just (q 1) } ] }
  , candidateTransaction { author: p.cn, intents: [ trade { exchange: p.technologyWorld, quantity: q 4, limit: Just (q 1) } ] }
  ]

-- | P-A: the US firm exports 4 T to China at 1 and pays the export tax, 0.5 a unit, to the US state.
fixturePA :: Tuple String Json
fixturePA = fixture "P-A" spec
  { candidateTransactions = Just exports
  , expect = Just (noExpect { accounts = balances [ Tuple p.usFirm (keyed [ p.money := 52, p.technology := 4 ]), Tuple p.cn (keyed [ p.money := 76, p.technology := 4 ]), Tuple p.us (keyed [ p.money := 122 ]) ], reserves = balances [ Tuple p.technologyWorld (keyed [ p.money := 0, p.technology := 0 ]) ] })
  }

-- | P-H: the US firm sells 2 T at home to its own state, at the home price of 1.25, and pays no tax.
fixturePH :: Tuple String Json
fixturePH = fixture "P-H" spec
  { candidateTransactions = Just
      [ candidateTransaction { author: p.usFirm, intents: [ trade { exchange: p.technologyUs, quantity: q (-2), limit: Just (q "1.25") } ] }
      , candidateTransaction { author: p.us, intents: [ trade { exchange: p.technologyUs, quantity: q 2, limit: Just (q "1.25") } ] }
      ]
  , expect = Just (noExpect { accounts = balances [ Tuple p.usFirm (keyed [ p.money := "52.5", p.technology := 6 ]), Tuple p.us (keyed [ p.money := "117.5", p.technology := 4 ]) ] })
  }

-- | P-B: China enacts its ban on US technology in round 1; in round 2 the same exports move nothing, so the firm
-- | keeps its T.
fixturePB :: Tuple String Json
fixturePB = fixture "P-B" spec
  { rounds = Just
      [ noRound { candidateTransactions = Just [ candidateTransaction { author: p.cn, intents: [ ChangeRule (Enact (banRule p.cn)) ] } ] }
      , noRound
          { candidateTransactions = Just exports
          , expect = Just (noExpect { accounts = balances [ Tuple p.usFirm (keyed [ p.money := 50, p.technology := 8 ]), Tuple p.cn (keyed [ p.money := 80, p.technology := 0 ]) ] })
          }
      ]
  }

-- | P-G: the Chinese firm offers 2 I and nobody buys. Nothing moves.
fixturePG :: Tuple String Json
fixturePG = fixture "P-G" spec
  { candidateTransactions = Just [ candidateTransaction { author: p.cnFirm, intents: [ trade { exchange: p.industryWorld, quantity: q (-2), limit: Just (q 2) } ] } ]
  , expect = Just (noExpect { accounts = balances [ Tuple p.cnFirm (keyed [ p.money := 50, p.industry := 2 ]) ], reserves = balances [ Tuple p.industryWorld (keyed [ p.money := 0, p.industry := 0 ]) ] })
  }

-- | P-C: the Chinese firm invests 2 I on state credit, an order China signs; the new capacity is there in round 2.
fixturePC :: Tuple String Json
fixturePC = fixture "P-C" spec
  { cut = cut { actors = [ cn, cnFirm ], resources = [ p.money, p.industry, p.industryCapacity, p.credit ], exchanges = [], transformations = [ p.cnFirmInvestment ] }
  , rounds = Just
      [ noRound { candidateTransactions = Just [ (candidateTransaction { author: p.cnFirm, intents: [ Transform { by: p.cnFirm, transformation: p.cnFirmInvestment, quantity: q 1 } ] }) # \s -> s { candidateTransaction = s.candidateTransaction { signers = [ p.cn ] }, signatures = [ p.cn ] } ] }
      , noRound { expect = Just (noExpect { accounts = balances [ Tuple p.cnFirm (keyed [ p.industry := 0, p.industryCapacity := 3 ]) ] }) }
      ]
  }

-- | P-R: China releases 10 M to its private sector, which in round 2 buys 2 I at home from the Chinese state at 2.5.
fixturePR :: Tuple String Json
fixturePR = fixture "P-R" spec
  { cut = cut { actors = [ cn, cnPrivate ], resources = [ p.money, p.industry ], exchanges = [ p.industryCn ] }
  , rounds = Just
      [ noRound { candidateTransactions = Just [ candidateTransaction { author: p.cn, intents: [ Transfer { from: p.cn, to: p.cnPrivate, resource: p.money, quantity: q 10 } ] } ] }
      , noRound
          { candidateTransactions = Just
              [ candidateTransaction { author: p.cn, intents: [ trade { exchange: p.industryCn, quantity: q (-2), limit: Just (q "2.5") } ] }
              , candidateTransaction { author: p.cnPrivate, intents: [ trade { exchange: p.industryCn, quantity: q 2, limit: Just (q "2.5") } ] }
              ]
          , expect = Just (noExpect { accounts = balances [ Tuple p.cn (keyed [ p.money := 75, p.industry := 6 ]), Tuple p.cnPrivate (keyed [ p.money := 5, p.industry := 2 ]) ] })
          }
      ]
  }

fixtures :: Array (Tuple String Json)
fixtures = [ fixturePA, fixturePH, fixturePB, fixturePG, fixturePC, fixturePR ]
