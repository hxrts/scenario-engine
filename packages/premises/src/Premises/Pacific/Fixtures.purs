-- | Pacific's engine-level golden fixtures (docs/05_pacific.md §8): small worlds cut from Pacific with no
-- | circulation, whose decisions are literal candidate Transactions. They pin the matched-only Exchange and a
-- | ban on a matched pair through `resolveRound` alone. Every expected value is a literal from docs/05.
module Premises.Pacific.Fixtures (fixtures) where

import Prelude (map, negate, (<>), (==))

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
import Schema.Build.Base (mayEnact)

base :: CompositionParts
base = { composition: compositionWith defaultDials, config: (pacific defaultDials).config { preset = Nothing }, presets: Object.empty }

fixture :: String -> FixtureSpec -> Tuple String Json
fixture = Kit.fixture { seed: gameSeed, base }

-- | Both countries with their Money and goods, trade capabilities, and the US ban's enactment.
countries :: Array KeptActor
countries = map (\a -> { key: a, accounts: Just [ p.money, p.technology, p.industry ], capabilities: Just (capabilities a) }) [ p.us, p.cn ]
  where
  trading = [ mayTrade p.technologyMarket, mayTrade p.industryMarket, mayTransfer p.technology, mayTransfer p.industry ]
  capabilities a = if a == p.us then trading <> [ mayEnact p.usTechnologyBan ] else trading

cut :: Cut
cut = { actors: countries, resources: [ p.money, p.technology, p.industry ], exchanges: [ p.technologyMarket, p.industryMarket ], transformations: [], circulation: emptyCirculation, patches: [] }

spec :: FixtureSpec
spec = { cut, candidateTransactions: Nothing, expect: Nothing, rounds: Nothing }

-- | Each country's round-1 commerce: the US sells 4 T and buys 6 I, China sells 6 I and buys 4 T, at the prices.
commerce :: Array SignedCandidateTransaction
commerce =
  [ candidateTransaction { author: p.us, intents: [ trade { exchange: p.technologyMarket, quantity: q (-4), limit: Just (q 1) }, trade { exchange: p.industryMarket, quantity: q 6, limit: Just (q 2) } ] }
  , candidateTransaction { author: p.cn, intents: [ trade { exchange: p.industryMarket, quantity: q (-6), limit: Just (q 2) }, trade { exchange: p.technologyMarket, quantity: q 4, limit: Just (q 1) } ] }
  ]

-- | P-A: free trade, one round. The matched Exchanges move 4 T and 6 I and hold nothing.
fixturePA :: Tuple String Json
fixturePA = fixture "P-A" spec
  { candidateTransactions = Just commerce
  , expect = Just (noExpect { accounts = balances [ Tuple p.us (keyed [ p.money := 92, p.technology := 6, p.industry := 6 ]), Tuple p.cn (keyed [ p.money := 108, p.technology := 4, p.industry := 4 ]) ], reserves = balances [ Tuple p.technologyMarket (keyed [ p.money := 0, p.technology := 0 ]), Tuple p.industryMarket (keyed [ p.money := 0, p.industry := 0 ]) ] })
  }

-- | P-B: the US enacts its ban in round 1; in round 2 the same commerce moves no T and no Money for it, and I
-- | still crosses.
fixturePB :: Tuple String Json
fixturePB = fixture "P-B" spec
  { rounds = Just
      [ noRound { candidateTransactions = Just [ candidateTransaction { author: p.us, intents: [ ChangeRule (Enact (banRule p.us)) ] } ] }
      , noRound
          { candidateTransactions = Just commerce
          , expect = Just (noExpect { accounts = balances [ Tuple p.us (keyed [ p.money := 88, p.technology := 10, p.industry := 6 ]), Tuple p.cn (keyed [ p.money := 112, p.technology := 0, p.industry := 4 ]) ] })
          }
      ]
  }

-- | P-G: China offers 6 I and nobody buys. Nothing moves; China keeps its I and is paid nothing.
fixturePG :: Tuple String Json
fixturePG = fixture "P-G" spec
  { candidateTransactions = Just [ candidateTransaction { author: p.cn, intents: [ trade { exchange: p.industryMarket, quantity: q (-6), limit: Just (q 2) } ] } ]
  , expect = Just (noExpect { accounts = balances [ Tuple p.cn (keyed [ p.money := 100, p.industry := 10 ]) ], reserves = balances [ Tuple p.industryMarket (keyed [ p.money := 0, p.industry := 0 ]) ] })
  }

-- | P-R: China releases 10 M to its private sector in round 1, and in round 2 the private sector buys 2 I from
-- | the Chinese state on the matched market. The release is a transfer and the purchase a pair, so Money and
-- | goods are conserved and the Exchange holds nothing.
fixturePR :: Tuple String Json
fixturePR = fixture "P-R" spec
  { cut = cut { actors = [ { key: p.cn, accounts: Just [ p.money, p.industry ], capabilities: Just [ mayTrade p.industryMarket, mayTransfer p.money, mayTransfer p.industry ] }, { key: p.cnPrivate, accounts: Just [ p.money, p.industry ], capabilities: Just [ mayTrade p.industryMarket ] } ], exchanges = [ p.industryMarket ] }
  , rounds = Just
      [ noRound { candidateTransactions = Just [ candidateTransaction { author: p.cn, intents: [ Transfer { from: p.cn, to: p.cnPrivate, resource: p.money, quantity: q 10 } ] } ] }
      , noRound
          { candidateTransactions = Just
              [ candidateTransaction { author: p.cn, intents: [ trade { exchange: p.industryMarket, quantity: q (-2), limit: Just (q 2) } ] }
              , candidateTransaction { author: p.cnPrivate, intents: [ trade { exchange: p.industryMarket, quantity: q 2, limit: Just (q 2) } ] }
              ]
          , expect = Just (noExpect { accounts = balances [ Tuple p.cn (keyed [ p.money := 94, p.industry := 8 ]), Tuple p.cnPrivate (keyed [ p.money := 6, p.industry := 2 ]) ], reserves = balances [ Tuple p.industryMarket (keyed [ p.money := 0, p.industry := 0 ]) ] })
          }
      ]
  }

fixtures :: Array (Tuple String Json)
fixtures = [ fixturePA, fixturePB, fixturePG, fixturePR ]
