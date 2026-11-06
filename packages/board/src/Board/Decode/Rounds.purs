-- | Decoders for a round: its Proposals (signed Intents) and its `expect` checks. The
-- | expectation types are read by the fixture harness and by the capstone checks.
module Board.Decode.Rounds
  ( ProposalRef
  , Delivery
  , ProcedureExpect
  , Expect
  , Round
  , decodeRound
  , decodeExpect
  ) where

import Prelude

import Board.Decode.Intents (intent)
import Core.Num (Q)
import Core.Types (Id(..), SignedProposal, proposalId)
import Data.Argonaut.Core (Json, toString)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson, (.:), (.:?), (.!=))
import Data.Array (concat, groupBy, mapWithIndex, sortWith)
import Data.Array.NonEmpty (toArray)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)

-- | A proposal named in an expectation: its author and its position among that author's
-- | Proposals in the round (0 for the first). Written as the author's name or as
-- | `{ "author": ..., "index": ... }`.
type ProposalRef = { author :: String, index :: Int }

type Delivery = { entity :: String, resource :: String, quantity :: Q }

type ProcedureExpect = { proposal :: ProposalRef, support :: Q, executed :: Boolean }

-- | The checks a round makes. Every field is optional: only the fields given are checked, so
-- | `deliveries: []` asserts that nothing was delivered, and an absent field asserts nothing.
type Expect =
  { accounts :: Maybe (Object (Object Q))
  , reserves :: Maybe (Object (Object Q))
  , deliveries :: Maybe (Array Delivery)
  , rejected :: Maybe (Array ProposalRef)
  , lapsed :: Maybe (Array ProposalRef)
  , factors :: Maybe (Object (Object Q))
  , scheduled :: Maybe Int
  , procedures :: Maybe (Array ProcedureExpect)
  }

type Round = { inputs :: Array SignedProposal, expect :: Expect }

decodeRound :: Int -> Json -> Either JsonDecodeError Round
decodeRound round j = do
  o <- decodeJson j
  proposals <- o .:? "proposals" .!= []
  inputs <- concat <$> traverse signed (groupByAuthor proposals)
  expect <- o .:? "expect" >>= traverse decodeExpect
  pure { inputs, expect: fromMaybe noExpect expect }
  where
  groupByAuthor ps = map toArray (groupBy (\a b -> a.author == b.author) (sortWith _.author ps))
  signed group = traverse (\(Tuple i p) -> proposal round i p) (mapWithIndex Tuple group)
  noExpect = { accounts: Nothing, reserves: Nothing, deliveries: Nothing, rejected: Nothing, lapsed: Nothing, factors: Nothing, scheduled: Nothing, procedures: Nothing }

decodeExpect :: Json -> Either JsonDecodeError Expect
decodeExpect j = do
  o <- decodeJson j
  accounts <- o .:? "accounts"
  reserves <- o .:? "reserves"
  deliveries <- o .:? "deliveries" >>= traverse (traverse delivery)
  rejected <- o .:? "rejected" >>= traverse (traverse proposalRef)
  lapsed <- o .:? "lapsed" >>= traverse (traverse proposalRef)
  factors <- o .:? "factors"
  scheduled <- o .:? "scheduled"
  procedures <- o .:? "procedures" >>= traverse (traverse procedureExpect)
  pure { accounts, reserves, deliveries, rejected, lapsed, factors, scheduled, procedures }
  where
  delivery x = do
    d <- decodeJson x
    entity <- d .: "entity"
    res <- d .: "resource"
    quantity <- d .: "quantity"
    pure { entity, resource: res, quantity }
  procedureExpect x = do
    ref <- proposalRef x
    d <- decodeJson x
    support <- d .: "support"
    executed <- d .: "executed"
    pure { proposal: ref, support, executed }

-- | `"US"` is the first Proposal by US; `{ "author": "US", "index": 1 }` names the second.
proposalRef :: Json -> Either JsonDecodeError ProposalRef
proposalRef j = case toString j of
  Just author -> Right { author, index: 0 }
  Nothing -> do
    o <- decodeJson j
    author <- o .: "author"
    index <- o .:? "index" .!= 0
    pure { author, index }

proposal :: Int -> Int -> { author :: String, intents :: Array Json, signers :: Maybe (Array String), signatures :: Maybe (Array String), procedure :: Maybe String } -> Either JsonDecodeError SignedProposal
proposal round i p = do
  intents <- traverse intent p.intents
  let author = Id p.author
  pure
    { proposal: { id: proposalId round author i, author, intents, signers: map Id (unwrapArr p.signers), procedure: map Id p.procedure }
    , signatures: map Id (unwrapArr p.signatures)
    }
  where
  unwrapArr = case _ of
    Just xs -> xs
    Nothing -> []
