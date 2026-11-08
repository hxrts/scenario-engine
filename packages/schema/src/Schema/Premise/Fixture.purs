-- | The premise format of a literal premise (docs/05_test_instance.md §21–§27), as three parts: the
-- | `premise` (its seed and World), the `bindings` (each round's Proposals, the answers a seat plays
-- | as a Script binding, in order from the World's round), and the `expectations` (the checks a round
-- | makes, each naming its round). A round with no expectation checks nothing.
-- | `toLiteral` produces the engine-level value (`Literal`, whose `Round`s carry the typed `Expect`
-- | of `Schema.Round`) with every default filled in.
module Schema.Premise.Fixture
  ( Literal
  , Round
  , FixtureDoc
  , ProposalDoc
  , ExpectDoc
  , DeliveryDoc
  , ProposalRefDoc
  , ProcedureDoc
  , RoundDoc
  , fixtureShape
  , proposalShape
  , expectShape
  , roundShape
  , toRound
  , decodeLiteral
  , PremiseDoc
  , BindingDoc
  , ExpectationDoc
  ) where

import Prelude

import Core.Draw (Seed(..))
import Core.Num (Q, qShape)
import Core.Shape (Format(..), RecF, Shape, boolShape, contramapRec, decodeJsonWith, embedded, fieldOf, intShape, keyedAlways, listOf, literal, object, optionalOf, premiseSum)
import Core.Types (CollectiveProcedureId, EntityId, Id, Intent, ProposalId, ResourceId, RoundIndex, SignedProposal, idShape, proposalId)
import Core.World (WorldState, seeded)
import Data.Array (concat, elem, find, groupBy, length, mapWithIndex, nub, range, sortWith)
import Data.Array.NonEmpty (toArray)
import Data.Either (Either(..), either)
import Data.Foldable (all)
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Schema.Premise.Intent (intentShape)
import Schema.Premise.World (Keyed, WorldDoc, keyedToMap, worldRec, worldToCore)
import Schema.Round (Delivery, Expect, ProcedureOutcome)
import Data.Map as Map
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError(..))

-- | A round as the engine plays it: its round index, its signed Proposals, and its checks.
type Round = { round :: RoundIndex, inputs :: Array SignedProposal, expect :: Expect }

-- | A literal premise, decoded: its name, its seeded World, and its rounds.
type Literal = { name :: String, world :: WorldState, rounds :: Array Round }

-- | A Proposal as written: its author, its intents, and what signed it.
type ProposalDoc =
  { author :: EntityId
  , intents :: Array Intent
  , signers :: Maybe (Array EntityId)
  , signatures :: Maybe (Array EntityId)
  , procedure :: Maybe CollectiveProcedureId
  }

-- | A proposal named in an expectation: the author's name (the first Proposal), or the author with a
-- | position.
type ProposalRefDoc = Either EntityId { author :: EntityId, index :: Maybe Int }

-- | A procedure's outcome as written: the Proposal it names (flattened with its support and result, so
-- | it is written as an object even when it names the first Proposal), and its support and result.
type ProcedureDoc = { proposal :: ProposalRefDoc, support :: Q, executed :: Boolean }

-- | A delivery as written.
type DeliveryDoc = { entity :: EntityId, resource :: ResourceId, quantity :: Q }

type ExpectDoc =
  { accounts :: Maybe (Keyed (Keyed Q))
  , reserves :: Maybe (Keyed (Keyed Q))
  , deliveries :: Maybe (Array DeliveryDoc)
  , rejected :: Maybe (Array ProposalRefDoc)
  , lapsed :: Maybe (Array ProposalRefDoc)
  , factors :: Maybe (Keyed (Keyed Q))
  , scheduled :: Maybe Int
  , procedures :: Maybe (Array ProcedureDoc)
  }

type RoundDoc = { proposals :: Maybe (Array ProposalDoc), expect :: Maybe ExpectDoc }

-- | A fixture as written: its World, and its rounds (a list, or the one round written at the top level).
type PremiseDoc = { seed :: Int, world :: WorldDoc }

-- | One round's Proposals, as the seat answers it (a Script binding's `propose` and `sign`, derived at load).
type BindingDoc = { proposals :: Array ProposalDoc }

-- | The checks a round makes, named by its round.
type ExpectationDoc = { round :: Int, expect :: ExpectDoc }

-- | A fixture as written: its premise, its bindings (one per round, in order), and its expectations.
type FixtureDoc =
  { premise :: PremiseDoc
  , bindings :: Array BindingDoc
  , expectations :: Array ExpectationDoc
  }

-- Shapes --------------------------------------------------------------------------------------

deliveryShape :: Shape DeliveryDoc
deliveryShape = object "Delivery"
  ( mk <$> fieldOf "entity" "the Entity that receives" idShape _.entity
      <*> fieldOf "resource" "the Resource" idShape _.resource
      <*> fieldOf "quantity" "the quantity" qShape _.quantity
  )
  where
  mk e r q' = { entity: e, resource: r, quantity: q' }

-- | A proposal reference: a bare author name, or an object with an author and an optional index.
proposalRefShape :: Shape ProposalRefDoc
proposalRefShape = premiseSum "ProposalRef"
  [ literal idShape (either Just (const Nothing)) Left
  , embedded refObject (either (const Nothing) Just) Right
  ]
  where
  refObject = object "ProposalAt" proposalAtRec

-- | A Proposal by its author and its position among the author's Proposals.
proposalAtRec :: RecF { author :: EntityId, index :: Maybe Int } { author :: EntityId, index :: Maybe Int }
proposalAtRec = mk <$> fieldOf "author" "the author" idShape _.author
  <*> fieldOf "index" "the position among the author's Proposals (default 0)" (optionalOf intShape) _.index
  where
  mk a i = { author: a, index: i }

proposalShape :: Shape ProposalDoc
proposalShape = object "Proposal"
  ( mk <$> fieldOf "author" "the author" idShape _.author
      <*> fieldOf "intents" "the Intents" (listOf intentShape) _.intents
      <*> fieldOf "signers" "the Entities that must sign it" (optionalOf (listOf idShape)) _.signers
      <*> fieldOf "signatures" "the Entities that signed it" (optionalOf (listOf idShape)) _.signatures
      <*> fieldOf "procedure" "the collective procedure it is put to" (optionalOf idShape) _.procedure
  )
  where
  mk a i s g p = { author: a, intents: i, signers: s, signatures: g, procedure: p }

procedureShape :: Shape ProcedureDoc
procedureShape = object "ProcedureOutcome"
  ( mk <$> contramapRec (proposalAt <<< _.proposal) proposalAtRec
      <*> fieldOf "support" "the weight behind it" qShape _.support
      <*> fieldOf "executed" "whether it ran" boolShape _.executed
  )
  where
  mk r s e = { proposal: Right r, support: s, executed: e }
  proposalAt = either { author: _, index: Nothing } identity

expectShape :: Shape ExpectDoc
expectShape = object "Expect"
  ( mk <$> fieldOf "accounts" "the balances, by Entity and Resource" (optionalOf (keyedAlways (keyedAlways qShape))) _.accounts
      <*> fieldOf "reserves" "the reserves, by Market and Resource" (optionalOf (keyedAlways (keyedAlways qShape))) _.reserves
      <*> fieldOf "deliveries" "the deliveries made" (optionalOf (listOf deliveryShape)) _.deliveries
      <*> fieldOf "rejected" "the Proposals rejected" (optionalOf (listOf proposalRefShape)) _.rejected
      <*> fieldOf "lapsed" "the Proposals that lapsed" (optionalOf (listOf proposalRefShape)) _.lapsed
      <*> fieldOf "factors" "the stored factors, by Entity and domain" (optionalOf (keyedAlways (keyedAlways qShape))) _.factors
      <*> fieldOf "scheduled" "the number of changes scheduled" (optionalOf intShape) _.scheduled
      <*> fieldOf "procedures" "the outcomes of the procedures" (optionalOf (listOf procedureShape)) _.procedures
  )
  where
  mk a r d rj l f s p = { accounts: a, reserves: r, deliveries: d, rejected: rj, lapsed: l, factors: f, scheduled: s, procedures: p }

roundShape :: Shape RoundDoc
roundShape = object "Round" roundRec

roundRec :: RecF RoundDoc RoundDoc
roundRec = mk <$> fieldOf "proposals" "the Proposals the round receives" (optionalOf (listOf proposalShape)) _.proposals
  <*> fieldOf "expect" "the checks the round makes" (optionalOf expectShape) _.expect
  where
  mk p e = { proposals: p, expect: e }

bindingShape :: Shape BindingDoc
bindingShape = object "Binding"
  (mk <$> fieldOf "proposals" "the Proposals the round receives, in order" (listOf proposalShape) _.proposals)
  where
  mk p = { proposals: p }

expectationShape :: Shape ExpectationDoc
expectationShape = object "Expectation"
  ( mk <$> fieldOf "round" "the round it checks" intShape _.round
      <*> fieldOf "expect" "the checks the round makes" expectShape _.expect
  )
  where
  mk r e = { round: r, expect: e }

premiseShape :: Shape PremiseDoc
premiseShape = object "Premise"
  ( mk <$> fieldOf "seed" "the game seed of the fixture's World" intShape _.seed
      <*> contramapRec _.world worldRec
  )
  where
  mk s w = { seed: s, world: w }

fixtureShape :: Shape FixtureDoc
fixtureShape = object "Fixture"
  ( mk <$> fieldOf "premise" "the seed and the World" premiseShape _.premise
      <*> fieldOf "bindings" "the rounds' Proposals, one binding per round, in order" (listOf bindingShape) _.bindings
      <*> fieldOf "expectations" "the checks, each naming its round" (listOf expectationShape) _.expectations
  )
  where
  mk p b e = { premise: p, bindings: b, expectations: e }

-- Conversions ---------------------------------------------------------------------------------

-- | A literal premise as the engine plays it: its name, its seeded World, and its rounds with their
-- | Proposals numbered. A fixture with no `rounds` list is one round, written at the top level.
toLiteral :: FixtureDoc -> Literal
toLiteral f =
  { name: fromMaybe "" f.premise.world.name
  , world: seeded (Seed f.premise.seed) (worldToCore f.premise.world)
  , rounds: mapWithIndex (\k b -> toRound (round + k) (roundOf (round + k) b)) f.bindings
  }
  where
  round = fromMaybe 1 f.premise.world.round
  roundOf r b = { proposals: Just b.proposals, expect: map _.expect (find (\x -> x.round == r) f.expectations) }

-- | One round: its Proposals in author order, each numbered within its author's group, and its expectation.
toRound :: RoundIndex -> RoundDoc -> Round
toRound round r = { round, inputs: concat (map signed groups), expect: fromMaybe noExpect (map (expectToCore round) r.expect) }
  where
  proposals = fromMaybe [] r.proposals
  groups = map toArray (groupBy (\a b -> a.author == b.author) (sortWith _.author proposals))
  signed group = mapWithIndex (\i p -> signedProposal round i p) group
  noExpect = { accounts: Map.empty, reserves: Map.empty, factors: Map.empty, deliveries: Nothing, rejected: Nothing, lapsed: Nothing, procedures: Nothing, scheduled: Nothing }

signedProposal :: RoundIndex -> Int -> ProposalDoc -> SignedProposal
signedProposal round i p =
  { proposal: { id: proposalId round p.author i, author: p.author, intents: p.intents, signers: fromMaybe [] p.signers, procedure: p.procedure }
  , signatures: fromMaybe [] p.signatures
  }

-- | An expectation as the checker reads it. A Proposal named in it takes its id in `round`. Every
-- | written value is a `Just` leaf: an absent key or field is not checked.
expectToCore :: RoundIndex -> ExpectDoc -> Expect
expectToCore round e =
  { accounts: maybe Map.empty nested e.accounts
  , reserves: maybe Map.empty nested e.reserves
  , factors: maybe Map.empty nested e.factors
  , deliveries: map (map deliveryToCore) e.deliveries
  , rejected: map (map (refToId round)) e.rejected
  , lapsed: map (map (refToId round)) e.lapsed
  , procedures: map (map (procedureToCore round)) e.procedures
  , scheduled: e.scheduled
  }
  where
  nested :: forall a b. Keyed (Keyed Q) -> Map (Id a) (Map (Id b) (Maybe Q))
  nested kvs = map (map Just <<< keyedToMap) (keyedToMap kvs)

refToId :: RoundIndex -> ProposalRefDoc -> ProposalId
refToId round = either (\a -> proposalId round a 0) (\r -> proposalId round r.author (fromMaybe 0 r.index))

deliveryToCore :: DeliveryDoc -> Delivery
deliveryToCore d = d

procedureToCore :: RoundIndex -> ProcedureDoc -> ProcedureOutcome
procedureToCore round p = { proposal: refToId round p.proposal, support: p.support, executed: p.executed }

-- | A literal premise, decoded from its JSON (the fixture file's document).
decodeLiteral :: Json -> Either JsonDecodeError Literal
decodeLiteral j = do
  f <- decodeJsonWith Premise fixtureShape j
  let
    round = fromMaybe 1 f.premise.world.round
    named = map _.round f.expectations
    bound = map (\k -> round + k) (range 0 (length f.bindings - 1))
  if all (\r -> elem r bound) named && length (nub named) == length named then pure (toLiteral f)
  else Left (TypeMismatch "an expectation names a round with no binding, or names one round twice")
