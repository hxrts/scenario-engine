-- | The premise format of a literal premise's rounds (docs/05_test_instance.md §21–§27): the
-- | Proposals a round receives (signed Intents), and the `expect` checks that round makes. A
-- | fixture file is a World with its rounds: a `rounds` list, or one round written at the top level.
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
  , roundRec
  , proposalRefShape
  , toLiteral
  , toRound
  , decodeLiteral
  , decodeExpect
  ) where

import Prelude

import Core.Draw (Seed(..))
import Core.Num (Q)
import Core.Shape (RecF, Shape, boolShape, contramapRec, embedded, fieldOf, intShape, keyedAlways, listOf, literal, object, optionalOf, premiseSum, qShape, stringShape, Format(..), decodeJsonWith)
import Core.Types (Id(..), IntentF, ProposalId, RoundIndex, SignedProposal, proposalId)
import Core.World (WorldState, seeded)
import Data.Array (concat, groupBy, mapWithIndex, sortWith)
import Data.Array.NonEmpty (toArray)
import Data.Either (Either(..), either)
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import Schema.Premise.Intent (intentShape)
import Schema.Premise.World (Keyed, WorldDoc, worldRec, worldToCore)
import Schema.Round (Delivery, Expect, ProcedureOutcome)
import Data.Map as Map
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError)

-- | A round as the engine plays it: its round index, its signed Proposals, and its checks.
type Round = { round :: RoundIndex, inputs :: Array SignedProposal, expect :: Expect }

-- | A literal premise, decoded: its name, its seeded World, and its rounds.
type Literal = { name :: String, world :: WorldState, rounds :: Array Round }

-- | A Proposal as written: its author, its intents, and what signed it.
type ProposalDoc =
  { author :: String
  , intents :: Array (IntentF Q)
  , signers :: Maybe (Array String)
  , signatures :: Maybe (Array String)
  , procedure :: Maybe String
  }

-- | A proposal named in an expectation: the author's name (the first Proposal), or the author with a
-- | position.
type ProposalRefDoc = Either String { author :: String, index :: Maybe Int }

-- | A procedure's outcome as written: the Proposal it names, flattened with its support and result.
type ProcedureDoc = { author :: String, index :: Maybe Int, support :: Q, executed :: Boolean }

-- | A delivery as written.
type DeliveryDoc = { entity :: String, resource :: String, quantity :: Q }

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
type FixtureDoc =
  { seed :: Int
  , world :: WorldDoc
  , rounds :: Maybe (Array RoundDoc)
  , inline :: RoundDoc
  }

-- Shapes --------------------------------------------------------------------------------------

deliveryShape :: Shape DeliveryDoc
deliveryShape = object "Delivery"
  ( mk <$> fieldOf "entity" "the Entity that receives" stringShape _.entity
      <*> fieldOf "resource" "the Resource" stringShape _.resource
      <*> fieldOf "quantity" "the quantity" qShape _.quantity
  )
  where
  mk e r q' = { entity: e, resource: r, quantity: q' }

-- | A proposal reference: a bare author name, or an object with an author and an optional index.
proposalRefShape :: Shape ProposalRefDoc
proposalRefShape = premiseSum "ProposalRef"
  [ literal stringShape (either Just (const Nothing)) Left
  , embedded refObject (either (const Nothing) Just) Right
  ]
  where
  refObject = object "ProposalAt"
    ( mk <$> fieldOf "author" "the author" stringShape _.author
        <*> fieldOf "index" "the position among the author's Proposals (default 0)" (optionalOf intShape) _.index
    )
    where
    mk a i = { author: a, index: i }

proposalShape :: Shape ProposalDoc
proposalShape = object "Proposal"
  ( mk <$> fieldOf "author" "the author" stringShape _.author
      <*> fieldOf "intents" "the Intents" (listOf intentShape) _.intents
      <*> fieldOf "signers" "the Entities that must sign it" (optionalOf (listOf stringShape)) _.signers
      <*> fieldOf "signatures" "the Entities that signed it" (optionalOf (listOf stringShape)) _.signatures
      <*> fieldOf "procedure" "the collective procedure it is put to" (optionalOf stringShape) _.procedure
  )
  where
  mk a i s g p = { author: a, intents: i, signers: s, signatures: g, procedure: p }

procedureShape :: Shape ProcedureDoc
procedureShape = object "ProcedureOutcome"
  ( mk <$> fieldOf "author" "the author of the Proposal" stringShape _.author
      <*> fieldOf "index" "the position among the author's Proposals (default 0)" (optionalOf intShape) _.index
      <*> fieldOf "support" "the weight behind it" qShape _.support
      <*> fieldOf "executed" "whether it ran" boolShape _.executed
  )
  where
  mk a i s e = { author: a, index: i, support: s, executed: e }

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

fixtureShape :: Shape FixtureDoc
fixtureShape = object "Fixture"
  ( mk <$> fieldOf "seed" "the game seed of the fixture's World" intShape _.seed
      <*> contramapRec _.world worldRec
      <*> fieldOf "rounds" "the rounds, in order" (optionalOf (listOf roundShape)) _.rounds
      <*> contramapRec _.inline roundRec
  )
  where
  mk s w r i = { seed: s, world: w, rounds: r, inline: i }

-- Conversions ---------------------------------------------------------------------------------

-- | A literal premise as the engine plays it: its name, its seeded World, and its rounds with their
-- | Proposals numbered. A fixture with no `rounds` list is one round, written at the top level.
toLiteral :: FixtureDoc -> Literal
toLiteral f =
  { name: fromMaybe "" f.world.name
  , world: seeded (Seed f.seed) (worldToCore f.world)
  , rounds: case f.rounds of
      Just rs -> mapWithIndex (\k r -> toRound (round + k) r) rs
      Nothing -> [ toRound round f.inline ]
  }
  where
  round = fromMaybe 1 f.world.round

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
  { proposal: { id: proposalId round author i, author, intents: p.intents, signers: map Id (fromMaybe [] p.signers), procedure: map Id p.procedure }
  , signatures: map Id (fromMaybe [] p.signatures)
  }
  where
  author = Id p.author

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
  nested kvs = Map.fromFoldable (map (\(Tuple k inner) -> Tuple (Id k) (Map.fromFoldable (map (\(Tuple k' v) -> Tuple (Id k') (Just v)) inner))) kvs)

refToId :: RoundIndex -> ProposalRefDoc -> ProposalId
refToId round = case _ of
  Left a -> proposalId round (Id a) 0
  Right r -> proposalId round (Id r.author) (fromMaybe 0 r.index)

deliveryToCore :: DeliveryDoc -> Delivery
deliveryToCore d = { entity: Id d.entity, resource: Id d.resource, quantity: d.quantity }

procedureToCore :: RoundIndex -> ProcedureDoc -> ProcedureOutcome
procedureToCore round p = { proposal: proposalId round (Id p.author) (fromMaybe 0 p.index), support: p.support, executed: p.executed }

-- | A literal premise, decoded from its JSON (the fixture file's document).
decodeLiteral :: Json -> Either JsonDecodeError Literal
decodeLiteral j = map toLiteral (decodeJsonWith Premise fixtureShape j)

-- | The checks of one round, decoded from their JSON (an `expect` object) for the round it checks.
decodeExpect :: RoundIndex -> Json -> Either JsonDecodeError Expect
decodeExpect round j = map (expectToCore round) (decodeJsonWith Premise expectShape j)
