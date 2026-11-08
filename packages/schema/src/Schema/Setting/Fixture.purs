-- | The setting format of a literal setting (docs/05_test_instance.md §21–§27), as three parts: the
-- | `setting` (its seed, name and World), the `bindings` (each round's Proposals, the answers a seat
-- | plays as a Script binding, in order from the World's round), and the `expectations` (the checks a
-- | round makes, each naming its round). A round with no expectation checks nothing. The file's
-- | Proposals are `SignedProposal`s with no id yet: a Proposal's id depends on its round and position,
-- | and `toRound` numbers them. `decodeLiteral` yields the engine-level `Literal`.
module Schema.Setting.Fixture
  ( Literal
  , Round
  , Fixture
  , Binding
  , RoundSpec
  , unnumbered
  , fixtureShape
  , proposalShape
  , expectShape
  , roundShape
  , toRound
  , decodeLiteral
  ) where

import Prelude

import Core.Draw (Seed(..))
import Core.Num (Q, qShape)
import Core.Shape (Format(..), RecF, Shape, arrayOf, boolShape, contramapRec, decodeJsonWith, defaultsTo, embedded, emptyByDefault, fieldOf, intShape, iso, listOf, literal, object, optionalOf, settingSum, stringShape)
import Core.Types (Id(..), ProposalId, RoundIndex, SignedProposal, idMap, idShape, proposalId)
import Core.World (UnseededWorld, WorldState, seeded)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError(..))
import Data.Array (concat, elem, find, groupBy, length, mapWithIndex, nub, range, sortWith)
import Data.Array.NonEmpty (toArray)
import Data.Either (Either(..))
import Data.Foldable (all)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Schema.Setting.Intent (intentShape)
import Schema.Setting.World (SettingWorld, worldRec)
import Schema.Recurring as Recurring
import Schema.Round (Authored, Delivery, Expect, ProcedureOf, ProposalRef, atRound, noExpect)

-- | A round as the engine plays it: its round index, its signed Proposals, and its checks.
type Round = { round :: RoundIndex, inputs :: Array SignedProposal, expect :: Expect }

-- | A literal setting, decoded: its name, its seeded World, and its rounds.
type Literal = { name :: String, world :: WorldState, rounds :: Array Round }

-- | One round's Proposals, as the seat answers it (a Script binding's `propose` and `sign`, derived at load).
type Binding = { proposals :: Array SignedProposal }

-- | The fixture file: the setting (seed, name, World), the bindings, and the expectations.
type Fixture =
  { seed :: Int
  , name :: String
  , world :: UnseededWorld
  , settingAuthored :: Recurring.Authored -- what only the document holds
  , bindings :: Array Binding
  , expectations :: Array { round :: Int, expect :: Authored }
  }

-- | One round as an author gives it: its Proposals and its checks.
type RoundSpec = { proposals :: Maybe (Array SignedProposal), expect :: Maybe Authored }

-- | The id of a Proposal not yet numbered.
unnumbered :: ProposalId
unnumbered = Id ""

-- Shapes --------------------------------------------------------------------------------------

deliveryShape :: Shape Delivery
deliveryShape = object "Delivery"
  ( mk <$> fieldOf "entity" "the Entity that receives" idShape _.entity
      <*> fieldOf "resource" "the Resource" idShape _.resource
      <*> fieldOf "quantity" "the quantity" qShape _.quantity
  )
  where
  mk e r q' = { entity: e, resource: r, quantity: q' }

-- | A Proposal by its author and its position among the author's Proposals; the position is omitted when 0.
proposalAtRec :: RecF ProposalRef ProposalRef
proposalAtRec = mk <$> fieldOf "author" "the author" idShape _.author
  <*> fieldOf "index" "the position among the author's Proposals (default 0)" (defaultsTo 0 intShape) _.index
  where
  mk a i = { author: a, index: i }

-- | A proposal reference: a bare author name (the first Proposal), or an object with an author and an optional index.
proposalRefShape :: Shape ProposalRef
proposalRefShape = settingSum "ProposalRef"
  [ literal idShape (\r -> if r.index == 0 then Just r.author else Nothing) (\a -> { author: a, index: 0 })
  , embedded (object "ProposalAt" proposalAtRec) Just identity
  ]

-- | A fixture Proposal, with no id: `toRound` numbers it. Signers and signatures default to none.
proposalShape :: Shape SignedProposal
proposalShape = object "Proposal"
  ( mk <$> fieldOf "author" "the author" idShape _.proposal.author
      <*> fieldOf "intents" "the Intents" (listOf intentShape) _.proposal.intents
      <*> fieldOf "signers" "the Entities that must sign it" (arrayOf idShape) _.proposal.signers
      <*> fieldOf "signatures" "the Entities that signed it" (arrayOf idShape) _.signatures
      <*> fieldOf "procedure" "the collective procedure it is put to" (optionalOf idShape) _.proposal.procedure
  )
  where
  mk a i s g p = { proposal: { id: unnumbered, author: a, intents: i, signers: s, procedure: p }, signatures: g }

-- | A procedure's outcome: the Proposal it names is flattened into the object, so it is written as an
-- | object even when it names the first Proposal.
procedureShape :: Shape (ProcedureOf ProposalRef)
procedureShape = object "ProcedureOutcome"
  ( mk <$> contramapRec _.proposal proposalAtRec
      <*> fieldOf "support" "the weight behind it" qShape _.support
      <*> fieldOf "executed" "whether it ran" boolShape _.executed
  )
  where
  mk r s e = { proposal: r, support: s, executed: e }

-- | A two-level balance map, written sparsely: only a leaf asserted is written.
balances :: forall a b. Shape (Map (Id a) (Map (Id b) (Maybe Q)))
balances = emptyByDefault (idMap (iso (map Just) (Map.mapMaybe identity) (idMap qShape)))

expectShape :: Shape Authored
expectShape = object "Expect"
  ( mk <$> fieldOf "accounts" "the balances, by Entity and Resource" balances _.accounts
      <*> fieldOf "reserves" "the reserves, by Market and Resource" balances _.reserves
      <*> fieldOf "deliveries" "the deliveries made" (optionalOf (listOf deliveryShape)) _.deliveries
      <*> fieldOf "rejected" "the Proposals rejected" (optionalOf (listOf proposalRefShape)) _.rejected
      <*> fieldOf "lapsed" "the Proposals that lapsed" (optionalOf (listOf proposalRefShape)) _.lapsed
      <*> fieldOf "factors" "the stored factors, by Entity and domain" balances _.factors
      <*> fieldOf "scheduled" "the number of changes scheduled" (optionalOf intShape) _.scheduled
      <*> fieldOf "procedures" "the outcomes of the procedures" (optionalOf (listOf procedureShape)) _.procedures
  )
  where
  mk a r d rj l f s p = { accounts: a, reserves: r, deliveries: d, rejected: rj, lapsed: l, factors: f, scheduled: s, procedures: p }

roundShape :: Shape RoundSpec
roundShape = object "Round"
  ( mk <$> fieldOf "proposals" "the Proposals the round receives" (optionalOf (listOf proposalShape)) _.proposals
      <*> fieldOf "expect" "the checks the round makes" (optionalOf expectShape) _.expect
  )
  where
  mk p e = { proposals: p, expect: e }

bindingShape :: Shape Binding
bindingShape = object "Binding"
  (mk <$> fieldOf "proposals" "the Proposals the round receives, in order" (listOf proposalShape) _.proposals)
  where
  mk p = { proposals: p }

expectationShape :: Shape { round :: Int, expect :: Authored }
expectationShape = object "Expectation"
  ( mk <$> fieldOf "round" "the round it checks" intShape _.round
      <*> fieldOf "expect" "the checks the round makes" expectShape _.expect
  )
  where
  mk r e = { round: r, expect: e }

settingShape :: Shape { seed :: Int, name :: String, world :: SettingWorld }
settingShape = object "Setting"
  ( mk <$> fieldOf "seed" "the game seed of the fixture's World" intShape _.seed
      <*> fieldOf "name" "the setting's name" (defaultsTo "" stringShape) _.name
      <*> contramapRec _.world worldRec
  )
  where
  mk s n w = { seed: s, name: n, world: w }

fixtureShape :: Shape Fixture
fixtureShape = object "Fixture"
  ( mk <$> fieldOf "setting" "the seed and the World" settingShape (\f -> { seed: f.seed, name: f.name, world: { world: f.world, authored: f.settingAuthored } })
      <*> fieldOf "bindings" "the rounds' Proposals, one binding per round, in order" (listOf bindingShape) _.bindings
      <*> fieldOf "expectations" "the checks, each naming its round" (listOf expectationShape) _.expectations
  )
  where
  mk p b e = { seed: p.seed, name: p.name, world: p.world.world, settingAuthored: p.world.authored, bindings: b, expectations: e }

-- Reading -------------------------------------------------------------------------------------

-- | A literal setting as the engine plays it: its name, its seeded World, and its rounds with their
-- | Proposals numbered.
toLiteral :: Fixture -> Literal
toLiteral f =
  { name: f.name
  , world: seeded (Seed f.seed) f.world
  , rounds: mapWithIndex (\k b -> toRound (f.world.round + k) (roundOf (f.world.round + k) b)) f.bindings
  }
  where
  roundOf r b = { proposals: Just b.proposals, expect: map _.expect (find (\x -> x.round == r) f.expectations) }

-- | One round: its Proposals in author order, each numbered within its author's group, and its expectation.
toRound :: RoundIndex -> RoundSpec -> Round
toRound round r = { round, inputs: concat (map (mapWithIndex numbered) groups), expect: atRound round (fromMaybe noExpect r.expect) }
  where
  proposals = fromMaybe [] r.proposals
  groups = map toArray (groupBy (\a b -> a.proposal.author == b.proposal.author) (sortWith _.proposal.author proposals))
  numbered i p = p { proposal { id = proposalId round p.proposal.author i } }

-- | A literal setting, decoded from its JSON (the fixture file's document).
decodeLiteral :: Json -> Either JsonDecodeError Literal
decodeLiteral j = do
  f <- decodeJsonWith Authored fixtureShape j
  let
    named = map _.round f.expectations
    bound = map (\k -> f.world.round + k) (range 0 (length f.bindings - 1))
  if all (\r -> elem r bound) named && length (nub named) == length named then pure (toLiteral f)
  else Left (TypeMismatch "an expectation names a round with no binding, or names one round twice")
