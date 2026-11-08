-- | Generic premise definitions (docs/03): the pieces a setting is written with. Nothing here
-- | names a premise: every Resource, name, table, and threshold is given by the caller.
module Premises.Kit
  ( Cover(..)
  , SeatSpec
  , seat
  , HoldingTable
  , Holding
  , holdingOf
  , accountsIn
  , capacitiesIn
  , pickAccountsIn
  , capacityHoldersIn
  , MarketTerms
  , market
  , paid
  , banded
  , SupplierNames
  , Supplier
  , supplier
  , supplierOffers
  , LevyKit
  , BlockKit
  , ReduceKit
  , levyRule
  , blockRule
  , reduceRule
  , aggregateEntity
  , flowsOf
  , (~>)
  , requirementsOf
  , Premise
  , PremiseParts
  , emptyParts
  , premise
  , assemble
  , FixtureContext
  , FixtureSpec
  , fixture
  ) where

import Prelude

import Core.Num (Q)
import Core.Shape (encodeWith, Format(..))
import Core.Strategy (Clause, Expr, IntentTemplate, Strategy, selfName)
import Core.Types (Capability, Capacities, CostFunction, Direction(..), Entity, EntityId, EntityKind, FlowEffect(..), Holdings, Id(..), Intent, IntentF(..), LevyBasis(..), LevyRecipient(..), Market, MarketId, Payment, Representation, ResourceDefinition, ResourceId, RuleChangeF(..), RuleF(..), RuleKindDecl, RuleKindId, SignedProposal, Transformation, TransformationId)
import Core.World (FlowGate)
import Data.Argonaut.Core (Json)
import Data.Array (filter, find, mapWithIndex)
import Data.Array as Array
import Data.Foldable (any)
import Data.Map (Map)
import Data.Map as Map
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set as Set
import Data.Tuple (Tuple(..), fst)
import Partial.Unsafe (unsafeCrashWith)
import Schema.Build.Base (coverage, institutional, keyed, mayEnact, (:=))
import Schema.Build.Decision (eqQ, ge, gt, le, lit, max', named, not', orderAt, param, priceAt, roundQ, rule, selfEnact, subQ)
import Schema.Build.World (definition, emptyCirculation, emptyWorld, noEntity, noMarket, objective)
import Schema.Gameboard (ActorDefinition, Criterion, Fallback)
import Schema.Premise.Fixture (Fixture, RoundSpec, fixtureShape)
import Schema.Premise.World (rulesFromList)
import Schema.Recurring (Circulation, Decay, Flow, Requirement, compileIn)
import Schema.Round (Authored)

-- Seats -----------------------------------------------------------------------------------------

-- | How a seat is driven when it has no player, and what covers it.
data Cover
  = Unassigned -- no NPC Policy and no fallback
  | NpcOnly String -- an in-world Policy, with no fallback
  | Governed String -- an in-world Policy, which is also the seat's fallback
  | Covered String -- the Aggregate that covers the seat's accounts when it has no player

-- | One seat: its accounts, capabilities and capacities, its objective, its description and
-- | dependency, and how it is covered.
type SeatSpec =
  { capabilities :: Array Capability
  , required :: Boolean
  , cover :: Cover
  , goal :: String
  , criteria :: Array Criterion
  , description :: String
  , dependency :: String
  }

-- | A seat Actor named by its key in the holding table: an Entity whose definition is a seat, with
-- | its accounts and capacities from the table.
seat :: HoldingTable -> String -> SeatSpec -> Tuple Entity ActorDefinition
seat table key s = Tuple
  ( noEntity
      { accounts = accountsIn table key
      , capabilities = s.capabilities
      , capacities = capacitiesIn table key
      }
  )
  (definitionOf s)

-- | An Aggregate named by its key in the holding table.
aggregateEntity :: HoldingTable -> String -> { capabilities :: Array Capability, kind :: EntityKind } -> Entity
aggregateEntity table key a = (noEntity { accounts = accountsIn table key, capabilities = a.capabilities, capacities = capacitiesIn table key }) { kind = a.kind }

-- | Recurring flows of one Resource: `money [ a ~> b := 15 ]`.
flowsOf :: ResourceId -> Array (Tuple (Tuple String String) Q) -> Array Flow
flowsOf resource = map \(Tuple (Tuple from to) amount) -> { from: Id from, to: Id to, resource, amount }

infix 7 Tuple as ~>

-- | Requirements sharing a Resource and penalty: those with amounts, then those that need only what
-- | they have demanded.
requirementsOf :: { resource :: ResourceId, penalty :: Q } -> Array (Tuple String Q) -> Array String -> Array Requirement
requirementsOf d amounts open = map (\(Tuple e a) -> need e (Just a)) amounts <> map (\e -> need e Nothing) open
  where
  need e amount = { entity: Id e, resource: d.resource, amount, penalty: d.penalty }

-- | The seat's definition. Its fallback is written only when the seat has one.
definitionOf :: SeatSpec -> ActorDefinition
definitionOf s = definition
  { required: s.required
  , seat: true
  , npc: maybe [] pure (npcOf s.cover)
  , fallback: fallbackOf s.cover
  , objective: objective { description: s.goal, criteria: s.criteria }
  , description: s.description
  , dependency: s.dependency
  }

npcOf :: Cover -> Maybe String
npcOf = case _ of
  NpcOnly p -> Just p
  Governed p -> Just p
  _ -> Nothing

fallbackOf :: Cover -> Maybe Fallback
fallbackOf = case _ of
  Governed p -> Just (institutional p)
  Covered a -> Just (coverage a)
  _ -> Nothing

-- Holdings --------------------------------------------------------------------------------------

-- | One Entity's holdings: its accounts and its capacities, in declaration order.
type Holding = { accounts :: Array (Tuple String Q), capacities :: Array (Tuple String Q) }

-- | Every Entity's holding, in catalog order.
type HoldingTable = Array (Tuple String Holding)

-- | The holding of a named Entity. A table covers its premise's catalog, so a missing name is a
-- | programming error.
holdingOf :: HoldingTable -> String -> Holding
holdingOf table name = case find (\(Tuple n _) -> n == name) table of
  Just (Tuple _ h) -> h
  Nothing -> unsafeCrashWith ("no holding for Entity " <> name)

-- | An Entity's accounts, as the table declares them.
accountsIn :: HoldingTable -> String -> Holdings
accountsIn table name = keyed (holdingOf table name).accounts

-- | An Entity's capacities, as the table declares them.
capacitiesIn :: HoldingTable -> String -> Capacities
capacitiesIn table name = keyed (holdingOf table name).capacities

-- | Some of an Entity's accounts, in the order given. A name the Entity does not hold is a
-- | programming error.
pickAccountsIn :: HoldingTable -> String -> Array String -> Holdings
pickAccountsIn table name wanted = keyed (map pick wanted)
  where
  pick k = case find (\(Tuple n _) -> n == k) (holdingOf table name).accounts of
    Just kv -> kv
    Nothing -> unsafeCrashWith ("Entity " <> name <> " holds no account " <> k)

-- | The Entities that hold a capacity, in table order.
capacityHoldersIn :: HoldingTable -> String -> Array String
capacityHoldersIn table capacity = map fst (filter holds table)
  where
  holds (Tuple _ h) = any (\(Tuple n _) -> n == capacity) h.capacities

-- Markets and payments --------------------------------------------------------------------------

-- | What a Market trades and prices: its base and quote Resources, its curve, and its reserves.
-- | Access is set per use.
type MarketTerms = { base :: String, quote :: String, cost :: CostFunction, reserves :: Holdings }

-- | A Market from its terms and the Entities that may trade in it.
market :: MarketTerms -> Array String -> Market
market t access = noMarket { base = Id t.base, quote = Id t.quote, cost = t.cost, reserves = t.reserves, access = map Id access }

-- | A facility's price, paid in the Resource to the Entity.
paid :: ResourceId -> CostFunction -> EntityId -> Maybe Payment
paid resource curve to = Just { curve, resource, to }

-- Strategy pieces -------------------------------------------------------------------------------

-- | A price band: trade `below` while the Market's price is at or below `threshold`, and `above`
-- | while it is above.
banded :: String -> Int -> Int -> Int -> Array Clause
banded m threshold below above =
  [ rule [ le (priceAt (named m)) (lit threshold) ] [ orderAt (named m) (lit below) Nothing ]
  , rule [ gt (priceAt (named m)) (lit threshold) ] [ orderAt (named m) (lit above) Nothing ]
  ]

-- | The names of one Market's supplier parameters: the offer below the high price, the offer at or
-- | above it, and the disruption cut.
type SupplierNames = { offer :: String, high :: String, cut :: String }

-- | A Market's supplier parameters: their names, and their entries with the values given.
type Supplier = { names :: SupplierNames, entries :: Array (Tuple String Q) }

-- | The supplier parameters of the Market with this suffix: `supplier-offer-<suffix>`,
-- | `supplier-offer-<suffix>-high`, and `disruption-cut-<suffix>`.
supplier :: String -> { offer :: Int, high :: Int, cut :: Int } -> Supplier
supplier suffix v = { names, entries: [ names.offer := v.offer, names.high := v.high, names.cut := v.cut ] }
  where
  names =
    { offer: "supplier-offer-" <> suffix
    , high: "supplier-offer-" <> suffix <> "-high"
    , cut: "disruption-cut-" <> suffix
    }

-- | Suppliers' offers in one Market. Below the parameter `highPrice` they offer the base volume; at or
-- | above it, the high volume. In the round the parameter `disruptionRound` names, the offer is cut by
-- | the disruption amount and never below zero: a cut larger than the offer removes it, and never
-- | turns it into a purchase.
supplierOffers :: { disruptionRound :: String, highPrice :: String } -> String -> SupplierNames -> Array Clause
supplierOffers ps m names =
  [ rule [ notDisruption, low ] [ offer (subQ (lit 0) (param names.offer)) ]
  , rule [ notDisruption, high ] [ offer (subQ (lit 0) (param names.high)) ]
  , rule [ disruption, low ] [ offer (subQ (lit 0) (max' (lit 0) (subQ (param names.offer) (param names.cut)))) ]
  , rule [ disruption, high ] [ offer (subQ (lit 0) (max' (lit 0) (subQ (param names.high) (param names.cut)))) ]
  ]
  where
  disruption = eqQ roundQ (param ps.disruptionRound)
  notDisruption = not' disruption
  low = not' (ge (priceAt (named m)) (param ps.highPrice))
  high = ge (priceAt (named m)) (param ps.highPrice)
  offer quantity = orderAt (named m) quantity Nothing

-- Rule kinds ------------------------------------------------------------------------------------

-- | A levy on inbound flows of the covered Resources, per unit, paid in a Resource to the destination.
type LevyKit =
  { may :: Capability
  , rule :: { jurisdiction :: String, origin :: String, rate :: Q } -> RuleF Q
  , enact :: { jurisdiction :: String, origin :: String, rate :: Q } -> Intent
  , self :: { origin :: String, rate :: Expr } -> IntentTemplate
  }

levyRule :: { kind :: String, covered :: Array ResourceId, paidIn :: ResourceId } -> LevyKit
levyRule d =
  { may: mayEnact d.kind
  , rule: \r -> levyOf r.jurisdiction r.origin r.rate
  , enact: \r -> ChangeRule (Enact (levyOf r.jurisdiction r.origin r.rate))
  , self: \r -> selfEnact (levyOf selfName r.origin r.rate)
  }
  where
  levyOf :: forall v. String -> String -> v -> RuleF v
  levyOf jurisdiction origin rate = FlowRule { kind: Id d.kind, scope: { governor: Id jurisdiction, counterparty: Id origin, direction: Inbound, over: d.covered }, effect: Levy { rate, basis: PerUnit, in: d.paidIn, to: LevyToDestination } }

-- | A block of flows of the covered Resources, either way.
type BlockKit =
  { may :: Capability
  , rule :: { jurisdiction :: String, target :: String } -> RuleF Q
  , enact :: { jurisdiction :: String, target :: String } -> Intent
  , self :: { target :: String } -> IntentTemplate
  }

blockRule :: { kind :: String, covered :: Array ResourceId } -> BlockKit
blockRule d =
  { may: mayEnact d.kind
  , rule: \r -> blockOf r.jurisdiction r.target
  , enact: \r -> ChangeRule (Enact (blockOf r.jurisdiction r.target))
  , self: \r -> selfEnact (blockOf selfName r.target)
  }
  where
  blockOf :: forall v. String -> String -> RuleF v
  blockOf jurisdiction target = FlowRule { kind: Id d.kind, scope: { governor: Id jurisdiction, counterparty: Id target, direction: EitherWay, over: d.covered }, effect: Block }

-- | A reduction of a capacity. Its enactment is polymorphic in its filling: a fixture's takes `Q`, a
-- | Strategy's an expression.
type ReduceKit =
  { may :: Capability
  , rule :: { capacity :: String, units :: Q } -> RuleF Q
  , enact :: forall m v. { capacity :: String, units :: v } -> IntentF m v
  }

reduceRule :: { kind :: String } -> ReduceKit
reduceRule d =
  { may: mayEnact d.kind
  , rule: \r -> reduceOf r.capacity r.units
  , enact: \r -> ChangeRule (Enact (reduceOf r.capacity r.units))
  }
  where
  reduceOf :: forall v. String -> v -> RuleF v
  reduceOf capacity units = CapacityRule { kind: Id d.kind, capacity: Id capacity, reduce: units }

-- Premises --------------------------------------------------------------------------------------

-- | What a reusable piece of a Setting contributes.
type PremiseParts =
  { resources :: Map ResourceId ResourceDefinition
  , markets :: Map MarketId Market
  , transformations :: Map TransformationId Transformation
  , ruleKinds :: Map RuleKindId RuleKindDecl
  , flows :: Array Flow
  , policies :: Array (Tuple String Strategy)
  , parameters :: Array (Tuple String Q)
  }

-- | A piece nothing has been put in; a `Premise` is built by updating it.
emptyParts :: PremiseParts
emptyParts = { resources: Map.empty, markets: Map.empty, transformations: Map.empty, ruleKinds: Map.empty, flows: [], policies: [], parameters: [] }

-- | A reusable piece of a Setting. Pieces combine with `<>`; a key two pieces both define fails, naming it.
newtype Premise = Pieces (Either String PremiseParts)

premise :: PremiseParts -> Premise
premise = Pieces <<< Right

instance Semigroup Premise where
  append (Pieces a) (Pieces b) = Pieces do
    x <- a
    y <- b
    resources <- union "Resource" x.resources y.resources
    markets <- union "Market" x.markets y.markets
    transformations <- union "Transformation" x.transformations y.transformations
    ruleKinds <- union "Rule kind" x.ruleKinds y.ruleKinds
    policies <- pairs "Policy" x.policies y.policies
    parameters <- pairs "parameter" x.parameters y.parameters
    pure { resources, markets, transformations, ruleKinds, flows: x.flows <> y.flows, policies, parameters }
    where
    union :: forall k v. Ord k => Show k => String -> Map k v -> Map k v -> Either String (Map k v)
    union what l r = case Array.head (Set.toUnfoldable (Set.intersection (Map.keys l) (Map.keys r))) of
      Just k -> Left ("two Premises define the " <> what <> " " <> show k)
      Nothing -> Right (Map.union l r)

    pairs :: forall v. String -> Array (Tuple String v) -> Array (Tuple String v) -> Either String (Array (Tuple String v))
    pairs what l r = case Array.head (Array.intersect (map fst l) (map fst r)) of
      Just k -> Left ("two Premises define the " <> what <> " " <> k)
      Nothing -> Right (l <> r)

instance Monoid Premise where
  mempty = premise emptyParts

-- | The combined parts, or the key two Premises both defined.
assemble :: Premise -> Either String PremiseParts
assemble (Pieces p) = p

-- Fixtures --------------------------------------------------------------------------------------

-- | What every fixture of a premise shares: the seed, the Rule kinds, and which Resources may be
-- | reserved or decay.
type FixtureContext =
  { seed :: Int
  , ruleKinds :: Map RuleKindId RuleKindDecl
  , reservable :: Array ResourceId
  , decays :: Array Decay
  }

-- | What a fixture gives. The Gameboard's sections are always written; the rounds and the circulation
-- | are written when given.
type FixtureSpec =
  { round :: Int
  , numeraire :: ResourceId
  , resources :: Map ResourceId ResourceDefinition
  , markets :: Map MarketId Market
  , transformations :: Map TransformationId Transformation
  , transport :: Array FlowGate
  , rules :: Array (RuleF Q)
  , entities :: Map EntityId Entity
  , representations :: Maybe (Array Representation)
  , circulation :: Maybe Circulation
  , proposals :: Maybe (Array SignedProposal)
  , expect :: Maybe Authored
  , rounds :: Maybe (Array RoundSpec)
  }

-- | The fixture's JSON, named `name`: its premise, its bindings (one per round, in order from the
-- | World's round), and its expectations (each round's checks, named by its round). The rounds are the
-- | list given, or the one round written at the top level.
fixture :: FixtureContext -> String -> FixtureSpec -> Tuple String Json
fixture ctx name s = Tuple name (encodeWith Premise fixtureShape doc)
  where
  rounds' :: Array RoundSpec
  rounds' = fromMaybe [ { proposals: s.proposals, expect: s.expect } ] s.rounds

  decays = filter (\d -> Map.member d.resource s.resources) ctx.decays

  doc :: Fixture
  doc =
    { seed: ctx.seed
    , name
    , premiseAuthored: { circulation: fromMaybe emptyCirculation s.circulation, reservable: filter (\r -> Map.member r s.resources) ctx.reservable, decays }
    , world: emptyWorld
        { round = s.round
        , numeraire = s.numeraire
        , resources = s.resources
        , markets = s.markets
        , transformations = s.transformations
        , flowGates = s.transport
        , ruleKinds = ctx.ruleKinds
        , entities = s.entities
        , rules = rulesFromList s.rules
        , representations = fromMaybe [] s.representations
        , recurring = compileIn s.numeraire (fromMaybe emptyCirculation s.circulation) decays
        }
    , bindings: map (\r -> { proposals: fromMaybe [] r.proposals }) rounds'
    , expectations: Array.catMaybes (mapWithIndex (\k r -> map (\e -> { round: s.round + k, expect: e }) r.expect) rounds')
    }
