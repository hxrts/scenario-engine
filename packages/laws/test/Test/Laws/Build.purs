-- | The core suite's test inputs: `core` depends on nothing, so its tests cannot use `Schema.Build`.
module Test.Laws.Build where

import Prelude
import Core.Draw (Seed(..))
import Core.Num (Q)
import Core.Num as Q
import Laws.World (WorldState)
import Laws.Types (LevyPayer(..), ResourceId, ResourceTag, Draw, DrawSource(..), Capability, Direction(..), Actor, ActorId, ActorKind(..), ActorTag, Estimator, FlowEffect(..), FlowGovernance(..), Id(..), Intent, LevyBasis(..), LevyRecipient(..), Exchange, Representation, ResourceDefinition, RuleF(..), RuleForm(..), RuleKindDecl, RuleKindTag, SignedCandidateTransaction, Transferability(..), Transformation, candidateTransactionId)
import Data.Tuple (Tuple(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))

q :: String -> Q
q s = case Q.parse s of
  Just x -> x
  Nothing -> Q.zero

-- | An Actor holding 100 of the numeraire M and the given capabilities.
explicitActor :: String -> Array Capability -> Actor
explicitActor name caps =
  { id: Id name
  , accounts: Map.singleton (Id "M") (Q.fromInt 100)
  , capabilities: caps
  , kind: PlayableActor (Id name)
  }

-- | An Actor with this holding added to its accounts.
withAccount :: Id ResourceTag -> Q -> Actor -> Actor
withAccount r x e = e { accounts = Map.insert r x e.accounts }

-- | A draw of `perUnit` from the holder of a Resource.
heldDraw :: Id ResourceTag -> Q -> Draw
heldDraw r perUnit = { resource: r, from: HolderOf, perUnit }

-- | A draw of `perUnit` from the executor.
ownDraw :: Id ResourceTag -> Q -> Draw
ownDraw r perUnit = { resource: r, from: Executor, perUnit }

-- | A World State over the given Actors, with resources M (numeraire) and E, no exchanges, and
-- | no rules, in round 1.
baseWorld :: Array Actor -> WorldState
baseWorld es =
  { round: 1
  , seed: Seed 1
  , actors: Map.fromFoldable (map (\e -> Tuple e.id e) es)
  , resources: Map.fromFoldable (map (\r -> Tuple (Id r) { id: Id r, unit: r, transferability: Free, category: Nothing }) [ "M", "E" ])
  , exchanges: Map.empty
  , transformations: Map.empty
  , representations: []
  , numeraire: Id "M"
  , focus: Nothing
  , flowGates: []
  , groups: Map.empty
  , ruleKinds: testKinds
  , rules: Map.empty
  , pending: []
  , laws: []
  , templates: Map.empty
  , factors: Map.empty
  , recurring: []
  , events: []
  }

-- | An ordinary, unsigned-by-others candidate Transaction from `author`, numbered `index` in round 1.
submit :: String -> Int -> Array Intent -> SignedCandidateTransaction
submit author index intents =
  { candidateTransaction: { id: candidateTransactionId 1 (Id author) index, author: Id author, intents, signers: [], procedure: Nothing }
  , signatures: []
  }

-- | An Exchange `id` for `base` against M at a flat price, with reserves of 50 of each, and the given access.
priced :: String -> String -> Array String -> String -> Exchange
priced id base access p =
  { id: Id id
  , base: Id base
  , quote: Id "M"
  , cost: { intercept: q p, slope: Q.zero, domain: { lo: Q.zero, hi: Q.fromInt 100 } }
  , position: Q.zero
  , reserves: Map.fromFoldable [ Tuple (Id base) (Q.fromInt 50), Tuple (Id "M") (Q.fromInt 50) ]
  , access: map Id access
  , adjustment: Q.zero
  }

-- | An Exchange for E against M, cost base 1 and slope 0 over [0, 100], with the given access.
exchange :: Array String -> Exchange
exchange access =
  { id: Id "EM"
  , base: Id "E"
  , quote: Id "M"
  , cost: { intercept: Q.fromInt 1, slope: Q.zero, domain: { lo: Q.zero, hi: Q.fromInt 100 } }
  , position: Q.zero
  , reserves: Map.fromFoldable [ Tuple (Id "E") (Q.fromInt 50), Tuple (Id "M") (Q.fromInt 50) ]
  , access: map Id access
  , adjustment: Q.zero
  }

-- Rules of the three kinds the test premises declare (S8: the forms are what these tests pin).
tariffR :: forall v. { jurisdiction :: Id ActorTag, origin :: Id ActorTag, rate :: v } -> RuleF v
tariffR r = FlowRule { kind: Id "tariff", scope: { governor: r.jurisdiction, counterparty: r.origin, direction: Inbound, over: [ Id "E" ] }, effect: Levy { rate: r.rate, basis: PerUnit, in: Id "M", to: LevyToDestination, payer: PaidBySender } }

banR :: forall v. { jurisdiction :: Id ActorTag, target :: Id ActorTag } -> RuleF v
banR r = FlowRule { kind: Id "accessBan", scope: { governor: r.jurisdiction, counterparty: r.target, direction: EitherWay, over: [ Id "E" ] }, effect: Block }

holdR :: forall v. { capacity :: ResourceId, units :: v } -> RuleF v
holdR r = HoldingRule { kind: Id "withholding", resource: r.capacity, reduce: r.units }

testKinds :: Map.Map (Id RuleKindTag) RuleKindDecl
testKinds = Map.fromFoldable
  [ Tuple (Id "tariff") { form: LevyFlows GovernedByDestination, text: "" }
  , Tuple (Id "accessBan") { form: BlockFlows GovernedByEither, text: "" }
  , Tuple (Id "withholding") { form: ReduceHolding, text: "" }
  ]

-- | Change one Actor of a World.
onActor :: ActorId -> (Actor -> Actor) -> WorldState -> WorldState
onActor k f w = w { actors = Map.update (Just <<< f) k w.actors }

-- | A Resource, freely transferable, whose unit is its id.
resource :: String -> ResourceDefinition
resource r = { id: Id r, unit: r, transferability: Free, category: Nothing }

-- | A Transformation that does nothing and accepts any order: a test updates what it is about.
transformation :: String -> Transformation
transformation name =
  { id: Id name
  , inputs: Map.empty
  , outputs: Map.empty
  , draws: []
  , payment: Nothing
  , delay: 0
  , minimum: Q.zero
  , maximum: Nothing
  , ownerSigns: false
  , effects: []
  }

-- | A Representation `r` of an Aggregate's domain, at weight 1.
representation :: { explicitActor :: String, aggregate :: String, domain :: String, observes :: String, estimator :: Estimator } -> Representation
representation r = { id: Id "r", explicitActor: Id r.explicitActor, aggregate: Id r.aggregate, domain: Id r.domain, observes: Id r.observes, weight: Q.fromInt 1, estimator: r.estimator }
