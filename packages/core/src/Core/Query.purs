-- | Read-only queries over a World State, shared by the engine, invariants, analysis views,
-- | and tests. Absent entries read as zero. Also here: the draw and Transformation-pool helpers
-- | that Fund prices and Settle allocates against, and the Intent selectors.
module Core.Query
  ( lookupQ
  , holding
  , accountsOf
  , capabilitiesOf
  , entitiesOf
  , marketsOf
  , rulesOf
  , transformationsOf
  , priceOf
  , totalHolding
  , reserveOf
  , aggregateOf
  , isAggregate
  , holderOf
  , drawable
  , conversionPrice
  , poolOf
  , poolCeiling
  , procedureOf
  , procedureSupport
  , tradesOf
  , transfersOf
  , transformsOf
  , enactedAtBoundary
  , droppedProposals
  ) where

import Prelude

import Core.Sum (ctor)

import Control.Alt ((<|>))
import Core.Map (values)

import Core.Kernel.Law (withheld)
import Core.Kernel.Types (Amount(..), Operation(..), Subject(..), Trigger(..))
import Core.Market (averagePrice, marketPrice)
import Core.Num (Q)
import Core.Num as Q
import Core.Types (poolDraw, holderDraw, CollectiveProcedure, CollectiveProcedureId, Aggregate, Capability, Entity, EntityId, EntityKind(..), IntentF, Market, MarketId, Price, ProposalId, ResourceId, RoundIndex, Rule, RuleF(..), Transformation, TransformationId)
import Core.Slice (event, orKind, read, select)
import Core.World (Cause(..), Event, RecurringAmount(..), WorldState)
import Data.Array (any, concatMap, filter, find, mapMaybe)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Foldable (foldl)

lookupQ :: forall k. Ord k => k -> Map k Q -> Q
lookupQ k m = fromMaybe Q.zero (Map.lookup k m)

-- | What an Entity holds of a Resource.
holding :: WorldState -> EntityId -> ResourceId -> Q
holding w e r = lookupQ r (accountsOf w e)

-- | An Entity's accounts: the amount it holds of each Resource it has an account for; empty for an
-- | Entity the World State does not have.
accountsOf :: WorldState -> EntityId -> Map ResourceId Q
accountsOf w e = maybe Map.empty _.accounts (Map.lookup e w.entities)

-- | Every Entity, in id order.
entitiesOf :: WorldState -> Array Entity
entitiesOf = values <<< _.entities

-- | Every Market, in id order.
marketsOf :: WorldState -> Array Market
marketsOf = values <<< _.markets

-- | The Rules in force, in id order.
rulesOf :: WorldState -> Array Rule
rulesOf = values <<< _.rules

-- | Every Transformation, in id order.
transformationsOf :: WorldState -> Array Transformation
transformationsOf = values <<< _.transformations

-- | What an Entity may do; nothing for an Entity the World State does not have.
capabilitiesOf :: WorldState -> EntityId -> Array Capability
capabilitiesOf w e = maybe [] _.capabilities (Map.lookup e w.entities)

-- | A Market's current marginal price, by id.
priceOf :: WorldState -> MarketId -> Maybe Price
priceOf w m = marketPrice <$> Map.lookup m w.markets

-- | A Resource's total across every account and Market reserve.
totalHolding :: WorldState -> ResourceId -> Q
totalHolding w r = Q.sumQ (map (lookupQ r <<< _.accounts) (entitiesOf w) <> map (lookupQ r <<< _.reserves) (marketsOf w))

-- | What a Market holds of a Resource in reserve; zero for an unknown Market.
reserveOf :: WorldState -> MarketId -> ResourceId -> Q
reserveOf w m r = maybe Q.zero (lookupQ r <<< _.reserves) (Map.lookup m w.markets)

-- | An Entity's Aggregate definition, when it is an Aggregate.
aggregateOf :: Entity -> Maybe Aggregate
aggregateOf e = case e.kind of
  Aggregate a -> Just a
  _ -> Nothing

isAggregate :: Entity -> Boolean
isAggregate = isJust <<< aggregateOf

-- | The holder of a Resource: the Entity with a positive holding of it, else the first with an account
-- | for it (a holder run down to zero is still its holder). Only a Resource that has one holder that
-- | matters has one: one a holder draw names, an effect changes, or a recurring flow reads as an amount
-- | held. INV-08 keeps a positive holder unique for the drawn ones. Holding Rules and draws both use this.
holderOf :: WorldState -> ResourceId -> Maybe EntityId
holderOf w r
  | not (governable w r) = Nothing
  | otherwise = _.id <$> (find (\e -> lookupQ r e.accounts > Q.zero) entities <|> find (\e -> Map.member r e.accounts) entities)
      where
      entities = entitiesOf w

-- | Whether a Resource is one whose holder governs it: a holder draw, an effect, or a recurring read names it.
governable :: WorldState -> ResourceId -> Boolean
governable w r = any names (transformationsOf w) || any reads w.recurring
  where
  names tr = (_.resource <$> holderDraw tr) == Just r || any (\e -> e.resource == r) tr.effects
  reads flow = case flow.amount of
    HeldAmount x -> x == r
    _ -> false

-- | What can be drawn from a Resource: its holder's holding, less what Rules reduce it by.
drawable :: WorldState -> ResourceId -> Q
drawable w r =
  let
    held = maybe Q.zero (\o -> holding w o r) (holderOf w r)
    reduced = withheld r (mapMaybe holdingLaw (rulesOf w))
  in
    max Q.zero (Q.sub held reduced)
  where
  holdingLaw = case _ of
    HoldingRule x -> Just
      { subject: OfStock { holders: Nothing, resource: x.resource }
      , trigger: EachRound
      , operation: Withhold (Exactly x.reduce)
      , guard: []
      , phase: 0
      , cause: OfRule { kind: x.kind }
      }
    FlowRule _ -> Nothing

-- | A Transformation's uniform price at a total of `q` units: its cost function's average price, or
-- | zero for a Transformation with no payment.
conversionPrice :: Transformation -> Q -> Q
conversionPrice tr q = maybe Q.zero (\p -> averagePrice p.curve Q.zero q) tr.payment

-- | The pool a Transformation's fills draw on: the Resource it draws from a holder if it has one, else the
-- | Transformation alone. Allocations in one pool are allocated together and priced at one uniform price.
poolOf :: TransformationId -> Transformation -> Either TransformationId ResourceId
poolOf tid tr = maybe (Left tid) (Right <<< _.resource) (poolDraw tr)

-- | The most units a Transformation's pool can fill in total, its ceiling, which escrow prices at in full
-- | and settlement never exceeds (§5.3). The pool is every Transformation that draws the same Resource
-- | from its holder, so its fills together draw on it: a total of q units uses at least
-- | q × (smallest `perUnit` in the pool). The ceiling is therefore what can be drawn divided by that
-- | smallest use, rounded down, and every member of the pool escrows at it, whatever its own use.
-- | A Transformation with no such draw has a ceiling at is the domain top of its cost function. Nothing means no
-- | ceiling, which only a Transformation with no cost function has, and that one is priced at zero.
poolCeiling :: WorldState -> Transformation -> Maybe Q
poolCeiling w tr = case poolDraw tr of
  Just d -> Q.divFloor (drawable w d.resource) (smallestUse w d.resource d.perUnit)
  _ -> _.curve.domain.hi <$> tr.payment

-- | The smallest `perUnit` among the pool's draws on `r`, starting from `use` (the Transformation's own, so
-- | the result is never above it).
smallestUse :: WorldState -> ResourceId -> Q -> Q
smallestUse w r use = foldl min use (map _.perUnit (filter ((_ == r) <<< _.resource) (mapMaybe poolDraw (transformationsOf w))))

-- | The collective procedure a Proposal is put to, through its author Aggregate.
procedureOf :: WorldState -> EntityId -> CollectiveProcedureId -> Maybe CollectiveProcedure
procedureOf w author pid = Map.lookup author w.entities >>= aggregateOf >>= Map.lookup pid <<< _.procedures

-- | A procedure's support: its residual weight and the weights of those who signed, the author aside.
procedureSupport :: CollectiveProcedure -> EntityId -> Array EntityId -> Q
procedureSupport proc author signed = Q.add proc.residualWeight (Q.sumQ (map (\m -> lookupQ m proc.weights) (filter (_ /= author) signed)))

-- | The Trade orders of one Intent list, in order: the one place that selects a Trade.
tradesOf :: forall m v. Array (IntentF m v) -> Array { market :: m, quantity :: v, limitPrice :: Maybe v }
tradesOf = mapMaybe (ctor @"Trade")

transfersOf :: forall m v. Array (IntentF m v) -> Array { from :: EntityId, to :: EntityId, resource :: ResourceId, quantity :: v }
transfersOf = mapMaybe (ctor @"Transfer")

transformsOf :: forall m v. Array (IntentF m v) -> Array { by :: EntityId, transformation :: TransformationId, quantity :: v }
transformsOf = mapMaybe (ctor @"Transform")

-- | The Proposals a round turned down whole: rejected or lapsed. A Proposal that lost one order is not among them.
droppedProposals :: Array Event -> Array ProposalId
droppedProposals = select (orKind (read @"proposal" (event @"Rejected")) (read @"proposal" (event @"Lapsed")))

-- | The Rules `by` enacted against `target` that took effect at the boundary of `round`, i.e. that
-- | were proposed in the round before: a flows Rule of `by`'s whose counterparty is `target`.
enactedAtBoundary :: RoundIndex -> EntityId -> EntityId -> Array Event -> Array Rule
enactedAtBoundary round by target = concatMap rulesOf' <<< select (event @"Enacted")
  where
  rulesOf' e
    | e.round /= round = []
    | otherwise = case e.rule of
        FlowRule r | r.scope.governor == by && r.scope.counterparty == target -> [ e.rule ]
        _ -> []
