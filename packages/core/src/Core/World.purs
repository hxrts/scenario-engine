-- | World State, Views, events, and round records
-- | (docs/02_type_system.md §4, §8; docs/04_instantiated_game.md §9).
module Core.World where

import Prelude

import Core.Draw (Seed)
import Core.Types (BehaviorDomain, Entity, EntityId, Market, MarketId, Price, Proposal, ProposalId, Quantity, Ratio, Representation, ResourceDefinition, ResourceId, RoundIndex, Rule, RuleId, SignedProposal, Transformation, TransformationId, CapacityRef)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)

type WorldState =
  { round :: RoundIndex
  , seed :: Seed -- the game seed
  , entities :: Map EntityId Entity
  , resources :: Map ResourceId ResourceDefinition
  , markets :: Map MarketId Market
  , transformations :: Map TransformationId Transformation
  , representations :: Array Representation
  , numeraire :: ResourceId -- the Resource facility costs are paid in
  , transport :: Map ResourceId TransformationId -- facility that carries a Resource between regions
  , jurisdiction :: Map EntityId EntityId -- governing Entity, when not the Entity itself
  , rules :: Map RuleId Rule -- Rules in force
  , scheduled :: Map RuleId (Maybe Rule) -- take effect at the next round boundary; Nothing repeals
  , deliveries :: Array Delivery -- due at Prepare
  , factors :: Map EntityId (Map BehaviorDomain Ratio) -- stored Representation adjustment factors
  , circulation :: Circulation
  , events :: Array Event -- public history
  }

-- | The recurring economic cycle (docs/02_type_system.md §6), as data.
type Circulation =
  { production :: Array { entity :: EntityId, resource :: ResourceId, capacity :: CapacityRef } -- delivered next round
  , flows :: Array { from :: EntityId, to :: EntityId, resource :: ResourceId, amount :: Quantity } -- fixed income and expenditure
  , shares :: Array { from :: EntityId, to :: EntityId, share :: Ratio, source :: ShareSource } -- variable flows
  , requirements :: Array Requirement -- consumed at commit from the net position
  }

-- | What a variable flow is a share of, from the paying Entity's settlement this round.
data ShareSource
  = SalesOf ResourceId -- net Money revenue from selling this Resource on Markets
  | PayeeReceipts -- Money received as a facility payee

type Requirement =
  { entity :: EntityId
  , resource :: ResourceId
  , amount :: Maybe Quantity -- Nothing: equal to this round's purchases of the Resource
  , penalty :: Quantity -- reduction of fixed incoming flows per unit unmet
  }

-- | A scheduled change to a holding or a capacity, applied at Prepare.
type Delivery =
  { round :: RoundIndex, entity :: EntityId, target :: Target, quantity :: Quantity }

data Target = Holding ResourceId | CapacityOf CapacityRef

-- | What a Policy sees (INV-16).
type View =
  { round :: RoundIndex
  , self :: Entity
  , prices :: Map MarketId Price
  , rules :: Array Rule
  , events :: Array Event
  , factors :: Map BehaviorDomain Ratio
  , toSign :: Array Proposal
  }

-- | Everything the engine records. The round record is the list of a round's events.
data Event
  = Lapsed { round :: RoundIndex, proposal :: ProposalId }
  | Rejected { round :: RoundIndex, proposal :: ProposalId, reason :: String }
  | Traded { round :: RoundIndex, market :: MarketId, entity :: EntityId, quantity :: Quantity, price :: Price }
  | Transferred { round :: RoundIndex, from :: EntityId, to :: EntityId, resource :: ResourceId, quantity :: Quantity }
  | Transformed { round :: RoundIndex, by :: EntityId, transformation :: TransformationId, quantity :: Quantity, paid :: Quantity }
  | Delivered { round :: RoundIndex, entity :: EntityId, target :: Target, quantity :: Quantity }
  | Enacted { round :: RoundIndex, id :: RuleId, rule :: Rule }
  | Repealed { round :: RoundIndex, id :: RuleId }
  | CapacityChanged { round :: RoundIndex, entity :: EntityId, capacity :: CapacityRef, delta :: Quantity }
  | Estimated { round :: RoundIndex, aggregate :: EntityId, domain :: BehaviorDomain, factor :: Ratio }
  | Consumed { round :: RoundIndex, entity :: EntityId, resource :: ResourceId, quantity :: Quantity }
  -- an amount owed but not delivered: an unpaid payment (Transfer, tariff, facility payment),
  -- or an unmet operating requirement; the resource says which
  | Shortfall { round :: RoundIndex, entity :: EntityId, resource :: ResourceId, quantity :: Quantity }
  | Decayed { round :: RoundIndex, entity :: EntityId, resource :: ResourceId, quantity :: Quantity }
  | ProcedureOutcome { round :: RoundIndex, proposal :: ProposalId, support :: Ratio, threshold :: Ratio, executed :: Boolean }

derive instance Generic Target _
derive instance Eq Target
instance Show Target where
  show = genericShow

instance EncodeJson Target where
  encodeJson = genericEncodeJson

instance DecodeJson Target where
  decodeJson = genericDecodeJson

derive instance Generic ShareSource _
derive instance Eq ShareSource
instance Show ShareSource where
  show = genericShow

instance EncodeJson ShareSource where
  encodeJson = genericEncodeJson

instance DecodeJson ShareSource where
  decodeJson = genericDecodeJson

derive instance Generic Event _
derive instance Eq Event
instance Show Event where
  show = genericShow

instance EncodeJson Event where
  encodeJson = genericEncodeJson

instance DecodeJson Event where
  decodeJson = genericDecodeJson

type RoundRecord =
  { round :: RoundIndex
  , inputs :: Array SignedProposal
  , events :: Array Event
  }

type RoundResult = { world :: WorldState, record :: RoundRecord }

-- | The immutable record of a game's resolved configuration and initial World State.
type Manifest =
  { gameboard :: String
  , engine :: String
  , draw :: String
  , initial :: WorldState
  }
