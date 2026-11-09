-- | The vocabulary a fixture is written in, re-exported so a fixture imports one module. This is the one deliberate re-export in the tree.
module Premises.Kit.Prelude
  ( module Core.Num
  , module Laws.Types
  , module Data.Argonaut.Core
  , module Data.Map
  , module Data.Maybe
  , module Data.Tuple
  , module Schema.Build.Base
  , module Schema.Build.Fixture
  , module Schema.Build.Intent
  , module Schema.Build.Quantity
  ) where

import Core.Num (Q)
import Laws.Types (Capability, Actor, ActorId, ActorKind(Aggregate), Id(..), Exchange, ExchangeId, Representation, Transformation, TransformationId)
import Data.Argonaut.Core (Json)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Schema.Build.Base (keyed, mayTrade, mayTransfer, mayTransform, (:=))
import Schema.Build.Fixture (balances, noExpect, noRound, procedureOutcome, candidateTransaction, ref)
import Schema.Build.Intent (trade, transfer, transform)
import Schema.Build.Quantity (q)
