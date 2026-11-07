-- | Decoder for the Circulation section: production, flows, shares, and requirements.
module Board.Decode.Circulation
  ( decodeCirculation
  ) where

import Prelude

import Control.Applicative (when)
import Board.Decode.Common (unknownKind)
import Board.Tags (argOf, isBare, payeeKind, salesKind)
import Core.Num as Q
import Core.Types (Id(..))
import Core.World (Circulation, ShareSource(..))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError(..), decodeJson, (.:), (.:?), (.!=))
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Traversable (traverse)

decodeCirculation :: Json -> Either JsonDecodeError Circulation
decodeCirculation j = do
  o <- decodeJson j
  production <- o .:? "production" .!= [] >>= traverse \x -> do
    p <- decodeJson x
    e <- p .: "entity"
    r <- p .: "resource"
    c <- p .: "capacity"
    pure { entity: Id e, resource: Id r, capacity: Id c }
  flows <- o .:? "flows" .!= [] >>= traverse \x -> do
    f <- decodeJson x
    from <- f .: "from"
    to <- f .: "to"
    r <- f .: "resource"
    amount <- f .: "amount"
    -- a flow is a fixed payment per round (docs/03 §5.4), so its amount is never negative
    when (amount < Q.zero) (Left (Named "flow amount" (UnexpectedValue x)))
    pure { from: Id from, to: Id to, resource: Id r, amount }
  shares <- o .:? "shares" .!= [] >>= traverse \x -> do
    f <- decodeJson x
    from <- f .: "from"
    to <- f .: "to"
    share <- f .: "share"
    src <- f .: "source"
    source <- case argOf salesKind src of
      Just r -> Right (SalesOf (Id r))
      Nothing
        | isBare payeeKind src -> Right PayeeReceipts
        | otherwise -> Left (unknownKind "share source" src)
    pure { from: Id from, to: Id to, share, source }
  requirements <- o .:? "requirements" .!= [] >>= traverse \x -> do
    f <- decodeJson x
    e <- f .: "entity"
    r <- f .: "resource"
    amount <- f .:? "amount"
    penalty <- f .:? "penalty" .!= Q.zero
    pure { entity: Id e, resource: Id r, amount, penalty }
  pure { production, flows, shares, requirements }
