-- | The reply helpers and refusals of the read and write handlers: why a stored run cannot be served, and the
-- | smart constructors of each refusal an operation may declare (`Handlers.Api.Contract`). Each stays polymorphic in
-- | the rest of the row, so a handler's refusals are exactly those its operation's entry lists.
module Server.Errors
  ( describe
  , Answer
  , ok
  , badRequest
  , notFound
  , conflict
  , internal
  , viewOr
  , replyOr
  ) where

import Prelude

import Data.Either (Either(..), either)
import Data.Variant (Variant, inj)
import Effects.Signatures (RunError(..))
import Type.Proxy (Proxy(..))

describe :: RunError -> String
describe = case _ of
  Missing -> "not found"
  Unreadable why -> why

-- | What a handler produces: its reply, or a refusal from its row.
type Answer :: Row Type -> Type -> Type
type Answer e a = Either (Variant e) a

ok :: forall m e a. Applicative m => a -> m (Answer e a)
ok = pure <<< Right

badRequest :: forall m e a. Applicative m => String -> m (Answer (badRequest :: String | e) a)
badRequest = pure <<< Left <<< inj (Proxy :: Proxy "badRequest")

notFound :: forall m e a. Applicative m => String -> m (Answer (notFound :: String | e) a)
notFound = pure <<< Left <<< inj (Proxy :: Proxy "notFound")

conflict :: forall m e a. Applicative m => String -> m (Answer (conflict :: String | e) a)
conflict = pure <<< Left <<< inj (Proxy :: Proxy "conflict")

internal :: forall m e a. Applicative m => String -> m (Answer (internal :: String | e) a)
internal = pure <<< Left <<< inj (Proxy :: Proxy "internal")

-- | A view, or a 400 with the reason it cannot be given (for example an out-of-range round).
viewOr :: forall m e a. Applicative m => Either String a -> m (Answer (badRequest :: String | e) a)
viewOr = either badRequest ok

-- | A run's reply, or a 404 for a run that is missing and a 500 for one that cannot be read.
replyOr :: forall m e a b. Applicative m => String -> (a -> m (Answer (notFound :: String, internal :: String | e) b)) -> Either RunError a -> m (Answer (notFound :: String, internal :: String | e) b)
replyOr id f = case _ of
  Right a -> f a
  Left Missing -> notFound ("no run " <> id)
  Left (Unreadable why) -> internal ("run " <> id <> " is unreadable: " <> why)
