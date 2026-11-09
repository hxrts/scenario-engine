-- | Generic access to a sum's constructors: reading one constructor's payload, and a record of one handler
-- | per constructor. A constructor name that does not exist is a compile error.
module Core.Sum
  ( class Pick
  , pick
  , ctor
  , class EnumFields
  , enumFields
  , class HandlerRow
  , class Handles
  , handles
  ) where

import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments, Sum(..), from)
import Data.Semigroup ((<>))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Prim.Row as Row
import Prim.TypeError (class Fail, Beside, QuoteLabel, Text)
import Record as Record
import Type.Proxy (Proxy(..))

-- | Find the payload of the constructor `name` in a sum's representation.
class Pick :: Symbol -> Type -> Type -> Constraint
class Pick name rep b | name rep -> b where
  pick :: Proxy name -> rep -> Maybe b

instance Pick name (Constructor name (Argument b)) b where
  pick _ (Constructor (Argument b)) = Just b

else instance Pick name (Sum (Constructor name (Argument b)) r) b where
  pick _ = case _ of
    Inl (Constructor (Argument b)) -> Just b
    Inr _ -> Nothing

else instance Pick name r b => Pick name (Sum (Constructor other a) r) b where
  pick p = case _ of
    Inr r -> pick p r
    Inl _ -> Nothing

else instance Fail (Beside (Text "The sum has no constructor named ") (QuoteLabel name)) => Pick name rep b where
  pick _ _ = Nothing

-- | The payload of one constructor: `ctor @"Name"`.
ctor :: forall @name a rep b. Generic a rep => Pick name rep b => a -> Maybe b
ctor x = pick (Proxy :: Proxy name) (from x)

-- | One handler per constructor: the record a total match over a sum needs.
class HandlerRow :: Type -> Type -> Row Type -> Constraint
class HandlerRow rep res row | rep res -> row

instance (HandlerRow a res l, HandlerRow b res r, Row.Union l r row) => HandlerRow (Sum a b) res row

instance Row.Cons name (b -> res) () row => HandlerRow (Constructor name (Argument b)) res row

-- | Applying the handler the value's constructor names.
class Handles :: Type -> Row Type -> Type -> Constraint
class Handles rep row res | rep row -> res where
  handles :: Record row -> rep -> res

instance (Handles a row res, Handles b row res) => Handles (Sum a b) row res where
  handles r = case _ of
    Inl x -> handles r x
    Inr x -> handles r x

instance (IsSymbol name, Row.Cons name (b -> res) t row) => Handles (Constructor name (Argument b)) row res where
  handles r (Constructor (Argument b)) = Record.get (Proxy :: Proxy name) r b

-- | The values of a record with one field per constructor of an enum, in constructor order. A new
-- | constructor fails to compile until its field is written.
class EnumFields :: Type -> Row Type -> Type -> Constraint
class EnumFields rep row res | rep row -> res where
  enumFields :: Proxy rep -> Record row -> Array res

instance (EnumFields a row res, EnumFields b row res) => EnumFields (Sum a b) row res where
  enumFields _ r = enumFields (Proxy :: Proxy a) r <> enumFields (Proxy :: Proxy b) r

instance (IsSymbol name, Row.Cons name res t row) => EnumFields (Constructor name NoArguments) row res where
  enumFields _ r = [ Record.get (Proxy :: Proxy name) r ]
