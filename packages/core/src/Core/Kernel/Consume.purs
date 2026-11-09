-- | Consumption (docs/02_type_system.md): every obligation due in a round ends performed, short or
-- | lapsed, and its branch of `next` is instantiated at commit. Arguments freeze here, as plain
-- | values, and a successor can name only the parties its parent bound.
module Core.Kernel.Consume
  ( ConsumeError(..)
  , validSpawn
  , validateTemplates
  , partiesOf
  , spawned
  , due
  ) where

import Prelude

import Core.Kernel.Resolve (Consumed, Fate(..))
import Core.Kernel.Types (Account(..), Amount(..), Condition(..), Coupling, Effect(..), Next, Obligation, Party(..), Pending, Spawn, Template)

import Core.Map (entries)
import Core.Num as Q
import Core.Types (EntityId, RoundIndex)
import Data.Array (concat, concatMap, elem, index, nub, partition)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(..))

data ConsumeError
  = DueTooSoon String Int
  | UnknownTemplate String
  | OutsideParent String EntityId
  | MissingParty String String

derive instance Eq ConsumeError
derive instance Generic ConsumeError _
instance Show ConsumeError where
  show = genericShow

-- | A spawn is valid when it is due in a later round.
validSpawn :: forall query. Spawn query -> Either ConsumeError Unit
validSpawn s
  | s.due < 1 = Left (DueTooSoon s.template s.due)
  | otherwise = Right unit

-- | Refuse a template, at validation, that spawns for its own round.
validateTemplates :: forall cap dynamic query reason cause. Map String (Template cap dynamic query reason cause) -> Either ConsumeError Unit
validateTemplates = traverse_ (traverse_ checkNext <<< _.coupling.obligations)
  where
  checkNext o = traverse_ validSpawn (o.next.performed <> o.next.short <> o.next.lapsed)

-- | Everyone a coupling binds: its obligors, the holders its legs name, and its signers.
partiesOf :: forall cap dynamic query reason cause. Coupling cap dynamic query reason cause -> Array EntityId
partiesOf c = nub (c.consented <> concatMap ofObligation c.obligations)
  where
  ofObligation o = named o.obligor <> ofEffect o.effect
  ofEffect = case _ of
    Move l -> ofAccount l.from <> ofAccount l.to
    Change _ -> []
  ofAccount = case _ of
    HeldBy p -> named p
    _ -> []
  named = case _ of
    Named e -> [ e ]
    Bound _ -> []

-- | The pending obligations due by `round`, and the rest.
due :: forall cap dynamic query reason cause. RoundIndex -> Array (Pending cap dynamic query reason cause) -> { now :: Array (Pending cap dynamic query reason cause), later :: Array (Pending cap dynamic query reason cause) }
due round pending = case partition (\p -> p.due <= round) pending of
  { yes, no } -> { now: yes, later: no }

-- | What every consumed obligation spawns, due relative to `round`.
spawned
  :: forall cap dynamic query reason cause
   . RoundIndex
  -> Map String (Template cap dynamic query reason cause)
  -> Array (Coupling cap dynamic query reason cause)
  -> Array (Consumed dynamic reason)
  -> Either ConsumeError (Array (Pending cap dynamic query reason cause))
spawned round templates couplings consumed = concat <$> traverse one consumed
  where
  one c = case index couplings c.coupling of
    Nothing -> Right []
    Just parent -> case index parent.obligations c.obligation of
      Nothing -> Right []
      Just o -> traverse (instantiate round templates parent c) (branch c.fate o)

branch :: forall cap dynamic query reason. Fate -> Obligation cap dynamic query reason -> Array (Spawn query)
branch fate o = pick o.next
  where
  pick :: Next query -> Array (Spawn query)
  pick n = case fate of
    Fulfilled -> n.performed
    Shorted -> n.short
    Expired -> n.lapsed

instantiate
  :: forall cap dynamic query reason cause
   . RoundIndex
  -> Map String (Template cap dynamic query reason cause)
  -> Coupling cap dynamic query reason cause
  -> Consumed dynamic reason
  -> Spawn query
  -> Either ConsumeError (Pending cap dynamic query reason cause)
instantiate round templates parent c s = do
  validSpawn s
  template <- maybe (Left (UnknownTemplate s.template)) Right (Map.lookup s.template templates)
  bindings <- traverse (bindParty s.template) (entries s.parties)
  let
    parties = Map.fromFoldable bindings
    args = map (freeze c) s.args
  traverse_ (\p -> if Map.member p parties then Right unit else Left (MissingParty s.template p)) template.parties
  pure
    { due: round + s.due
    , coupling: substitute args parties parent template
    }
  where
  allowed = partiesOf parent
  bindParty t (Tuple name party) = case party of
    Named e | e `elem` allowed -> Right (Tuple name e)
    Named e -> Left (OutsideParent t e)
    Bound p -> Left (MissingParty t p)

-- | An argument's value when its obligation is consumed.
freeze :: forall dynamic query reason. Consumed dynamic reason -> Amount query -> Amount query
freeze c = go
  where
  moved = maybe Q.zero _.moved c.flow
  owed = maybe Q.zero _.owed c.flow

  go :: Amount query -> Amount query
  go = case _ of
    Moved -> Exactly moved
    Remainder -> Exactly (Q.sub owed moved)
    Times t -> Times t { amount = go t.amount }
    Plus p -> Plus { left: go p.left, right: go p.right }
    Minus p -> Minus { left: go p.left, right: go p.right }
    Least p -> Least { left: go p.left, right: go p.right }
    other -> other

-- | A template's coupling with its arguments and parties filled in, carrying its parent's consent.
substitute
  :: forall cap dynamic query reason cause
   . Map String (Amount query)
  -> Map String EntityId
  -> Coupling cap dynamic query reason cause
  -> Template cap dynamic query reason cause
  -> Coupling cap dynamic query reason cause
substitute args parties parent template = c
  { obligations = map obligation c.obligations
  , holds = map (\h -> h { party = party h.party, amount = amount h.amount }) c.holds
  , cause = case c.cause of
      Nothing -> parent.cause
      own -> own
  , consented = nub (parent.consented <> c.consented)
  }
  where
  c = template.coupling
  party = case _ of
    Bound p | Just e <- Map.lookup p parties -> Named e
    other -> other
  account = case _ of
    HeldBy p -> HeldBy (party p)
    other -> other

  amount :: Amount query -> Amount query
  amount = case _ of
    Arg p | Just a <- Map.lookup p args -> a
    Stock s -> Stock s { at = account s.at }
    Times t -> Times t { amount = amount t.amount }
    Plus p -> Plus { left: amount p.left, right: amount p.right }
    Minus p -> Minus { left: amount p.left, right: amount p.right }
    Least p -> Least { left: amount p.left, right: amount p.right }
    ShareOf s -> ShareOf s { limit = amount s.limit, among = map amount s.among }
    other -> other
  obligation o = o
    { obligor = party o.obligor
    , effect = case o.effect of
        Move l -> Move l { from = account l.from, to = account l.to, owed = amount l.owed }
        other -> other
    , when = map (\g -> g { condition = condition g.condition }) o.when
    , next = { performed: map spawn o.next.performed, short: map spawn o.next.short, lapsed: map spawn o.next.lapsed }
    }
  -- a successor's own successors are filled in too, so a template can spawn itself
  spawn sp = sp { args = map amount sp.args, parties = map party sp.parties }
  condition = case _ of
    StockAtLeast x -> StockAtLeast { stock: x.stock { at = account x.stock.at }, amount: amount x.amount }
    QuantityAtLeast a -> QuantityAtLeast (amount a)
    Holds h -> Holds h { party = party h.party }
    other -> other

