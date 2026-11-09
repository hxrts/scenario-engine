-- | Authoring agreements directly as couplings: obligations bound to conditions, and successors as
-- | templates (docs/03_gameboard.md). A loan, a treaty and a vesting schedule are patterns written here.
module Schema.Build.Agreement
  ( Agreement
  , Clause
  , Guard
  , Step
  , agreement
  , clause
  , due
  , guard
  , repeal
  , spawn
  , step
  , onAction
  , held
  ) where

import Core.Kernel.Types (Account(..), Amount(..), Condition, Coupling, Effect(..), LawChange(..), Next, Obligation, Operation(..), Party(..), Short(..), Subject(..), Trigger(..), Spawn, Template, noNext)
import Rules.Types (Capability, EntityId, ResourceId, Rule, RuleKindId, RuleId, idText)
import Core.Kernel.Law (holderParam, quantityParam)
import Core.Num (Q)
import Rules.World (AuthoredLaw(..), Cause(..), LegQuery, PendingCoupling(..), RejectReason, WorldTemplate(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

type Agreement = Coupling Capability Rule LegQuery RejectReason Cause
type Clause = Obligation Capability Rule LegQuery RejectReason
type Guard = { condition :: Condition Capability LegQuery, reason :: Maybe RejectReason }
type Step = Spawn LegQuery

-- | What a party holds.
held :: Party -> Account
held = HeldBy

-- | A condition that reports nothing when it fails.
guard :: Condition Capability LegQuery -> Guard
guard condition = { condition, reason: Nothing }

-- | One party's duty to move a quantity, with what it depends on, what a shortfall does, and what follows.
clause :: String -> Party -> { from :: Account, to :: Account, resource :: ResourceId, owed :: Amount LegQuery } -> Short -> Array Guard -> Next LegQuery -> Clause
clause id obligor leg onShort when next = { id, obligor, effect: Move leg, when, onShort, next }

-- | One party's duty to remove a law.
repeal :: String -> Party -> RuleId -> Array Guard -> Clause
repeal id obligor rule when = { id, obligor, effect: Change (RemoveLaw (idText rule)), when, onShort: Reject, next: noNext }

-- | Obligations settled together at a round boundary, signed by `signers`.
agreement :: RuleKindId -> Array EntityId -> Array Clause -> Agreement
agreement kind signers obligations =
  { obligations
  , holds: []
  , solver: Nothing
  , ration: { tier: 0, whole: true, scaleGroup: Nothing }
  , phase: 0
  , cause: Just (OfRule { kind })
  , consented: signers
  }

-- | An agreement falling due in `round`.
due :: Int -> Agreement -> PendingCoupling
due round coupling = PendingCoupling { due: round, coupling }

-- | A successor: its template, how many rounds on, and its frozen arguments and bound parties.
spawn :: String -> Int -> Array (Tuple String (Amount LegQuery)) -> Array (Tuple String Party) -> Step
spawn template offset args parties = { template, due: offset, args: Map.fromFoldable args, parties: Map.fromFoldable parties }

-- | A template, named for the World's table.
step :: String -> Array String -> Array String -> Agreement -> Tuple String WorldTemplate
step name params parties coupling = Tuple name (WorldTemplate ({ params, parties, coupling: coupling { cause = Nothing } } :: Template Capability Rule LegQuery RejectReason Cause))

-- | A law charged when the named action settles (a Market or a Facility, by its id), on the action's
-- | quantity, from whoever acts.
onAction :: RuleKindId -> String -> Account -> ResourceId -> Q -> AuthoredLaw
onAction kind action to resource rate = AuthoredLaw
  { subject: OfStock { holders: Nothing, resource }
  , trigger: OnAction { obligor: Nothing, cause: Just action }
  , operation: Emit { from: HeldBy (Bound holderParam), to, resource, owed: Times { ratio: rate, amount: Arg quantityParam } }
  , guard: []
  , phase: 1
  , cause: OfRule { kind }
  }
