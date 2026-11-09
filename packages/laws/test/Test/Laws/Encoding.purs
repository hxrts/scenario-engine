-- | Every current mechanism written as a kernel value, before the engine changes (work plan P3). The
-- | function over `RejectReason` is total, so a new reason cannot go unencoded.
module Test.Laws.Encoding (encoding) where

import Prelude

import Core.Kernel.Types (Account(..), Amount(..), Bond(..), Condition(..), Transaction, Effect(..), Guarded, Law, LawChange(..), Limit(..), Obligation, Operation(..), Party(..), Short(..), Spawn, Subject(..), Template, Trigger(..), noNext)
import Core.Num as Q
import Laws.Types (Capability(..), Id(..), Rule, ActorId, ResourceId, ExchangeId)
import Laws.World (Cause(..), Mechanism(..), RejectReason(..))
import Data.Array (length)
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Test.Laws.Build (q)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

type Q' = Unit
type Ob = Obligation Capability Rule Q' RejectReason
type Cp = Transaction Capability Rule Q' RejectReason Cause
type Gd = Guarded Capability Q' RejectReason
type Lw = Law Capability Q' RejectReason Cause

a :: ActorId
a = Id "A"

b :: ActorId
b = Id "B"

mkt :: ExchangeId
mkt = Id "EM"

money :: ResourceId
money = Id "M"

good :: ResourceId
good = Id "E"

cause :: Cause
cause = OfIntent { candidateTransaction: Id "r1/A/0", intent: 0, mechanism: Direct }

leg :: Account -> Account -> ResourceId -> String -> Effect Rule Q'
leg from to resource x = Move { from, to, resource, owed: Exactly (q x) }

ob :: String -> ActorId -> Effect Rule Q' -> Array Gd -> Short -> Ob
ob id who effect when onShort = { id, obligor: Named who, effect, when, onShort, next: noNext }

gd :: Condition Capability Q' -> RejectReason -> Gd
gd condition r = { condition, reason: Just r }

transaction :: Array Ob -> Int -> Boolean -> Cp
transaction obligations tier whole = { obligations, ration: { tier, whole, scaleGroup: if whole then Nothing else Just a }, phase: 1, cause: Just cause, consented: [ a, b ] }

held :: ActorId -> Account
held = HeldBy <<< Named

trade :: Cp
trade = transaction
  [ ob "base" a (leg (Reserve mkt) (held a) good "2") [ gd (PriceWithin (AtMost (q "3"))) LimitPriceNotMet, { condition: Performed { index: 1, bond: Proportional (q "1") }, reason: Nothing } ] Reject
  , ob "quote" a (leg (held a) (Reserve mkt) money "2") [ { condition: Performed { index: 0, bond: Proportional (q "1") }, reason: Nothing } ] Reject
  ]
  1
  false

route :: Cp
route = trade

transform :: Cp
transform = transaction
  [ ob "input" a (leg (held a) Destroyed good "4") [ gd (StockAtLeast { stock: { at: held a, resource: good }, amount: Exactly (q "4") }) (InputsNotHeld (Id "t")) ] Reject
  , ob "output" a (leg Created (held a) (Id "G") "2") [ { condition: Performed { index: 0, bond: Whole }, reason: Nothing }, gd (QuantityAtLeast (Exactly (q "1"))) (BelowMinimum (Id "t")), gd (Holds { party: Named a, capability: MayTransform (Id "t") }) (NotAuthorized a) ] Reject
  , ob "payment" a (leg (held a) (held b) money "6") [] Cap
  ]
  1
  false

ownerSigned :: Cp
ownerSigned = transform { obligations = map (\o -> o { when = o.when <> [ gd (Consent [ b ]) (OwnerMustSign (Id "t")) ] }) transform.obligations }

transfer :: Cp
transfer = transaction [ ob "t" a (leg (held a) (held b) money "5") [] Cap ] 1 false

joint :: Cp
joint = transaction
  [ ob "x" a (leg (held a) (held b) money "5") [ { condition: Performed { index: 1, bond: Whole }, reason: Nothing }, gd (Consent [ a, b ]) PartyCannotFund ] Reject
  , ob "y" b (leg (held b) (held a) good "1") [ { condition: Performed { index: 0, bond: Whole }, reason: Nothing }, gd (Consent [ a, b ]) PartyCannotFund ] Reject
  ]
  0
  true

procedure :: Cp
procedure = transaction [ ob "x" a (leg (held a) (held b) money "5") [ { condition: WeightedConsent { weights: Map.singleton b (q "0.5"), residual: Q.zero, threshold: q "0.5" }, reason: Nothing } ] Cap ] 1 false

changeRule :: Cp
changeRule = transaction [ ob "c" a (Change (RemoveLaw "r1/A/0-0")) [] Reject ] 1 false

deliver :: Template Capability Rule Q' RejectReason Cause
deliver =
  { params: [ "quantity" ]
  , parties: [ "holder" ]
  , transaction: { obligations: [ { id: "d", obligor: Bound "holder", effect: Move { from: Created, to: HeldBy (Bound "holder"), resource: good, owed: Arg "quantity" }, when: [], onShort: Cap, next: noNext } ], ration: { tier: 0, whole: true, scaleGroup: Nothing }, phase: 0, cause: Nothing, consented: [] }
  }

reversal :: Template Capability Rule Q' RejectReason Cause
reversal = deliver
  { transaction
      { obligations =
          [ { id: "r", obligor: Bound "holder", effect: Move { from: HeldBy (Bound "holder"), to: Destroyed, resource: good, owed: Arg "quantity" }, when: [ { condition: StockAtLeast { stock: { at: HeldBy (Bound "holder"), resource: good }, amount: Exactly (q "0.0001") }, reason: Nothing } ], onShort: Cap, next: noNext } ]
      }
  }

spawned :: Spawn Q'
spawned = { template: "deliver", due: 2, args: Map.singleton "quantity" (Exactly (q "3")), parties: Map.singleton "holder" (Named a) }

lawCause :: Cause
lawCause = OfRule { kind: Id "tariff" }

levy :: Lw
levy = { subject: OfFlow { from: [ a ], to: [ b ], resources: [ good ] }, trigger: OnAction { obligor: Nothing, cause: Nothing }, operation: Emit { from: held a, to: Destroyed, resource: money, owed: Times { ratio: q "0.5", amount: Arg "quantity" } }, guard: [], phase: 3, cause: lawCause }

block :: Lw
block = levy { operation = Refuse }

gate :: Lw
gate = levy { operation = Refuse, guard = [ { condition: Performed { index: 1, bond: Proportional (q "1") }, reason: Just MissingBundledOrder } ] }

recurringLaw :: Lw
recurringLaw = levy { subject = OfStock { holders: Just [ a ], resource: money }, trigger = EachRound, operation = Emit { from: held a, to: held b, resource: money, owed: Exactly (q "20") }, phase = 4 }

decay :: Lw
decay = recurringLaw { subject = OfStock { holders: Nothing, resource: good }, operation = Emit { from: HeldBy (Bound "holder"), to: Destroyed, resource: good, owed: Times { ratio: q "0.1", amount: Stock { at: HeldBy (Bound "holder"), resource: good } } }, phase = 5 }

withhold :: Lw
withhold = recurringLaw { operation = Withhold (Exactly (q "2")) }

-- | The offset: a recipient's accrued shortfall, shared pro rata over its offsetable flows, using only amount arithmetic.
offset :: Amount Q'
offset = Minus { left: Exactly (q "20"), right: ShareOf { limit: Least { left: Exactly (q "20"), right: Times { ratio: q "2", amount: Read unit } }, among: [ Exactly (q "20") ], index: 0 } }

data Fate = OnCondition | BySolver | AtCompile

derive instance Eq Fate
derive instance Generic Fate _
instance Show Fate where
  show = genericShow

-- | Where each reason is decided. Compile refusals are validation of the authored vocabulary.
reasonFate :: RejectReason -> Fate
reasonFate = case _ of
  PoolExceeded -> BySolver
  LimitPriceNotMet -> OnCondition
  QuoteReservesShort -> BySolver
  InputsNotHeld _ -> OnCondition
  PartyCannotFund -> BySolver
  JointBundlesOnlyTransfers -> AtCompile
  DuplicateCandidateTransactionId -> AtCompile
  OutsideProcedureAuthority -> AtCompile
  UnknownProcedure -> AtCompile
  UnknownActor _ -> AtCompile
  MustSign _ -> OnCondition
  NotAuthorized _ -> OnCondition
  UnknownExchange _ -> AtCompile
  UnknownResource _ -> AtCompile
  UnknownTransformation _ -> AtCompile
  UnknownRule _ -> AtCompile
  RouteNeedsLimit -> AtCompile
  NoExchangeAccess _ _ -> OnCondition
  NoRouteAccess _ _ _ -> OnCondition
  NotTransferable _ -> OnCondition
  OutOfScope _ -> AtCompile
  OnlyGoverningRepeal -> AtCompile
  BelowMinimum _ -> OnCondition
  AboveMaximum _ -> AtCompile
  OwnerMustSign _ -> OnCondition
  -- a gate's bundling compares the candidate Transaction's own quantities, before anything has been filled
  MissingBundledOrder -> AtCompile
  Blocked _ -> OnCondition
  UnknownAgreement _ -> AtCompile
  MissingArgument _ -> AtCompile
  MissingParty _ -> AtCompile

encoding :: Spec Unit
encoding = describe "Every mechanism encodes in the kernel" do
  it "transactions: each is a distinct value of one type" do
    let rows = [ trade, route, transform, ownerSigned, transfer, joint, procedure, changeRule ]
    map (length <<< _.obligations) rows `shouldEqual` [ 2, 2, 3, 3, 1, 2, 1, 1 ]
  it "a joint binds each side to the other, whole, in the first tier" do
    joint.ration `shouldEqual` { tier: 0, whole: true, scaleGroup: Nothing }
  it "shortage law 1 is one scale group per Actor; the single-signer tier scales" do
    map _.ration.scaleGroup [ transfer, trade ] `shouldEqual` [ Just a, Just a ]
  it "a template spawns at least one round on, and binds only named parameters" do
    (spawned.due >= 1) `shouldEqual` true
    (Array.fromFoldable (Map.keys spawned.parties)) `shouldEqual` deliver.parties
    length reversal.transaction.obligations `shouldEqual` 1
  it "laws: levy, block, gate, recurring flow, decay, withholding share one form" do
    map _.phase [ levy, block, gate, recurringLaw, decay, withhold ] `shouldEqual` [ 3, 3, 3, 4, 5, 4 ]
  it "the offset is amount arithmetic" do
    case offset of
      Minus { right: ShareOf s } -> s.index `shouldEqual` 0
      _ -> (false `shouldEqual` true)
  it "every RejectReason has a home" do
    map reasonFate [ PoolExceeded, LimitPriceNotMet, DuplicateCandidateTransactionId ] `shouldEqual` [ BySolver, OnCondition, AtCompile ]
