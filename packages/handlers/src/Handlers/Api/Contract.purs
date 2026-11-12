-- | What each operation of the API answers (docs/07_simulator.md §8, docs/08_architecture.md §6): its reply type,
-- | reply Shape, format and success status, and the refusals it may make, one entry per constructor of `ApiRequest`.
-- | The server dispatches through this module (`dispatch`), the client decodes through it (`Handlers.Api.Client.send`),
-- | and the OpenAPI documents read it (`operations`), so a reply cannot differ from its statement. A refusal's status
-- | is `Refusal`, reflected from the operation's row of labels; a refusal an operation does not declare does not compile.
module Handlers.Api.Contract
  ( Op
  , Operations
  , AnyRefusal
  , SessionState
  , SessionStarted
  , Created
  , Entry
  , class EntryOf
  , entryOf
  , class OperationRow
  , class Answers
  , answerOf
  , class Refusal
  , refusalStatus
  , class Refusals
  , statuses
  , refuse
  , class Describe
  , describeOf
  , dispatch
  , request
  , Described
  , operations
  ) where

import Prelude

import Core.Shape (class Exhausts, class Inject, Desc, Format(..), class Payload, Shape, inject, toPayload)
import Data.Argonaut.Core (Json)
import Data.Either (Either, either)
import Data.Generic.Rep (class Generic, Constructor(..), Sum(..), from, to)

import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Variant (Variant)
import Data.Variant as V
import Effects.Api (ApiRequest, Reply)

import Handlers.Api.Route (failWith, replyIn)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Schema.Run (RunStatus)
import Type.Proxy (Proxy(..))
import Views.Actor (ActorList, History)
import Views.BalanceSheet (BalanceSheet, BalanceSheetLink)

import Views.Compare (CompareView)
import Views.Glossary (Glossary)
import Views.Keys (class ViewShape, viewShape)
import Views.Overview (RunList)
import Views.Composition (CompositionInfo)
import Views.Report (RoundReport)

-- | One operation's reply type and refusal labels: 200 in the `Snapshot` format.
data Op :: Type -> Row Type -> Type
data Op a e

-- | One operation answering 201 in the `Authored` format, which leaves out an absent optional field.
data Created :: Type -> Row Type -> Type
data Created a e

-- | What an entry reads as at runtime: the Shape found from its reply type, and its status and format.
type Entry a = { reply :: Shape a, status :: Int, format :: Format }

class EntryOf :: Type -> Type -> Row Type -> Constraint
class EntryOf op a e | op -> a e where
  entryOf :: Proxy op -> Entry a

instance ViewShape a => EntryOf (Op a e) a e where
  entryOf _ = { reply: viewShape, status: 200, format: Snapshot }

instance ViewShape a => EntryOf (Created a e) a e where
  entryOf _ = { reply: viewShape, status: 201, format: Authored }

-- | The refusals of an operation nobody outside this repository depends on.
type AnyRefusal = (badRequest :: String, notFound :: String, conflict :: String, internal :: String)

type SessionState = { id :: String, tag :: String, round :: Int, status :: RunStatus, paused :: Boolean, ending :: Boolean, window :: Int, submitted :: Array String, awaiting :: Array String }

type SessionStarted = { id :: String, balanceSheets :: Array BalanceSheetLink }

-- | Every operation's entry, by constructor name: the one statement of its reply type and refusals.
type Operations =
  ( "ListRuns" :: Op RunList AnyRefusal
  , "Premise" :: Op (Array CompositionInfo) AnyRefusal
  , "Glossary" :: Op Glossary AnyRefusal
  , "Actors" :: Op ActorList AnyRefusal
  , "Actor" :: Op History AnyRefusal
  , "BalanceSheetLinks" :: Op (Array BalanceSheetLink) AnyRefusal
  , "BalanceSheet" :: Op BalanceSheet AnyRefusal
  -- which Shape a run view answers with depends on the view, so its reply is encoded JSON
  , "RunView" :: Op Json AnyRefusal
  , "RoundReport" :: Op RoundReport (notFound :: String, internal :: String)
  , "OpenApi" :: Op Json AnyRefusal
  , "Compare" :: Op CompareView AnyRefusal
  , "StartLive" :: Op SessionStarted (badRequest :: String, conflict :: String)
  , "GetLive" :: Op SessionState (notFound :: String)
  , "SubmitTranscript" :: Op SessionState (badRequest :: String, notFound :: String, conflict :: String)
  , "SubmitManual" :: Op SessionState (badRequest :: String, notFound :: String, conflict :: String)
  , "AdvanceLive" :: Op SessionState (notFound :: String, conflict :: String)
  , "PauseLive" :: Op SessionState (notFound :: String, conflict :: String)
  , "ResumeLive" :: Op SessionState (notFound :: String, conflict :: String)
  , "EndLive" :: Op SessionState (notFound :: String, conflict :: String)
  , "ClearLive" :: Op { cleared :: String } (notFound :: String)
  )

-- | The status of a refusal, by its label: the one place a status is written. Each carries the message.
class Refusal :: Symbol -> Constraint
class Refusal label where
  refusalStatus :: Proxy label -> Int

instance Refusal "badRequest" where
  refusalStatus _ = 400

instance Refusal "notFound" where
  refusalStatus _ = 404

instance Refusal "conflict" where
  refusalStatus _ = 409

instance Refusal "internal" where
  refusalStatus _ = 500

-- | A row of refusal labels, read: the statuses it can answer with, and the reply for one refusal.
class Refusals :: RL.RowList Type -> Row Type -> Constraint
class Refusals rl row | rl -> row where
  statuses :: Proxy rl -> Array Int
  refuse :: Proxy rl -> Variant row -> Reply

instance Refusals RL.Nil () where
  statuses _ = []
  refuse _ = V.case_

instance (IsSymbol label, Refusal label, Row.Cons label String tail row, Refusals rest tail) => Refusals (RL.Cons label String rest) row where
  statuses _ = [ refusalStatus (Proxy :: Proxy label) ] <> statuses (Proxy :: Proxy rest)
  refuse _ = V.on (Proxy :: Proxy label) (failWith (refusalStatus (Proxy :: Proxy label))) (refuse (Proxy :: Proxy rest))

-- | The record of implementations of a sum of operations: one field per constructor, from its payload to its reply
-- | or a refusal its entry declares. A constructor without an entry in `Operations` does not compile.
class OperationRow :: Type -> (Type -> Type) -> Row Type -> Constraint
class OperationRow rep m row | rep m -> row

instance (OperationRow a m l, OperationRow b m r, Row.Union l r row) => OperationRow (Sum a b) m row

instance
  ( Payload args p
  , Row.Cons name op t Operations
  , EntryOf op a e
  , Row.Cons name (p -> m (Either (Variant e) a)) () row
  ) =>
  OperationRow (Constructor name args) m row

-- | Running an operation by matching on its representation, encoding what its implementation returns.
class Answers :: Type -> (Type -> Type) -> Row Type -> Constraint
class Answers rep m impl where
  answerOf :: Record impl -> rep -> m Reply

instance (Answers a m impl, Answers b m impl) => Answers (Sum a b) m impl where
  answerOf impl = case _ of
    Inl x -> answerOf impl x
    Inr x -> answerOf impl x

instance
  ( IsSymbol name
  , Functor m
  , Payload args p
  , Row.Cons name op t Operations
  , EntryOf op a e
  , Row.Cons name (p -> m (Either (Variant e) a)) s impl
  , RL.RowToList e rl
  , Refusals rl e
  ) =>
  Answers (Constructor name args) m impl where
  answerOf impl (Constructor args) = either (refuse (Proxy :: Proxy rl)) (\a -> replyIn entry.format entry.reply entry.status a) <$> Record.get name impl (toPayload args)
    where
    name = Proxy :: Proxy name

    entry = entryOf (Proxy :: Proxy op)

-- | Answer a request: its operation's implementation, encoded and refused as its entry states.
dispatch :: forall rep m row. Generic ApiRequest rep => Exhausts rep Operations () => OperationRow rep m row => Answers rep m row => Record row -> ApiRequest -> m Reply
dispatch impl = answerOf impl <<< from

-- | An operation's entry, as a published description reads it.
type Described = { name :: String, reply :: Desc, status :: Int, refusals :: Array Int }

class Describe :: Type -> Constraint
class Describe rep where
  describeOf :: Proxy rep -> Array Described

instance (Describe a, Describe b) => Describe (Sum a b) where
  describeOf _ = describeOf (Proxy :: Proxy a) <> describeOf (Proxy :: Proxy b)

instance
  ( IsSymbol name
  , Row.Cons name op t Operations
  , EntryOf op a e
  , RL.RowToList e rl
  , Refusals rl e
  ) =>
  Describe (Constructor name args) where
  describeOf _ = [ { name: reflectSymbol name, reply: entry.reply.desc, status: entry.status, refusals: statuses (Proxy :: Proxy rl) } ]
    where
    name = Proxy :: Proxy name

    entry = entryOf (Proxy :: Proxy op)

describing :: forall rep. Generic ApiRequest rep => Exhausts rep Operations () => Describe rep => Proxy ApiRequest -> Array Described
describing _ = describeOf (Proxy :: Proxy rep)

-- | Every operation's entry, in the order of `ApiRequest`.
operations :: Array Described
operations = describing (Proxy :: Proxy ApiRequest)

-- | The operation of a constructor's name, with its entry.
request
  :: forall @name rep p op a e t
   . IsSymbol name
  => Generic ApiRequest rep
  => Inject name rep p
  => Row.Cons name op t Operations
  => EntryOf op a e
  => p
  -> { request :: ApiRequest, op :: Entry a }
request p = { request: to (inject (Proxy :: Proxy name) p), op: entryOf (Proxy :: Proxy op) }
