-- | The impure vocabulary (docs/02_type_system.md §8.4): the effect rows, their functor types, and the
-- | typed records that travel with them. Signatures only; nothing here implements an effect. Handlers
-- | in `handlers` interpret them over a typed backend, and adapters supply the raw primitives. The
-- | STORE records stay here, not in `handlers/Store`, because every STORE constructor and reply names
-- | them and `effects` may not import `handlers`.
module Effects.Signatures
  ( SEAT_INPUT
  , SeatInputF(..)
  , RECORD
  , RecordF(..)
  , INTERPRET
  , InterpretF(..)
  , NARRATE
  , NarrateF(..)
  , _seatInput
  , _record
  , _interpret
  , _narrate
  , _observe
  , _store
  , OBSERVE
  , ObserveF(..)
  , STORE
  , StoreF(..)
  , Spec
  , Stored
  , RunError(..)
  , NewRun
  , RunInfo
  , RunSummary
  , StoredManifest
  , StoredRound
  ) where

import Prelude

import Core.Invariants (Failure)
import Core.Proposal (JointProposal, Transcript, ProposalBody)
import Core.Run (DeriveKind, RunStatus)
import Core.Types (EntityId, ProposalId, RoundIndex)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Core.World (Manifest, RoundRecord, RoundResult, View, WorldState)
import Foreign.Object (Object)
import Type.Proxy (Proxy(..))

-- | A seat's Proposals in Propose, its signatures in Sign, and its transcript for the round in
-- | Speak (the seat's words, read by the round's joint pass; Nothing when it says nothing).
data SeatInputF a
  = Propose EntityId View (Array ProposalBody -> a)
  | Sign EntityId View (Array ProposalId -> a)
  | Speak EntityId View (Maybe String -> a)

derive instance Functor SeatInputF

type SEAT_INPUT r = (seatInput :: SeatInputF | r)

-- | Append a round's result to the run's records, with the seat transcripts its joint pass read (none
-- | when no seat spoke, and in simulation, where no seat speaks).
data RecordF a = Record RoundResult (Array Transcript) a

derive instance Functor RecordF

type RECORD r = (record :: RecordF | r)

-- | One joint pass over a round's transcripts (live play): the Proposals they reconcile into. A
-- | reply that does not decode is a `Left`, with the reason.
data InterpretF a = Interpret RoundIndex (Array Transcript) (Either String (Array JointProposal) -> a)

derive instance Functor InterpretF

type INTERPRET r = (interpret :: InterpretF | r)

-- | Resolved events to the front page and balance-sheet text; narration adds no facts. Every round
-- | requests one; only live play answers with text (simulation and replay answer with the empty text).
data NarrateF a = Narrate RoundResult (String -> a)

derive instance Functor NarrateF

type NARRATE r = (narrate :: NarrateF | r)

-- | A run's specification: the string fields it was launched or derived with (the preset, the seeds,
-- | the overrides, and the source it was derived from). Each producer writes its own fields.
type Spec = Object String

-- | A stored value: the value, or why the stored text does not read back.
type Stored a = Either String a

-- | Why a stored run cannot be served: it is not in the store, or it is there but unreadable.
data RunError = Missing | Unreadable String

-- | A run registered before Round 1. `source` and `kind` are set for a run derived from another.
type NewRun =
  { id :: String
  , name :: String
  , created :: String
  , spec :: Spec
  , manifest :: Manifest
  , source :: Maybe String
  , kind :: Maybe DeriveKind
  , owner :: String -- the process that created the run (`Adapters.SQLite.processOwner`): role, process id, start time
  }

-- | A run's row for the run list: its status, and why it failed if it did.
type RunInfo = { id :: String, name :: String, status :: RunStatus, created :: String, reason :: Maybe String }

-- | A run's list row: its stored manifest and specification, and the assertion sets of its rounds.
-- | No records or snapshots. `source` is the run it was derived from, if any.
type RunSummary =
  { id :: String
  , name :: String
  , status :: RunStatus
  , created :: String
  , reason :: Maybe String
  , spec :: Stored Spec
  , manifest :: Stored Manifest
  , archived :: Boolean
  , source :: Maybe String
  , assertions :: Array (Stored (Array Failure))
  }

-- | A run's stored manifest and specification, as read back.
type StoredManifest = { spec :: Stored Spec, manifest :: Stored Manifest }

-- | One stored round: its record, snapshot, assertions, and the seat transcripts read for it.
type StoredRound =
  { round :: Int
  , record :: Stored RoundRecord
  , snapshot :: Stored WorldState
  , assertions :: Stored (Array Failure)
  , transcripts :: Stored (Array Transcript)
  }

-- | The Run Store's writes (docs/06_simulator.md §8): the run lifecycle the server, the CLI, and `finish`
-- | make. Each round's own write is `RECORD`. Each write is one transaction.
data StoreF a
  = CreateRun NewRun (Unit -> a)
  | SetStatus String RunStatus (Maybe String) (Unit -> a) -- the run, its new status, and why it failed if it did
  | FailRunning { role :: String, reason :: String } (Int -> a)
  | SetSpec String Spec (Unit -> a)
  | SetArchived String Boolean (Unit -> a)
  | DeleteRun String (Int -> a)

derive instance Functor StoreF

type STORE r = (store :: StoreF | r)

-- | The Run Store's reads (`OBSERVE`): what the server's read operations and the run list need. Reads
-- | return stored values as typed records, with unreadable fields as `Left`. A read program has this row
-- | only, so it cannot write.
data ObserveF a
  = ListRuns (Array RunInfo -> a)
  | ListSummaries (Array RunSummary -> a)
  | ReadManifest String (Maybe StoredManifest -> a)
  | ReadRounds String (Array StoredRound -> a)

derive instance Functor ObserveF

type OBSERVE r = (observe :: ObserveF | r)

-- One label per row, so a handler and its smart constructors name the row the same way.

_seatInput :: Proxy "seatInput"
_seatInput = Proxy

_record :: Proxy "record"
_record = Proxy

_interpret :: Proxy "interpret"
_interpret = Proxy

_narrate :: Proxy "narrate"
_narrate = Proxy

_observe :: Proxy "observe"
_observe = Proxy

_store :: Proxy "store"
_store = Proxy
