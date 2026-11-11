-- | The impure vocabulary (docs/02_kernel.md §8.4): the effect rows, their functor types, and the
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
  , _seatInput
  , _record
  , _interpret
  , _observe
  , _store
  , OBSERVE
  , ObserveF(..)
  , STORE
  , StoreF(..)
  , Stored
  , RunError(..)
  , NewRun
  , RunInfo
  , RunSummary
  , StoredManifest
  , StoredRound
  , RunSet
  , CLOCK
  , ClockF(..)
  , _clock
  , ENTROPY
  , EntropyF(..)
  , _entropy
  , PROCESS
  , ProcessF(..)
  , ProcessIdentity
  , _process
  , FILES
  , FilesF(..)
  , FileError
  , _files
  , CONSOLE
  , ConsoleF(..)
  , _console
  , CACHE
  , CacheF(..)
  , _cache
  , PLAY
  , PlayF(..)
  , PlayRequest(..)
  , PlayOutcome
  , _play
  , LIVE
  , LiveF(..)
  , SESSIONS
  , SessionsF(..)
  , SessionState
  , SubmitResult(..)
  , _sessions
  , LiveRequest
  , _live
  ) where

import Prelude

import Laws.Invariants (Failure)
import Laws.CandidateTransaction (CandidateTransactionBody)
import Schema.Joint (JointCandidateTransaction, Transcript)
import Schema.Run (RunStatus, Spec)
import Core.Draw (Seed)
import Laws.Engine.Commit (EngineError)
import Laws.Types (ActorId, CandidateTransactionId, RoundIndex)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Assembly.Types (PreparedRun)
import Laws.World (Manifest, RoundRecord, RoundResult, View, WorldState)
import Type.Proxy (Proxy(..))

-- | A seat's candidate Transactions in Propose, its signatures in Sign, and its transcript for the round in
-- | Speak (the seat's words, read by the round's joint pass; Nothing when it says nothing).
data SeatInputF a
  = Propose ActorId View (Array CandidateTransactionBody -> a)
  | Sign ActorId View (Array CandidateTransactionId -> a)
  | Speak ActorId View (Maybe String -> a)

derive instance Functor SeatInputF

type SEAT_INPUT r = (seatInput :: SeatInputF | r)

-- | Append a round's result to the run's records, with the seat transcripts its joint pass read (none
-- | when no seat spoke, and in simulation, where no seat speaks).
data RecordF a = Record RoundResult (Array Transcript) a

derive instance Functor RecordF

type RECORD r = (record :: RecordF | r)

-- | One joint pass over a round's transcripts (live play): the candidate Transactions they reconcile into. A
-- | reply that does not decode is a `Left`, with the reason.
data InterpretF a = Interpret RoundIndex (Array Transcript) (Either String (Array JointCandidateTransaction) -> a)

derive instance Functor InterpretF

type INTERPRET r = (interpret :: InterpretF | r)

-- | A stored value: the value, or why the stored text does not read back.
type Stored a = Either String a

-- | Why a stored run cannot be served: it is not in the store, or it is there but unreadable.
data RunError = Missing | Unreadable String

-- | A run registered before Round 1. Its `source` is read from its specification.
type NewRun =
  { id :: String
  , name :: String
  , created :: String
  , spec :: Spec
  , manifest :: Manifest
  , owner :: String -- the process that created the run (`Effects.System.ownerOf`): role, process id, start time
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

-- | A named set of runs to compare: their ids and which is the baseline. It never holds run data.
type RunSet = { name :: String, baseline :: String, runs :: Array String }

-- | The Run Store's writes (docs/07_simulator.md §8): the run lifecycle the server, the CLI, and `finish`
-- | make. Each round's own write is `RECORD`. Each write is one dbDbTransaction.
data StoreF a
  = CreateRun NewRun (Unit -> a)
  | SetStatus String RunStatus (Maybe String) (Unit -> a) -- the run, its new status, and why it failed if it did
  | FailRunning { role :: String, reason :: String } (Int -> a)
  | SetSpec String Spec (Unit -> a)
  | SetArchived String Boolean (Unit -> a)
  | DeleteRun String (Int -> a)
  | CreateSet RunSet (Unit -> a)
  | RenameSet String String (Int -> a) -- the set, its new name; how many sets were renamed
  | DeleteSet String (Int -> a)

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
  | ListSets (Array RunSet -> a)

derive instance Functor ObserveF

type OBSERVE r = (observe :: ObserveF | r)

-- One label per row, so a handler and its smart constructors name the row the same way.

_seatInput :: Proxy "seatInput"
_seatInput = Proxy

_record :: Proxy "record"
_record = Proxy

_interpret :: Proxy "interpret"
_interpret = Proxy

_observe :: Proxy "observe"
_observe = Proxy

_store :: Proxy "store"
_store = Proxy

-- | The wall clock: now, as ISO text.
data ClockF a = Now (String -> a)

derive instance Functor ClockF

type CLOCK r = (clock :: ClockF | r)

_clock :: Proxy "clock"
_clock = Proxy

-- | The OS's entropy: one fresh number, drawn only for a player seed nobody gave. In-world randomness is
-- | the pure keyed `Core.Draw.draw` and is not an operation.
data EntropyF a = DrawSeed (Int -> a)

derive instance Functor EntropyF

type ENTROPY r = (entropy :: EntropyF | r)

_entropy :: Proxy "entropy"
_entropy = Proxy

-- | Who this process is: its id and its start time, as text.
type ProcessIdentity = { pid :: String, started :: String }

data ProcessF a = Identify (ProcessIdentity -> a)

derive instance Functor ProcessF

type PROCESS r = (process :: ProcessF | r)

_process :: Proxy "process"
_process = Proxy

-- | Why a path could not be read.
type FileError = { path :: String, reason :: String }

-- | Files and the environment: a variable, a directory's names, a file's text, and writing a file, its
-- | directory made first.
data FilesF a
  = Env String (Maybe String -> a)
  | List String (Either FileError (Array String) -> a)
  | ReadFile String (Either FileError String -> a)
  | WriteFile String String a

derive instance Functor FilesF

type FILES r = (files :: FilesF | r)

_files :: Proxy "files"
_files = Proxy

-- | The terminal: a line read after its question, a line written, and the end of input.
data ConsoleF a
  = ReadLine String (String -> a)
  | WriteLine String a
  | CloseInput a

derive instance Functor ConsoleF

type CONSOLE r = (console :: ConsoleF | r)

_console :: Proxy "console"
_console = Proxy

-- | What a process keeps between requests, by key: decoded runs, which a write forgets, and replies,
-- | which last. The row is generic in both because the types belong to the program that asks.
data CacheF run reply a
  = LookupRun String (Maybe run -> a)
  | KeepRun String run a
  | LookupReply String (Maybe reply -> a)
  | KeepReply String reply a
  | ForgetRuns a

derive instance Functor (CacheF run reply)

type CACHE run reply r = (cache :: CacheF run reply | r)

_cache :: Proxy "cache"
_cache = Proxy

-- | One whole stored play: a simulated game from its start, a game whose results are already known (a
-- | replay), or a stored run continued from its last snapshot.
data PlayRequest
  = Simulated { seed :: Seed, game :: PreparedRun, run :: NewRun }
  | Replayed { game :: PreparedRun, run :: NewRun, records :: Array RoundResult, value :: Either EngineError WorldState }
  | Continued { id :: String, seed :: Seed, game :: PreparedRun, world :: WorldState, done :: Int, spec :: Spec }

-- | A game that did not start, with nothing stored; or the played result, the engine's error rendered.
type PlayOutcome = Either String (Either String Unit)

-- | Playing is one operation because its lifecycle wraps the game: a failure anywhere in it is caught,
-- | the run is settled, and the error goes on. A program cannot express that around separate operations.
data PlayF a = Play PlayRequest (PlayOutcome -> a)

derive instance Functor PlayF

type PLAY r = (play :: PlayF | r)

_play :: Proxy "play"
_play = Proxy

-- | A live session of a run already registered: its game and id, what the seats are briefed with, how
-- | long a propose window lasts in seconds (0 for none), and the lines shown after each round.
type LiveRequest =
  { seed :: Seed
  , game :: PreparedRun
  , id :: String
  , window :: Int
  , briefing :: ActorId -> String
  , report :: RoundResult -> Array String
  }

-- | Play a live session and settle its run. The reply is why it failed, rendered, or `Right`.
data LiveF a = PlayLive LiveRequest (Either String Unit -> a)

derive instance Functor LiveF

type LIVE r = (live :: LiveF | r)

_live :: Proxy "live"
_live = Proxy

-- | One live session as a client sees it.
type SessionState = { round :: RoundIndex, status :: RunStatus, window :: Int, submitted :: Array ActorId, awaiting :: Array ActorId }

data SubmitResult = Accepted | NoSuchSession | NotASeat | NotThisRound RoundIndex | AlreadySubmitted | SessionOver

derive instance Eq SubmitResult

-- | Live sessions a server hosts, started once and fed text as it arrives.
data SessionsF a
  = Host { request :: LiveRequest, humans :: Array ActorId } a
  | State String (Maybe SessionState -> a)
  | Submit { id :: String, seat :: ActorId, round :: RoundIndex, text :: String } (SubmitResult -> a)

derive instance Functor SessionsF

type SESSIONS r = (sessions :: SessionsF | r)

_sessions :: Proxy "sessions"
_sessions = Proxy
