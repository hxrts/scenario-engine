-- | The static simulation suite (docs/07 §3): every simulated run a composition offers, as data. It does no IO; the
-- | CLI's `suite` command plays the entries whose cache key is not stored (`Cli.Suite`).
-- |
-- | - **Preset runs.** One run per preset, with every configured seat cast to it.
-- | - **Agency runs.** The default preset's run with one castable seat deviating to its alternative preset (the
-- |   configuration's `alternatives`), one run per seat: the agency analysis (docs/07 §10.3) as suite entries. Each names the preset run it
-- |   deviates from, its baseline.
-- |
-- | Every entry plays at the composition's configured rounds and game seed, under one fixed player seed.
module Premises.Suite
  ( Entry
  , EntryKind(..)
  , suiteOf
  , suitePlayerSeed
  , entryText
  , Plan
  , planSuite
  ) where

import Prelude

import Core.Map (values)

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

import Assembly.Assemble (configuredSeats, defaultCast)
import Assembly.Composition (CompositionParts, presetNames, presetOr)
import Core.Shape (Format(..), encodeWith)
import Data.Argonaut.Core (stringify)
import Data.Array (catMaybes, concatMap, elem, filter, length, notElem, nub)
import Data.Array as Array
import Data.Maybe (maybe)
import Data.Newtype (unwrap)
import Foreign.Object as Object
import Schema.Premise.StrategyRules (presetsShape)
import Data.Tuple (Tuple(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Laws.Types (ActorId, idText)
import Schema.Cast (Cast, Role(..), castShape)

data EntryKind
  = PresetRun String -- the preset every seat plays
  | AgencyRun { actor :: ActorId, alternative :: String, baseline :: String } -- the seat that deviates, to what, and the preset run it deviates from

derive instance Eq EntryKind
derive instance Generic EntryKind _

instance Show EntryKind where
  show = genericShow

-- | One simulated run of the suite: its name, what kind of run it is, its cast, and the preset it names every seat on
-- | when it is one preset's run.
type Entry = { name :: String, kind :: EntryKind, cast :: Cast, preset :: Maybe String }

-- | The player seed every suite run plays under, so the suite is deterministic.
suitePlayerSeed :: Int
suitePlayerSeed = 1

-- | The composition's suite: its preset runs in name order, then one agency run per seat with an alternative, in id order.
suiteOf :: CompositionParts -> Array Entry
suiteOf p = map presetRun (presetNames p) <> agency
  where
  baseline = presetOr "" p
  presetRun name = { name, kind: PresetRun name, cast: defaultCast p name, preset: Just name }
  agency = map (\(Tuple e alt) -> agencyRun alt e) (filter (\(Tuple e alt) -> alt /= baseline && elem e (configuredSeats p.config)) (Map.toUnfoldable p.config.alternatives))
  agencyRun alt e =
    { name: baseline <> " with " <> idText e <> " on " <> alt
    , kind: AgencyRun { actor: e, alternative: alt, baseline }
    , cast: Map.insert e (CastPreset alt) (defaultCast p baseline)
    , preset: Nothing
    }

-- | The text an entry's cache key covers besides the engine and the composition's files (presets apart): its cast,
-- | the preset each cast seat or non-player plays, its rounds and seeds. Two entries with the same text play the same
-- | run, so a change to one preset replays only the entries that use it.
entryText :: CompositionParts -> Entry -> String
entryText p e = joinWith "|" [ stringify (encodeWith Snapshot castShape e.cast), stringify (encodeWith Authored presetsShape used), show p.config.rounds, show (unwrap p.config.seed), show suitePlayerSeed ]
  where
  names = nub (Array.mapMaybe presetOf (values e.cast))
  used = Array.mapMaybe (\name -> Tuple name <$> Object.lookup name p.presets) names
  presetOf = case _ of
    CastPreset name -> Just name
    CastNonplayer name -> Just name
    _ -> Nothing

-- | What the suite does to a store: the entries to play, each with its cache key; how many are reused; and the stored
-- | simulated runs to delete, those whose key the suite no longer has (or that have none).
type Plan = { play :: Array { parts :: CompositionParts, entry :: Entry, key :: String }, reused :: Int, drop :: Array String }

-- | The plan for every composition, given a digest, the engine build's digest, each composition with its files' text
-- | (presets apart), and the stored simulated runs with their keys.
planSuite :: (String -> String) -> String -> Array { parts :: CompositionParts, text :: String } -> Array { id :: String, key :: Maybe String } -> Plan
planSuite digest engine compositions stored =
  { play: filter (\x -> notElem x.key storedKeys) keyed
  , reused: length (filter (\x -> elem x.key storedKeys) keyed)
  , drop: map _.id (filter (\s -> maybe true (\k -> notElem k wanted) s.key) stored)
  }
  where
  keyed = concatMap (\c -> map (\entry -> { parts: c.parts, entry, key: digest (joinWith "\n" [ engine, c.text, entryText c.parts entry ]) }) (suiteOf c.parts)) compositions
  storedKeys = catMaybes (map _.key stored)
  wanted = map _.key keyed
