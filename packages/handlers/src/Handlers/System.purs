-- | The interpreters of the system rows, each over one device primitive. The composition root supplies the
-- | primitives; a test supplies fixed ones.
module Handlers.System
  ( SystemDevices
  , FileDevice
  , ConsoleDevice
  , interpretClock
  , interpretEntropy
  , interpretProcess
  , interpretFiles
  , interpretConsole
  ) where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Effects.Signatures (ClockF(..), ConsoleF(..), EntropyF(..), FileError, FilesF(..), ProcessF(..), ProcessIdentity)

-- | The small devices a program's launch reads.
type SystemDevices = { now :: Effect String, drawSeed :: Effect Int, identity :: Effect ProcessIdentity }

type FileDevice =
  { env :: String -> Effect (Maybe String)
  , list :: String -> Effect (Either FileError (Array String))
  , read :: String -> Effect (Either FileError String)
  , write :: String -> String -> Effect Unit
  }

type ConsoleDevice = { readLine :: String -> Aff String, writeLine :: String -> Effect Unit, closeInput :: Effect Unit }

interpretClock :: forall m. MonadEffect m => SystemDevices -> ClockF ~> m
interpretClock d (Now k) = liftEffect (k <$> d.now)

interpretEntropy :: forall m. MonadEffect m => SystemDevices -> EntropyF ~> m
interpretEntropy d (DrawSeed k) = liftEffect (k <$> d.drawSeed)

interpretProcess :: forall m. MonadEffect m => SystemDevices -> ProcessF ~> m
interpretProcess d (Identify k) = liftEffect (k <$> d.identity)

interpretFiles :: forall m. MonadEffect m => FileDevice -> FilesF ~> m
interpretFiles d = liftEffect <<< case _ of
  Env name k -> k <$> d.env name
  List path k -> k <$> d.list path
  ReadFile path k -> k <$> d.read path
  WriteFile path text next -> next <$ d.write path text

interpretConsole :: ConsoleDevice -> ConsoleF ~> Aff
interpretConsole d = case _ of
  ReadLine question k -> k <$> d.readLine question
  WriteLine line next -> next <$ liftEffect (d.writeLine line)
  CloseInput next -> next <$ liftEffect d.closeInput
