module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

greeting :: String
greeting = "Scenario Planning Game"

main :: Effect Unit
main = log greeting
