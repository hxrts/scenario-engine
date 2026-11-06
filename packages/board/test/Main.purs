module Test.Board.Main where

import Prelude

import Effect (Effect)
import Test.Board.Golden (golden, orderIndependent, representation, unbalanced)
import Test.Spec (describe)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Golden fixtures" do
    golden [ "A", "A2", "B1", "B2", "B3", "B4", "B4-unsigned", "C1", "C2", "C3", "D", "D-minor", "F", "G1", "G2", "G3", "G3-ban", "G4", "G5", "G5-unsigned", "circulation" ]
    orderIndependent [ "A", "A2", "F", "G4" ]
    unbalanced
    representation
