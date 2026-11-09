module Test.Core.Main where

import Prelude

import Effect (Effect)
import Test.Core.Kernel (kernel)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] kernel
