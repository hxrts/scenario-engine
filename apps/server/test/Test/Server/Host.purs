-- | A session host for tests: a stub model that makes no billed call. Each seat says nothing, and each joint pass reads no input.
module Test.Server.Host (testHost, stubReply) where

import Prelude

import Effect.Unsafe (unsafePerformEffect)
import Handlers.Llm.Backend (backend)
import Handlers.Sessions (SessionHost, newSessionHost)
import Handlers.Store (StoreBackend)
import Views.Metrics (roundMetrics)

stubReply :: String
stubReply = "{\"text\":\"nothing\",\"candidateTransactions\":[]}"

testHost :: StoreBackend -> SessionHost
testHost store = unsafePerformEffect (newSessionHost { store, metricsOf: roundMetrics, llm: \_ -> backend { model: "stub", call: \_ _ -> pure { text: stubReply, raw: stubReply }, digest: identity } })
