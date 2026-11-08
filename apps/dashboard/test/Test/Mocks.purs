-- | The Playwright mocks' replies (`e2e/mocks.js`), as the JSON text they answer with, so a suite can decode
-- | them through the reply shapes the real pages read.
module Test.Mocks where

foreign import runListJson :: String
foreign import overviewJson :: String
