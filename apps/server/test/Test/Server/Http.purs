-- | The HTTP device (`apps/server/src/Http.js`) on a real socket: body assembly, the body limit, gzip,
-- | the static tree, and the cache headers.
module Test.Server.Http (spec, rawExec) where

import Prelude

import Data.Char (toCharCode)
import Data.Int as Int
import Data.String (joinWith, Pattern(..), contains)
import Data.String.CodeUnits (toCharArray, fromCharArray)

import Handlers.Sql (SqlDevice)
import Control.Promise (Promise, toAffE)
import Data.Array (replicate)
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Aff (Aff, bracket)
import Effect.Class (liftEffect)
import Effects.Api (ApiRequest, Reply)
import Foreign.Object (Object)
import Foreign.Object as Object
import Handlers.Api.Route (apiPrefix, route, Request)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

foreign import data Running :: Type
foreign import siteDir :: Effect String
foreign import startServerImpl :: String -> String -> (Request -> Effect Reply) -> Effect (Promise Running)
foreign import startSiteImpl :: String -> String -> { password :: String, session :: String } -> (Request -> Effect Reply) -> Effect (Promise Running)
foreign import portOf :: Running -> Int
foreign import stopServerImpl :: Running -> Effect (Promise Unit)
foreign import turnImpl :: Int -> String -> String -> Object String -> Array String -> Effect (Promise Answer)

-- | Run raw SQL against a store.
rawExec :: SqlDevice -> String -> Effect Unit
rawExec d = d.script

type Answer = { status :: Int, headers :: Object String, text :: String }

-- | A POST is echoed; every other request goes through the real route, over an operation that answers 200.
handler :: Request -> Effect Reply
handler r
  | r.method == "POST" = pure { status: 200, body: r.body }
  | otherwise = route answer r
      where
      answer :: ApiRequest -> Effect Reply
      answer _ = pure { status: 200, body: "{}" }

withServer :: forall a. (Int -> Aff a) -> Aff a
withServer test = do
  dir <- liftEffect siteDir
  bracket (toAffE (startServerImpl dir apiPrefix handler)) (\s -> toAffE (stopServerImpl s)) (test <<< portOf)

withSite :: forall a. (Int -> Aff a) -> Aff a
withSite = withSiteOf { password: "pw", session: "sess" }

withSiteOf :: forall a. { password :: String, session :: String } -> (Int -> Aff a) -> Aff a
withSiteOf admin test = do
  dir <- liftEffect siteDir
  bracket (toAffE (startSiteImpl dir apiPrefix admin handler)) (\s -> toAffE (stopServerImpl s)) (test <<< portOf)

-- | Text as the hex the test client writes.
hexOf :: String -> String
hexOf s = joinWith "" (map (\c -> Int.toStringAs Int.hexadecimal (toCharCode c)) (toCharArray s))

get :: Int -> String -> Array (Maybe String) -> Aff Answer
get port path accept = toAffE (turnImpl port "GET" path headers [])
  where
  headers = case accept of
    [ Just a ] -> Object.singleton "accept-encoding" a
    _ -> Object.empty

header :: String -> Answer -> Maybe String
header k a = Object.lookup k a.headers

spec :: Spec Unit
spec = describe "HTTP device" do
  it "a multi-byte character split across two chunks arrives whole" do
    withServer \port -> do
      -- "é€": the euro sign's three bytes are split between the chunks
      a <- toAffE (turnImpl port "POST" "/api/echo" Object.empty [ "c3a9e2", "82ac" ])
      a.status `shouldEqual` 200
      a.text `shouldEqual` "é€"
  it "a body over the limit is a 413" do
    withServer \port -> do
      a <- toAffE (turnImpl port "POST" "/api/echo" Object.empty [ oversized ])
      a.status `shouldEqual` 413
      a.text `shouldSatisfy` contains (Pattern "exceeds")
  it "replies are gzipped only for a client that accepts gzip" do
    withServer \port -> do
      zipped <- get port "/index.html" [ Just "gzip" ]
      header "content-encoding" zipped `shouldEqual` Just "gzip"
      zipped.text `shouldEqual` "<html>the index</html>"
      plain <- get port "/index.html" [ Nothing ]
      header "content-encoding" plain `shouldEqual` Nothing
      plain.text `shouldEqual` "<html>the index</html>"
  it "a path that climbs out of the static directory reads nothing outside it" do
    withServer \port -> do
      a <- get port "/..%2fsecret.txt" []
      a.text `shouldSatisfy` (not <<< contains (Pattern "the secret"))
  it "cache headers: bundle files immutable, the page revalidated, API replies never stored" do
    withServer \port -> do
      asset <- get port "/assets/app.js" []
      asset.status `shouldEqual` 200
      header "cache-control" asset `shouldEqual` Just "public, max-age=31536000, immutable"
      page <- get port "/" []
      header "cache-control" page `shouldEqual` Just "no-cache"
      api <- get port "/api/runs" []
      header "cache-control" api `shouldEqual` Just "no-store"
  it "content types cover the bundle's file kinds, and an unknown bundle file is a 404, not the page" do
    withServer \port -> do
      font <- get port "/assets/font.woff2" []
      header "content-type" font `shouldEqual` Just "font/woff2"
      missing <- get port "/assets/gone.js" []
      missing.status `shouldEqual` 404
      route' <- get port "/runs/run-1" []
      route'.status `shouldEqual` 200
      route'.text `shouldEqual` "<html>the index</html>"
  it "the public site: the landing page, a portfolio page, and the admin page behind its password" do
    withSite \port -> do
      landing <- get port "/" []
      landing.text `shouldEqual` "<html>the landing page</html>"
      (_.text <$> get port "/portfolio/run-1/abc" []) >>= (_ `shouldEqual` "<html>the index</html>")
      locked <- get port "/admin/" []
      locked.text `shouldSatisfy` contains (Pattern "password")
      locked.text `shouldSatisfy` (not <<< contains (Pattern "the index"))
      (_.status <$> get port "/runs/run-1" []) >>= (_ `shouldEqual` 404)
      bad <- toAffE (turnImpl port "POST" "/admin/login" Object.empty [ hexOf "password=wrong" ])
      bad.status `shouldEqual` 401
      good <- toAffE (turnImpl port "POST" "/admin/login" Object.empty [ hexOf "password=pw" ])
      good.status `shouldEqual` 303
      header "set-cookie" good `shouldSatisfy` maybe false (contains (Pattern "admin=sess"))
      opened <- toAffE (turnImpl port "GET" "/admin/" (Object.singleton "cookie" "admin=sess") [])
      opened.text `shouldEqual` "<html>the index</html>"
  it "a site with no password set admits nobody" do
    withSiteOf { password: "", session: "" } \port -> do
      empty <- toAffE (turnImpl port "POST" "/admin/login" Object.empty [ hexOf "password=" ])
      empty.status `shouldEqual` 401
  it "GET /api is an API request, a 404, not the page" do
    withServer \port -> do
      a <- get port "/api" []
      a.status `shouldEqual` 404
      a.text `shouldSatisfy` (not <<< contains (Pattern "the index"))
  where
  -- one more byte than the limit, as hex
  oversized = fromCharArray (replicate (2 * (1024 * 1024 + 1)) 'a')
