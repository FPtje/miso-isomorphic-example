{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

module Main where

import qualified Common
import           Data.Proxy
import qualified Lucid                                as L
import qualified Lucid.Base                           as L
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Gzip
import           Network.Wai.Middleware.RequestLogger
import qualified Servant
import           Servant ( (:>), (:<|>)(..) )
import qualified System.IO                            as IO

import           Miso

main :: IO ()
main = do
  IO.hPutStrLn IO.stderr "Running on port 3003..."

  run 3003 $ logStdout (compress app)
    where
      compress :: Middleware
      compress = gzip def { gzipFiles = GzipCompress }

app :: Application
app =
    Servant.serve (Proxy @ServerAPI) (static :<|> serverHandlers)
  where
    static :: Servant.Server StaticAPI
    static = Servant.serveDirectory "static"

    serverHandlers :: Servant.Server ServerRoutes
    serverHandlers = pure $ HtmlPage $ Common.homeView Common.initialModel


-- | Represents the top level Html code. Its value represents the body of the
-- page.
newtype HtmlPage a = HtmlPage a
  deriving (Show, Eq)

instance L.ToHtml a => L.ToHtml (HtmlPage a) where
  toHtmlRaw = L.toHtml
  toHtml (HtmlPage x) =
      L.doctypehtml_ $ do
        L.head_ $ do
          L.title_ "Miso isomorphic example"
          L.meta_ [L.charset_ "utf-8"]

          L.with (L.script_ mempty)
            [ L.makeAttribute "src" "static/all.js"
            , L.makeAttribute "async" mempty
            , L.makeAttribute "defer" mempty
            ]

        L.body_ (L.toHtml x)

-- Converts the ClientRoutes (which are a servant tree of routes leading to
-- some `View action`) to lead to `Get '[Html] (HtmlPage (View Common.Action))`
type ServerRoutes
   = ToServerRoutes Common.ClientRoutes HtmlPage Common.Action

-- The server serves static files besides the ServerRoutes, among which is the
-- javascript file of the client.
type ServerAPI =
       StaticAPI
  :<|> ServerRoutes

type StaticAPI = "static" :> Servant.Raw
