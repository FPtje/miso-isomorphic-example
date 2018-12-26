{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

module Main where

import qualified Common
import           Data.Proxy
import qualified Lucid                                as L
import qualified Lucid.Base                           as L
import qualified Network.HTTP.Types                   as HTTP
import qualified Network.Wai                          as Wai
import qualified Network.Wai.Handler.Warp             as Wai
import qualified Network.Wai.Middleware.Gzip          as Wai
import qualified Network.Wai.Middleware.RequestLogger as Wai
import qualified Servant
import           Servant ( (:>), (:<|>)(..) )
import qualified System.IO                            as IO

import qualified Miso
import Miso ( View )

main :: IO ()
main = do
    IO.hPutStrLn IO.stderr "Running on port 3003..."

    Wai.run 3003 $ Wai.logStdout $ compress app
  where
    compress :: Wai.Middleware
    compress = Wai.gzip Wai.def { Wai.gzipFiles = Wai.GzipCompress }

app :: Wai.Application
app =
    Servant.serve (Proxy @ServerAPI)
        (    static
        :<|> serverHandlers
        :<|> Servant.Tagged page404
        )
  where
    static :: Servant.Server StaticAPI
    static = Servant.serveDirectoryFileServer "static"

    serverHandlers :: Servant.Server ServerRoutes
    serverHandlers = homeServer :<|> flippedServer

    -- Alternative type:
    -- Servant.Server (ToServerRoutes Common.Home HtmlPage Common.Action)
    -- Handles the route for the home page, rendering Common.homeView.
    homeServer :: Servant.Handler (HtmlPage (View Common.Action))
    homeServer =
        pure $ HtmlPage $
          Common.viewModel $
          Common.initialModel Common.homeLink

    -- Alternative type:
    -- Servant.Server (ToServerRoutes Common.Flipped HtmlPage Common.Action)
    -- Renders the /flipped page.
    flippedServer :: Servant.Handler (HtmlPage (View Common.Action))
    flippedServer =
        pure $ HtmlPage $
          Common.viewModel $
          Common.initialModel Common.flippedLink

    -- The 404 page is a Wai application because the endpoint is Raw.
    -- It just renders the page404View and sends it to the client.
    page404 :: Wai.Application
    page404 _ respond = respond $ Wai.responseLBS
        HTTP.status404 [("Content-Type", "text/html")] $
        L.renderBS $ L.toHtml Common.page404View

-- | Represents the top level Html code. Its value represents the body of the
-- page.
newtype HtmlPage a = HtmlPage a
  deriving (Show, Eq)

instance L.ToHtml a => L.ToHtml (HtmlPage a) where
    toHtmlRaw = L.toHtml
    toHtml (HtmlPage x) = do
        L.doctype_
        L.head_ $ do
          L.title_ "Miso isomorphic example"
          L.meta_ [L.charset_ "utf-8"]

          L.with (L.script_ mempty)
            [ L.makeAttribute "src" "/static/all.js"
            , L.makeAttribute "async" mempty
            , L.makeAttribute "defer" mempty
            ]

        L.body_ (L.toHtml x)

-- Converts the ClientRoutes (which are a servant tree of routes leading to
-- some `View action`) to lead to `Get '[Html] (HtmlPage (View Common.Action))`
type ServerRoutes
   = Miso.ToServerRoutes Common.ViewRoutes HtmlPage Common.Action

-- The server serves static files besides the ServerRoutes, among which is the
-- javascript file of the client.
type ServerAPI =
       StaticAPI
  :<|> (ServerRoutes
  :<|> Servant.Raw) -- This will show the 404 page for any unknown route

type StaticAPI = "static" :> Servant.Raw

