{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import qualified Common
import Data.Proxy
import Control.Lens hiding ( view )
import Servant.API
#if MIN_VERSION_servant(0,10,0)
import Servant.Utils.Links
#endif

import Miso
import Miso.String

-- Miso has to know what the URI of the application is for use in apps with
-- multiple pages.
instance HasURI Common.Model where
  lensURI = Common.uri

main :: IO ()
main = do
  currentURI <- getCurrentURI

  miso App
    { initialAction = Common.NoOp
    , model         = Common.initialModel currentURI
    , update        = fromTransition . updateModel
    , view          = viewModel
    , events        = defaultEvents
    , subs          = [ uriSub Common.HandleURIChange ]
    , mountPoint    = Nothing
    }

updateModel
    :: Common.Action
    -> Transition Common.Action Common.Model ()
updateModel action =
    case action of
      Common.NoOp          -> pure ()
      Common.AddOne        -> Common.counterValue += 1
      Common.SubtractOne   -> Common.counterValue -= 1
      Common.ChangeURI uri ->
        scheduleIO $ do
          pushURI uri
          pure Common.NoOp
      Common.HandleURIChange uri -> Common.uri .= uri

-- Checks which URI is open and shows the appropriate view
viewModel :: Common.Model -> View Common.Action
viewModel m =
    case runRoute (Proxy @Common.ClientRoutes) viewTree m of
      Left _routingError -> Common.page404View
      Right v -> v

-- Servant tree of view functions
-- Should follow the structure of Common.ClientRoutes
viewTree
    ::      (Common.Model -> View Common.Action)
       :<|> (Common.Model -> View Common.Action)
viewTree = Common.homeView :<|> Common.flippedView
