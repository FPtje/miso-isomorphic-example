{-# LANGUAGE CPP               #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import qualified Common
import Data.Proxy ( Proxy(..) )
import Control.Lens ( (^.), (%=), (+=), (-=), (.=), makeLenses )
import qualified Servant.API as Servant
import Servant.API ( (:<|>)(..) )
#if MIN_VERSION_servant(0,10,0)
import qualified Servant.Utils.Links as Servant
#endif
import qualified Miso
import Miso ( View, App(..) )
import qualified Miso.String as Miso
import qualified JavaScript.Object.Internal as JS

main :: IO ()
main = do
  currentURI <- Miso.getCurrentURI

  homePlus     <- liftIO Miso.createLifeCycleKey
  homeMinus    <- liftIO Miso.createLifeCycleKey
  flippedPlus  <- liftIO Miso.createLifeCycleKey
  flippedMinus <- liftIO Miso.createLifeCycleKey

  Miso.miso App
    { initialAction = Common.NoOp
    , model         =
        Common.initialModel
          currentURI
          homePlus
          homeMinus
          flippedPlus
          flippedMinus
    , update        = Miso.fromTransition . updateModel
    , view          = Common.viewModel
    , events        = Miso.defaultEvents
    , subs          = [ Miso.uriSub Common.HandleURIChange ]
    , mountPoint    = Nothing
    }

updateModel
    :: Common.Action
    -> Miso.Transition Common.Action Common.Model ()
updateModel action =
    case action of
      Common.NoOp          -> pure ()
      Common.AddOne        -> Common.counterValue += 1
      Common.SubtractOne   -> Common.counterValue -= 1
      Common.ChangeURI uri ->
        Miso.scheduleIO $ do
          Miso.pushURI uri
          pure Common.NoOp
      Common.HandleURIChange uri -> Common.uri .= uri
      Common.OnCreated sink (Miso.DOMNode obj) -> Miso.scheduleIO $ do
        putStrLn "OnCreated!!!"
        Miso.consoleLog obj
        pure Common.NoOp
      Common.OnDestroyed (Miso.DOMNode obj) -> Miso.scheduleIO $ do
        putStrLn "OnDestroyed!"
        Miso.consoleLog obj
        pure Common.NoOp
      Common.HideAll ->
        Common.hideAll %= not
