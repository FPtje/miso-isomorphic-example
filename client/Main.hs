module Main where

import qualified Common
import Data.Proxy
import Control.Lens hiding ( view )

import Miso
import Miso.String

-- Miso has to know what the URI of the application is for use in apps with
-- multiple pages.
instance HasURI Common.Model where
  lensURI = Common.uri

main :: IO ()
main =
  miso App
    { initialAction = Common.NoOp
    , model         = Common.initialModel
    , update        = updateModel
    , view          = Common.homeView
    , events        = defaultEvents
    , subs          = []
    , mountPoint    = Nothing
    }


updateModel
    :: Common.Action
    -> Common.Model
    -> Effect Common.Action Common.Model
updateModel action m =
    case action of
      Common.NoOp -> noEff m
      Common.AddOne ->
        m & Common.counterValue +~ 1
          & noEff
      Common.SubtractOne ->
        m & Common.counterValue -~ 1
          & noEff
