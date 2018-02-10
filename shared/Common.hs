{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE CPP                        #-}

module Common where

import Control.Lens
import Data.Proxy ( Proxy(..) )
import qualified Servant.API as Servant
import Servant.API ( (:<|>), (:>) )
#if MIN_VERSION_servant(0,10,0)
import qualified Servant.Utils.Links as Servant
#endif
import Miso ( View )
import Miso.Html
import qualified Miso.String as Miso
import qualified Network.URI as Network


data Model
   = Model
     { _uri          :: !Network.URI
     , _counterValue :: !Int
     }
     deriving (Eq, Show)

initialModel :: Network.URI -> Model
initialModel uri =
    Model
    { _uri = uri
    , _counterValue = 0
    }

data Action
  = NoOp
  | AddOne
  | SubtractOne
  | ChangeURI !Network.URI
  | HandleURIChange !Network.URI
  deriving (Show, Eq)

-- Holds a servant route tree of `View action`
type ClientRoutes = Home :<|> Flipped

-- Home route, contains two buttons and a field
type Home = View Action

-- Flipped route, same as Home, but with the buttons flipped
type Flipped = "flipped" :> View Action

-- Network.URI that points to the home route
homeLink :: Network.URI
homeLink =
#if MIN_VERSION_servant(0,10,0)
    Servant.linkURI $ Servant.safeLink (Proxy @ClientRoutes) (Proxy @Home)
#else
    safeLink (Proxy @ClientRoutes) (Proxy @Home)
#endif

-- Network.URI that points to the flipped route
flippedLink :: Network.URI
flippedLink =
#if MIN_VERSION_servant(0,10,0)
    Servant.linkURI $ Servant.safeLink (Proxy @ClientRoutes) (Proxy @Flipped)
#else
    safeLink (Proxy @ClientRoutes) (Proxy @Flipped)
#endif

makeLenses ''Model

-- View function of the Home route
homeView :: Model -> View Action
homeView m =
    div_ []
      [ div_
        []
        [ button_ [ onClick SubtractOne ] [ text "-" ]
        , text $ Miso.ms $ show $ _counterValue m
        , button_ [ onClick AddOne ] [ text "+" ]
        ]
      , button_ [ onClick $ ChangeURI flippedLink ] [ text "Go to /flipped" ]
      ]

-- View function of the Home route
flippedView :: Model -> View Action
flippedView m =
    div_ []
      [ div_
        []
        [ button_ [ onClick AddOne ] [ text "+" ]
        , text $ Miso.ms $ show $ _counterValue m
        , button_ [ onClick SubtractOne ] [ text "-" ]
        ]
      , button_ [ onClick $ ChangeURI homeLink ] [ text "Go to /" ]
      ]

page404View :: View Action
page404View =
    text "Yo, 404, page unknown. Go to / or /flipped. Shoo!"
