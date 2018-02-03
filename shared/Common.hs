{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE CPP                        #-}

module Common where

import           Control.Lens
import           Data.Proxy
import           Servant.API
#if MIN_VERSION_servant(0,10,0)
import           Servant.Utils.Links
#endif

import           Miso
import           Miso.String


data Model
   = Model
     { _uri          :: !URI
     , _counterValue :: !Int
     }
     deriving (Eq, Show)

initialModel :: URI -> Model
initialModel uri =
    Model
    { _uri = uri
    , _counterValue = 0
    }

data Action
  = NoOp
  | AddOne
  | SubtractOne
  | ChangeURI !URI
  | HandleURIChange !URI
  deriving (Show, Eq)

-- Holds a servant route tree of `View action`
type ClientRoutes = Home :<|> Flipped

-- Home route, contains two buttons and a field
type Home = View Action

-- Flipped route, same as Home, but with the buttons flipped
type Flipped = "flipped" :> View Action

-- URI that points to the home route
homeLink :: URI
homeLink =
#if MIN_VERSION_servant(0,10,0)
    linkURI $ safeLink (Proxy @ClientRoutes) (Proxy @Home)
#else
    safeLink (Proxy @ClientRoutes) (Proxy @Home)
#endif

-- URI that points to the flipped route
flippedLink :: URI
flippedLink =
#if MIN_VERSION_servant(0,10,0)
    linkURI $ safeLink (Proxy @ClientRoutes) (Proxy @Flipped)
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
        , text $ ms $ show $ _counterValue m
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
        , text $ ms $ show $ _counterValue m
        , button_ [ onClick SubtractOne ] [ text "-" ]
        ]
      , button_ [ onClick $ ChangeURI homeLink ] [ text "Go to /" ]
      ]

page404View :: View Action
page404View =
    text "Yo, 404, page unknown. Go to / or /flipped. Shoo!"
