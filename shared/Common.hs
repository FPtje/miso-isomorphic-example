{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE CPP                        #-}

module Common where

import           Control.Lens
import           Data.Proxy
import           Servant.API

import           Miso
import           Miso.String


data Model
   = Model
     { _uri          :: !URI
     , _counterValue :: !Int
     }
     deriving (Eq, Show)


initialModel :: Model
initialModel =
    Model
    { _uri = homeLink
    , _counterValue = 0
    }


data Action
  = NoOp
  | AddOne
  | SubtractOne
  deriving (Show, Eq)

-- Holds a servant route tree of `View action`, currently consisting of just
-- one route.
type ClientRoutes = Home

-- Currently the only route
type Home = View Action

-- URI that points to the home route
homeLink :: URI
homeLink =
#if MIN_VERSION_servant(0,10,0)
    linkURI $ safeLink (Proxy @ClientRoutes) (Proxy @Home)
#else
    safeLink (Proxy @ClientRoutes) (Proxy @Home)
#endif

makeLenses ''Model

-- View function of the Home route
homeView :: Model -> View Action
homeView m =
  div_
    []
    [ button_ [ onClick SubtractOne ] [ text "-" ]
    , text $ ms $ show $ _counterValue m
    , button_ [ onClick AddOne ] [ text "+" ]
    ]
