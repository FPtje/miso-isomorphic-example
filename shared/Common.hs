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
import Servant.API ( (:<|>)(..), (:>) )
#if MIN_VERSION_servant(0,10,0)
import qualified Servant.Utils.Links as Servant
#endif
import qualified Miso
import Miso ( View )
import Miso.Html
import qualified Miso.String as Miso
import qualified Network.URI as Network

data Model
   = Model
     { _uri          :: !Network.URI
     , _counterValue :: !Int
     , _hideAll      :: !Bool
     }
     deriving (Eq, Show)

initialModel :: Network.URI -> Model
initialModel uri =
    Model
    { _uri = uri
    , _counterValue = 0
    , _hideAll = False
    }

data Action
  = NoOp
  | AddOne
  | SubtractOne
  | ChangeURI !Network.URI
  | HandleURIChange !Network.URI
  | OnCreated (Action -> IO ()) !Miso.DOMNode
  | OnDestroyed !Miso.DOMNode
  | HideAll

-- Holds a servant route tree of `View action`
type ViewRoutes = Home :<|> Flipped

-- Home route, contains two buttons and a field
type Home = View Action

-- Flipped route, same as Home, but with the buttons flipped
type Flipped = "flipped" :> View Action

makeLenses ''Model

-- Checks which URI is open and shows the appropriate view
viewModel :: Common.Model -> View Common.Action
viewModel m =
    case Miso.runRoute (Proxy @Common.ViewRoutes) viewTree m of
      Left _routingError -> page404View
      Right v -> v

-- Servant tree of view functions
-- Should follow the structure of ViewRoutes
viewTree
    ::      (Model -> View Action)
       :<|> (Model -> View Action)
viewTree = homeView :<|> flippedView

-- View function of the Home route
homeView :: Model -> View Action
homeView m =
    div_ [] $
      buttonsNShit ++
      [ button_ [ onClick HideAll ] [ text "hide all" ]
      ]
  where
      buttonsNShit = if m ^. hideAll
        then []
        else
          [ div_
            []
            [ button_ [ onClick SubtractOne, Miso.lifeCycleEvents "home-" OnCreated OnDestroyed ] [ text "-" ]
            , text $ Miso.ms $ show $ _counterValue m
            , button_ [ onClick AddOne, Miso.lifeCycleEvents "home+" OnCreated OnDestroyed ] [ text "+" ]
            ]
          , button_ [ onClick $ ChangeURI flippedLink ] [ text "Go to /flipped" ]
          ]

-- View function of the Home route
flippedView :: Model -> View Action
flippedView m =
    div_ []
      [ div_
        []
        [ button_ [ onClick AddOne, Miso.lifeCycleEvents "flipped+" OnCreated OnDestroyed ] [ text "+" ]
        , text $ Miso.ms $ show $ _counterValue m
        , button_ [ onClick SubtractOne, Miso.lifeCycleEvents "flipped-" OnCreated OnDestroyed ] [ text "-" ]
        ]
      , button_ [ onClick $ ChangeURI homeLink ] [ text "Go to /" ]
      ]

page404View :: View Action
page404View =
    text "Yo, 404, page unknown. Go to / or /flipped. Shoo!"

-- Network.URI that points to the home route
homeLink :: Network.URI
homeLink =
#if MIN_VERSION_servant(0,10,0)
    Servant.linkURI $ Servant.safeLink (Proxy @ViewRoutes) (Proxy @Home)
#else
    safeLink (Proxy @ViewRoutes) (Proxy @Home)
#endif

-- Network.URI that points to the flipped route
flippedLink :: Network.URI
flippedLink =
#if MIN_VERSION_servant(0,10,0)
    Servant.linkURI $ Servant.safeLink (Proxy @ViewRoutes) (Proxy @Flipped)
#else
    safeLink (Proxy @ViewRoutes) (Proxy @Flipped)
#endif

-- Miso has to know what the URI of the application is for use in apps with
-- multiple pages.
instance Miso.HasURI Common.Model where
  lensURI = Common.uri
