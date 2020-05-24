module Component.Router where

import Prelude
import Capability.Navigate (navigate, class Navigate)
import Control.Monad.Reader (class MonadAsk)
import Data.Const (Const)
import Data.Either (hush)
import Data.Foldable (elem)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Route (Route(..), routeCodec)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen (ClassName(..), liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties (class_)
import Page.About as About
import Page.Home as Home
import Routing.Duplex as RD
import Routing.Hash (getHash)
import Slug as Slug

type State
  = { route :: Maybe Route
    }

data Query a
  = Navigate Route a

data Action
  = Initialize
  | Receive {}

type ChildSlots
  = ( home :: OpaqueSlot Int
    , about :: OpaqueSlot Int
    , request :: OpaqueSlot Int
    , submitRequest :: OpaqueSlot Int
    )

type OpaqueSlot
  = H.Slot (Const Void) Void

component ::
  forall m.
  MonadAff m =>
  Navigate m =>
  H.Component HH.HTML Query {} Void m
component =
  H.mkComponent
    { initialState: const { route: Nothing }
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleQuery = handleQuery
              , handleAction = handleAction
              , receive = Just <<< Receive
              , initialize = Just Initialize
              }
    }
  where
  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Initialize -> do
      initialRoute <- hush <<< (RD.parse routeCodec) <$> liftEffect getHash
      navigate $ fromMaybe Home initialRoute
      pure unit
    Receive {} -> do
      pure unit

  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
  handleQuery = case _ of
    Navigate dest a -> do
      { route } <- H.get
      {-- liftEffect $ log $ "test, do we get in here?" <> show dest --}
      when (route /= Just dest) do
        {-- liftEffect $ log "How about here?" --}
        H.modify_ _ { route = Just dest }
      pure (Just a)

  render :: State -> H.ComponentHTML Action ChildSlots m
  render { route } = do
    HH.div [ class_ (ClassName "blah") ]
      [ case route of
          Just r -> case r of
            Home -> HH.slot (SProxy :: _ "home") 0 Home.component unit absurd
            About -> HH.slot (SProxy :: _ "about") 1 About.component unit absurd
          Nothing -> HH.div_ [ HH.text "Oh no! That page wasn't found." ]
      ]
