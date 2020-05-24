module Page.About (component) where

import Prelude
import CSS as CSS
import Capability.Navigate (navigate, class Navigate)
import Data.Array (sortBy)
import Data.Const (Const)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Monoid (guard)
import Data.Monoid (mempty)
import Data.Route (Route(..))
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (classes)
import Halogen.HTML.Properties as HP
import Halogen.Hooks (HookM)
import Halogen.Hooks as Hooks
import Network.RemoteData as RD
import Partial.Unsafe (unsafePartial)
import Slug as Slug

type ConnectedChildSlots
  = ( innerComponent :: H.Slot (Const Void) Void )

_innerComponent :: SProxy "innerComponent"
_innerComponent = SProxy

component :: forall m. Navigate m => H.Component HH.HTML (Const Void) Unit Void m
component =
  Hooks.component \_ _ ->
    Hooks.pure $ HH.div [] [ HH.slot _innerComponent unit innerComponent unit absurd ]

innerComponent :: forall m. Navigate m => H.Component HH.HTML (Const Void) Unit Void m
innerComponent =
  Hooks.component \_ input -> Hooks.do
    Hooks.pure
      $ HH.div
          [ HE.onClick \_ -> Just $ H.lift $ navigate $ Home
          ]
          [ HH.text "About page" ]
