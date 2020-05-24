module Main where

{-- import Prelude --}
import Component.Router as Router
import Component.Router (Query)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Routing.Hash (matchesWith)
import Routing.Duplex (parse)
import Data.Route (routeCodec)
import Data.Maybe (Maybe(..))
import Capability.AppM (runAppM)
import Prelude
import Control.Alt (class Functor)
import Control.Alternative (class Applicative, class Apply, apply)
import Control.Bind (class Bind)
import Control.Monad.Reader (ReaderT(..))
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (log)
import Halogen (HalogenM)
import Halogen as H
import Halogen.Component (ComponentSlot)
import Halogen.Component (mkComponent)
import Halogen.HTML as HH
import Halogen.Query.HalogenQ as HQ
import Halogen.Query.EventSource (EventSource(..), Emitter(..), affEventSource, emit)
import Halogen.Query.EventSource as ES
import Effect.Aff (error, forkAff, killFiber)
import Control.Monad.Rec.Class (forever)
{-- import FRP (FRP) --}
import FRP.Event (Event, create, subscribe)
import Effect.Aff (Aff)
import Effect.Ref (Ref)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class (liftEffect)
import Effect.Ref (Ref, new, read, write)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Coroutine as CC
import Data.Either (Either(..))
import Data.Bifunctor (class Bifunctor)
import Effect.Aff.AVar as AV
import Halogen.Aff.Util (runHalogenAff, awaitBody)
import Halogen.VDom.Driver (runUI)
import Data.Symbol (SProxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Halogen.HTML.Events as HE
import Effect.Aff.Bus as Bus
import Effect.Aff.Bus (BusRW)
import Debug.Trace (trace)

main :: Effect Unit
main =
  HA.runHalogenAff do
    let
      rootComponent :: H.Component HH.HTML Router.Query {} Void Aff
      rootComponent = H.hoist (runAppM {}) Router.component
    body <- HA.awaitBody
    halogenIO <-
      runUI rootComponent {} body
    void $ liftEffect
      $ matchesWith (parse routeCodec) \old new ->
          when (old /= Just new) do
            liftEffect $ log "changed!"
            launchAff_ $ halogenIO.query $ H.tell $ Router.Navigate new
    pure unit
