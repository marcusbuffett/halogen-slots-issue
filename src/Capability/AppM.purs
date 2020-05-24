module Capability.AppM where

import Prelude
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Data.Bifunctor (class Bifunctor)
import Effect.Aff.Bus as Bus
import Effect.Ref (Ref, new, read, write)
import Undefined (undefined)
import Conduit.Env (Env)

newtype AppM a
  = AppM (ReaderT Env Aff a)

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM :: Functor (AppM)

derive newtype instance applyAppM :: Apply (AppM)

derive newtype instance applicativeAppM :: Applicative (AppM)

derive newtype instance bindAppM :: Bind (AppM)

derive newtype instance monadAppM :: Monad (AppM)

derive newtype instance monadEffectAppM :: MonadEffect (AppM)

derive newtype instance monadAffAppM :: MonadAff (AppM)
