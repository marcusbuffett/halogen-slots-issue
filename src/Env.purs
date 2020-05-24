module Conduit.Env where

import Prelude
import Data.Maybe (Maybe)
import Effect.Aff.Bus (BusRW)
import Effect.Ref (Ref)

-- | Let's start with some types necessary for the rest of the module.
-- |
-- | Our app monad will provide a globally-accessible, read-only environment with a few fields.
-- | First, we'll use a `LogLevel` flag to indicate whether we'd like to log everything (`Dev`) or
-- | only critical messages (`Prod`). Next, we'll maintain a configurable base URL. Our `UserEnv`
-- | will represent some mutable state holding the currently-logged-in user (if there is one).
-- |
-- | In the `AppM` module I demonstrate how to make this information available (without passing it
-- | as an argument) to any function running in `AppM`.
type Env
  = {}
