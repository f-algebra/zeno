-- | Constructing and using computations which can fail.
-- Like 'MonadPlus' without 'mplus', and with more readably named functions.
-- Requires qualified import (usually as "Fail").
module Control.Failure (
  MonadFailure (..),
  when, unless, toMaybe, success, catchWith
) where

import Prelude hiding ( catch )
import Data.Maybe ( fromJust, fromMaybe, isNothing )
import Control.Monad ( Monad, liftM, mzero )
import Control.Monad.Trans.Maybe ( MaybeT (..) )
import qualified Control.Monad as Monad

class Monad m => MonadFailure m where
  -- | The computation fails if this point is reached, like 'mzero'.
  here :: m a

-- | The computation fails if the argument is 'True'.
when :: MonadFailure m => Bool -> m ()
when = flip Monad.when here

-- | The computation fails if the argument is 'False'.
unless :: MonadFailure m => Bool -> m ()
unless = flip Monad.unless here

-- * The following functions remove the instance of 'MonadFailure'
-- from some computation, by handling the failure in some way.
-- They work by instantiating this 'MonadFailure' as a 'MaybeT' and
-- handling the maybe.

-- | Converts a 'MonadFailure' instance into a 'Maybe'
toMaybe :: Monad m => MaybeT m a -> m (Maybe a)
toMaybe = runMaybeT

-- | Asserts that a computation has succeeded, will throw a pattern match
-- exception if this is not the case.
success :: Monad m => MaybeT m a -> m a
success mby_t = do
  mby <- runMaybeT mby_t
  Monad.when (isNothing mby)
    $ error "Failure.success was given a failed computation"
  return (fromJust mby)

-- | Strips 'MonadFailure' by providing a default value to return
-- if the computation has failed.
catchWith :: Monad m => a -> MaybeT m a -> m a
catchWith def = liftM (fromMaybe def) . toMaybe
  

-- * Instance declarations

instance Monad m => MonadFailure (MaybeT m) where
  here = mzero

