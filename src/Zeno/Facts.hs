module Zeno.Facts (
  Reader (..), FactsT,
  runT, with, none, asks, add
) where

import Prelude ()
import Zeno.Prelude hiding ( ask, local, Reader )
import Zeno.Var ( ZVar, ZTerm, ZEquation )
import Zeno.Unique ( MonadUnique (..) )

import qualified Control.Monad.Reader as Reader

type Facts = [ZEquation]

class Monad m => Reader m where
  ask :: m Facts
  local :: (Facts -> Facts) -> m a -> m a

newtype FactsT m a = FactsT { runT :: Facts -> m a }

add :: Reader m => ZEquation -> m a -> m a
add fact = local (fact:)

with :: Monad m => Facts -> FactsT m a -> m a
with = flip runT

none :: Monad m => FactsT m a -> m a
none = with []

instance Monad m => Monad (FactsT m) where
  return = FactsT . const . return
  FactsT f >>= g = FactsT $ \eqs -> do
    x <- f eqs
    runT (g x) eqs

instance MonadTrans FactsT where
  lift = FactsT . const

instance Monad m => Reader (FactsT m) where
  ask = FactsT return
  local f (FactsT g) = FactsT $ \eqs -> g (f eqs)
  
instance MonadUnique m => MonadUnique (FactsT m) where
  getStream = lift getStream
  putStream = lift . putStream
  
instance Reader Identity where
  ask = return []
  local = const id

instance Reader m => Reader (MaybeT m) where
  ask = lift ask
  local f = MaybeT . local f . runMaybeT

instance Reader m => Reader (IdentityT m) where
  ask = lift ask
  local f = IdentityT . local f . runIdentityT


