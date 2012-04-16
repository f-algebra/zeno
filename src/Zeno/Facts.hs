module Zeno.Facts (
  Reader (..), State (..), 
  FactsT, runT, with, none, asks
) where

import Prelude ()
import Zeno.Prelude hiding ( ask, Reader, State )
import Zeno.Var ( ZVar, ZTerm, ZEquation )
import Zeno.Unique ( MonadUnique (..) )

import qualified Control.Monad.Reader as Reader

class Monad m => Reader m where
  ask :: m [ZEquation]
  add :: ZEquation -> m a -> m a  
  
class Reader m => State m where
  set :: [ZEquation] -> m ()

newtype FactsT m a = FactsT { runT :: [ZEquation] -> m (a, [ZEquation]) }

with :: Monad m => [ZEquation] -> FactsT m a -> m a
with eqs = liftM fst . flip runT eqs

none :: Monad m => FactsT m a -> m a
none = with []

instance Monad m => Monad (FactsT m) where
  return x = FactsT $ \eqs -> return (x, eqs)
  FactsT f >>= g = FactsT $ \eqs -> do
    (x, eqs') <- f eqs
    runT (g x) eqs' 

instance MonadTrans FactsT where
  lift m = FactsT $ \eqs -> do
    x <- m
    return (x, eqs)

instance Monad m => Reader (FactsT m) where
  ask = FactsT $ \eqs -> return (eqs, eqs)
  add eq (FactsT f) = FactsT $ \eqs -> f (eq:eqs)
  
instance MonadUnique m => MonadUnique (FactsT m) where
  getStream = lift getStream
  putStream = lift . putStream
  
instance Reader Identity where
  ask = return []
  add _ = id

instance Reader m => Reader (MaybeT m) where
  ask = lift ask
  add eq = MaybeT . add eq . runMaybeT 


