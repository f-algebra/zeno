-- |A combination of the Reader and Writer monad.
-- Use with a commutative monoid to give a parallelizable computation. 
module Zeno.ReaderWriter (
  ReaderWriter, runReaderWriter
) where

import Prelude ()
import Zeno.Prelude

newtype ReaderWriter r w a 
  = RW { runRW :: r -> (a, w) }

runReaderWriter :: ReaderWriter r w a -> r -> (a, w)
runReaderWriter = runRW
  

instance Functor (ReaderWriter r w) where
  fmap f rw = RW newRW
    where
    newRW r = (f a, w)
      where (a, w) = runRW rw r

      
instance Monoid w => Monad (ReaderWriter r w) where
  return a = RW newRW
    where newRW _ = (a, mempty)
  
  m >>= f = RW newRW
    where
    newRW r = (a', w `mappend` w')
      where
      (a, w) = runRW m r
      (a', w') = runRW (f a) r
      

instance Monoid w => MonadWriter w (ReaderWriter r w) where
  tell w = RW newRW
    where newRW _ = ((), w)
    
  listen m = RW newRW 
    where 
    newRW r = ((a, w), w)
      where (a, w) = runRW m r
    
  pass m = RW newRW
    where 
    newRW r = (a, f w)
      where ((a, f), w) = runRW m r
      
      
instance Monoid w => MonadReader r (ReaderWriter r w) where
  ask = RW newRW
    where newRW r = (r, mempty)
    
  local f m = RW newRW
    where newRW r = runRW m (f r) 
  
