module Zeno.Unique (
  Unique, MonadUnique (..), next
) where

import Prelude ()
import Zeno.Prelude

newtype Unique = Unique { run :: Int }
  deriving ( Eq, Ord )

next :: Unique -> Unique
next = Unique . (+ 1) . run

class Monad m => MonadUnique m where
  new :: m Unique
  
instance Show Unique where
  show = intToChars . run
    where
    intToChars :: Int -> String
    intToChars 0 = []                          
    intToChars n = 
      let c = chr $ (n `mod` 26) + (ord 'a')
      in c : intToChars (n `div` 26)
      
instance Monoid Unique where
  mempty = Unique 0
  mappend (Unique i) (Unique j) = Unique (max i j)
  
instance MonadUnique m => MonadUnique (ReaderT r m) where
  new = lift new
  
instance (MonadUnique m, Monoid w) => MonadUnique (WriterT w m) where
  new = lift new
  
instance (MonadUnique m, Monoid w) => MonadUnique (RWST r w s m) where
  new = lift new
  
instance (Error e, MonadUnique m) => MonadUnique (ErrorT e m) where
  new = lift new
  
instance MonadUnique m => MonadUnique (MaybeT m) where
  new = lift new
