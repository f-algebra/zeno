module Zeno.Unique (
  Unique, MonadUnique (..), stream, new
) where

import Prelude ()
import Zeno.Prelude

newtype Unique = Unique { run :: Int }
  deriving ( Eq, Ord )
  
stream :: [Unique]
stream = map Unique [1..]

class Monad m => MonadUnique m where
  getStream :: m [Unique]
  putStream :: [Unique] -> m ()
  
new :: MonadUnique m => m Unique
new = do
  (fst:rest) <- getStream
  putStream rest
  return fst
  
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
  getStream = lift getStream
  putStream = lift . putStream
  
instance (MonadUnique m, Monoid w) => MonadUnique (WriterT w m) where
  getStream = lift getStream
  putStream = lift . putStream
  
instance (MonadUnique m, Monoid w) => MonadUnique (RWST r w s m) where
  getStream = lift getStream
  putStream = lift . putStream
  
instance (Error e, MonadUnique m) => MonadUnique (ErrorT e m) where
  getStream = lift getStream
  putStream = lift . putStream
  
instance MonadUnique m => MonadUnique (MaybeT m) where
  getStream = lift getStream
  putStream = lift . putStream
