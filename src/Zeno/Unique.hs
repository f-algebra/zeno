-- | The generation and use of unique identifiers.
-- Requires qualified import.
module Zeno.Unique (
  Unique, MonadUnique (..), 
  stream, new, global, readonly
) where

import Prelude ()
import Zeno.Prelude

newtype Unique = Unique { run :: Int }
  deriving ( Eq, Ord )
  
stream :: [Unique]
stream = map Unique [1..]

-- | A computation which requires unique identifiers
class Monad m => MonadUnique m where
  getStream :: m [Unique]
  putStream :: [Unique] -> m ()
  
new :: MonadUnique m => m Unique
new = do
  (fst:rest) <- getStream
  putStream rest
  return fst
 
-- | Asserts that a given computation that requires unique values does
-- not return something that contains these values.
-- Therefore, we can reset the unique stream to what it was before 
-- the computation, since these values will still be unique, 
-- as they have not been output.
-- 'State' becomes 'Reader'.
readonly :: MonadUnique m => m a -> m a
readonly comp = do
  st <- getStream
  x <- comp
  putStream st
  return x
  
globalStream :: IORef [Unique]
globalStream
  = unsafePerformIO 
  $ newIORef 
  $ map Unique [-1,-2..]
  
-- | Take a new globally unique identitfier
global :: IO Unique
global 
  = atomicModifyIORef globalStream
  $ \s -> (tail s, head s) 

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
  
instance MonadUnique m => MonadUnique (IdentityT m) where
  getStream = lift getStream
  putStream = lift . putStream
