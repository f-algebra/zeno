-- | The generation and use of unique identifiers.
-- Requires qualified import, usually as "Unique".
module Control.Unique (
  Unique, MonadUnique (..), GenT, Gen,
  new, readonly, abstractGen, 
  primaryStream,
  primaryGenT, localGenT,
  global, unsafeIOGlobal,
) where

import Data.IORef ( IORef, newIORef, atomicModifyIORef )
import Data.Monoid ( Monoid (..) )
import Data.Char ( chr, ord )
import Data.Empty
import Control.Applicative ( Applicative )
import Control.Monad ( Monad, liftM )
import Control.Monad.Identity ( Identity, runIdentity )
import Control.Monad.Trans ( lift, MonadTrans )
import Control.Monad.Trans.Identity ( IdentityT )
import Control.Monad.Trans.Maybe ( MaybeT )
import Control.Monad.State ( State, get, put, runState,
  StateT, runStateT )
import Control.Monad.Writer ( WriterT )
import Control.Monad.Reader ( ReaderT )
import System.IO.Unsafe ( unsafePerformIO )

import Control.Failure ( MonadFailure )
import qualified Control.Failure as Fail

data StreamType = Primary | Local | Global
  deriving ( Eq, Ord )

data Unique 
  = Unique  { uniqueType :: !StreamType
            , uniqueId :: !Int }
  deriving ( Eq, Ord )

primaryStream :: [Unique]
primaryStream = map (Unique Primary) [1..]

globalStream :: IORef [Unique]
globalStream = unsafePerformIO
  $ newIORef 
  $ map (Unique Global) [1..]

localStream :: [Unique]
localStream = map (Unique Local) [1..]

instance Empty Unique where
  empty = Unique Local 0

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
-- the computation, since these values will still be unused.
-- The type 'a' should not contain 'Unique'.
-- 'State' becomes 'Reader'.
readonly :: MonadUnique m => m a -> m a
readonly comp = do
  st <- getStream
  x <- comp
  putStream st
  return x
  
instance Show Unique where
  show = intToChars . uniqueId
    where
    intToChars :: Int -> String
    intToChars 0 = []                          
    intToChars n = 
      let c = chr $ (n `mod` 26) + (ord 'a')
      in c : intToChars (n `div` 26)

instance MonadUnique m => MonadUnique (MaybeT m) where
  getStream = lift getStream
  putStream = lift . putStream

instance MonadUnique m => MonadUnique (ReaderT r m) where
  getStream = lift getStream
  putStream = lift . putStream
  
instance (Monoid w, MonadUnique m) => MonadUnique (WriterT w m) where
  getStream = lift getStream
  putStream = lift . putStream
  
instance MonadUnique m => MonadUnique (IdentityT m) where
  getStream = lift getStream
  putStream = lift . putStream
  
instance Monad m => MonadUnique (GenT m) where
  getStream = GenT get
  putStream = GenT . put
  
instance MonadFailure m => MonadFailure (GenT m) where
  here = lift Fail.here
  
instance MonadTrans GenT where
  lift = GenT . lift
  
 
-- | A simple 'Monad' which implements 'MonadUnique', 
-- just a wrapper around a 'State' monad.
newtype GenT m a = GenT (StateT [Unique] m a)
  deriving ( Functor, Applicative, Monad )
  
type Gen = GenT Identity

runGenT :: Monad m => GenT m a -> [Unique] -> m (a, [Unique])
runGenT (GenT st) = runStateT st

runGen :: Gen a -> [Unique] -> (a, [Unique])
runGen gen = runIdentity . runGenT gen

evalGenT :: Monad m => GenT m a -> [Unique] -> m a
evalGenT gen = liftM fst . runGenT gen

-- | A function I made to hopefully improve speed. It allows you to
-- write a function that requires unique values using the concrete
-- 'Gen' monad rather than some abstract 'MonadUnique'. You then
-- use 'abstractGen' to convert your function to work over any
-- 'MonadUnique'.
abstractGen :: MonadUnique m => Gen a -> m a
abstractGen gen = do
  st <- getStream
  let (x, st') = runGen gen st
  putStream st'
  return x

global :: IO Unique
global = atomicModifyIORef globalStream
  $ \unis -> (tail unis, head unis) 
  
unsafeIOGlobal :: Unique
unsafeIOGlobal = unsafePerformIO global
  
-- | The "local" uniques are a separate stream to the main one,
-- allowing you to provide unique values within an isolated part of a
-- program. Just make sure any return values do not contain these 
-- generated uniques, they have to be isolated.
localGenT :: Monad m => GenT m a -> m a
localGenT = flip evalGenT localStream

primaryGenT :: Monad m => GenT m a -> m a
primaryGenT = flip evalGenT primaryStream
