module Zeno.Prelude
(
  module Prelude,
  module Control.Arrow,
  module Control.Applicative,
  module Control.Monad,
  module Control.Monad.State,
  module Control.Monad.Reader,
  module Control.Monad.Writer,
  module Control.Monad.Trans,
  module Control.Monad.List,
  module Control.Monad.RWS,
  module Control.Monad.Identity,
  module Control.Monad.Trans.Maybe,
  module Control.Monad.Fix,
  module Control.Exception,
  module Data.Maybe,
  module Data.Either,
  module Data.Monoid,
  module Data.Map,
  module Data.Set,
  module Data.Traversable,
  module Data.Foldable,
  module Data.List,
  module Data.String,
  module Data.IORef,
  module Data.IntMap,
  module Data.Char,
  module Data.IntSet,
  module Data.Function,
  module Data.Text,
  module Debug.Trace,
  module System.Random,
  
  (++), concat, intercalate, map, void,
  concatMap, concatMapM, partitionM,
  fromJustT, anyM, allM, findM, sortWith,
  minimalBy, nubOrd, elemOrd, intersectOrd
)
where

import Prelude hiding ( mapM, foldl, foldl1, mapM_, minimum, maximum, sequence_,
  foldr, foldr1, sequence, Maybe (..), maybe, all, any, elem, product,
  and, concat, notElem, or, concatMap, sum, (++), map )

import Control.Arrow ( (>>>), (<<<), (&&&), (***), first, second )
import Control.Applicative hiding ( empty )
import Control.Monad ( liftM, ap, replicateM, 
  zipWithM, filterM, when, unless, guard, (>=>), MonadPlus (..) )
import Control.Monad.Trans ( MonadTrans (..), lift, liftIO )
import Control.Monad.State ( evalStateT, execState, runState, evalState,
  MonadState (..), State (..), StateT (..), modify, gets )
import Control.Monad.Reader ( 
  MonadReader (..), Reader (..), ReaderT (..), asks, runReader )
import Control.Monad.Writer ( execWriter, runWriter, execWriterT,
  MonadWriter (..), Writer (..), WriterT (..) )
import Control.Monad.List ( ListT (..) )
import Control.Monad.Trans.Maybe
import Control.Monad.RWS ( RWS (..), RWST (..), execRWS, evalRWS )
import Control.Monad.Identity ( Identity (..) )
import Control.Monad.Fix
import Control.Exception ( assert )

import Data.Maybe
import Data.Either ( lefts, rights, partitionEithers )
import Data.Monoid
import Data.Map ( Map )
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.IntSet ( IntSet )
import Data.Traversable
import Data.Foldable hiding ( concat, concatMap )
import Data.List ( intersperse, unfoldr, partition
  , isPrefixOf, isSuffixOf, isInfixOf, sort, sortBy, findIndex
  , delete, elemIndices, intersect, union
  , (\\), subsequences, isSuffixOf, deleteBy )
import Data.IORef
import Data.Char ( isAlpha, isDigit, isAlphaNum, isSpace, chr, ord )
import Data.IntMap ( IntMap )
import Data.IntSet ( IntSet )
import Data.Function ( on )
import Data.Text ( Text )
import Data.String

import Debug.Trace
import System.Random

import qualified Data.Set as Set

infixr 6 ++

void :: Functor f => f a -> f ()
void = fmap (const ())

(++) :: Monoid m => m -> m -> m
(++) = mappend

map :: Functor f => (a -> b) -> f a -> f b
map = fmap

concat :: Monoid m => [m] -> m
concat = mconcat

concatMap :: Monoid m => (a -> m) -> [a] -> m
concatMap f = concat . map f

concatMapM :: (Monoid b, Monad m) => (a -> m b) -> [a] -> m b
concatMapM f = liftM concat . mapM f 

intercalate :: Monoid m => m -> [m] -> m
intercalate x = concat . intersperse x

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM f = foldrM f' ([], [])  
  where
  f' a (xs, ys) = do
    p <- f a
    return $
      if p then (a : xs, ys) else (xs, a : ys)
      
fromJustT :: Monad m => MaybeT m a -> m a
fromJustT = liftM fromJust . runMaybeT

anyM :: (Monad f, Traversable t) => (a -> f Bool) -> t a -> f Bool
anyM f = liftM (foldr (||) False) . mapM f

allM :: (Monad f, Traversable t) => (a -> f Bool) -> t a -> f Bool
allM f = liftM (foldr (&&) True) . mapM f

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM p [] = return Nothing
findM p (x:xs) = do
  found <- p x
  if found
    then return (Just x)
    else findM p xs
    
sortWith :: Ord b => (a -> b) -> [a] -> [a]
sortWith f = sortBy (compare `on` f)

instance Functor First where
  fmap f = First . fmap f . getFirst

minimalBy :: (a -> a -> Ordering) -> [a] -> [a]
minimalBy _ [] = []
minimalBy ord xs = y : (takeWhile ((== EQ) . ord y) ys)
  where (y:ys) = sortBy ord xs

nubOrd :: Ord a => [a] -> [a]
nubOrd = Set.toList . Set.fromList
  
elemOrd :: Ord a => a -> [a] -> Bool
elemOrd x = Set.member x . Set.fromList

intersectOrd :: Ord a => [a] -> [a] -> [a]
intersectOrd xs ys = Set.toList 
  $ Set.intersection (Set.fromList xs) (Set.fromList ys) 

  
  
