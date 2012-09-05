-- | Removing my boilerplate.
-- Does not require qualified import.
module Zeno.Traversing (
  WithinTraversable (..),
  withinList, strictlyWithinList,
  contains, containsStrictly,
  removeSupersets, removeSubsets, anyWithin,
  replaceWithin,
  HasVariables (..), isFreeIn
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Utils ( mapM_to_fmap )

import qualified Data.Map as Map
import qualified Data.Set as Set

class WithinTraversable t f where
  mapWithinM :: Monad m => (t -> m t) -> f -> m f
  
  mapWithin :: (t -> t) -> f -> f
  mapWithin = mapM_to_fmap mapWithinM
  
  foldWithin :: Monoid m => (t -> m) -> f -> m
  foldWithin g = execWriter . mapWithinM (\t -> tell (g t) >> return t)

  
anyWithin :: forall t f .  WithinTraversable t f => 
  (t -> Bool) -> f -> Bool
anyWithin p = getAny . foldWithin (Any . p)

withinList :: WithinTraversable t f => f -> [t]
withinList = foldWithin return

replaceWithin :: (Eq t, WithinTraversable t f) => t -> t -> f -> f
replaceWithin from to = mapWithin repl
  where
  repl x | x == from = to
         | otherwise = x

strictlyWithinList :: (WithinTraversable t t, Eq t) => t -> [t]
strictlyWithinList t = filter (/= t) (withinList t)

contains :: (WithinTraversable t f, Eq t) => f -> t -> Bool
contains f = flip elem (withinList f)

containsStrictly :: (WithinTraversable t t, Eq t) => t -> t -> Bool
containsStrictly t = flip elem (strictlyWithinList t)

removeSubsets :: (WithinTraversable a a, Eq a) => [a] -> [a]
removeSubsets sets = filter (not . isSubset) sets
  where isSubset set = any (flip containsStrictly set) sets

removeSupersets :: (WithinTraversable a a, Eq a) => [a] -> [a]
removeSupersets sets = filter (not . isSuperset) sets
  where isSuperset set = any (containsStrictly set) sets
  
-- * These are here for want of a better place.

class Ord (Var a) => HasVariables a where
  type Var a
  freeVars :: a -> Set (Var a) 
  
instance (HasVariables a, HasVariables b, Var a ~ Var b) => 
    HasVariables (a, b) where
  type Var (a, b) = Var a
  freeVars (x, y) = freeVars x ++ freeVars y
  
isFreeIn :: HasVariables a => Var a -> a -> Bool
isFreeIn v = Set.member v . freeVars 
  
