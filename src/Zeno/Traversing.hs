module Zeno.Traversing (
  WithinTraversable (..), HasVariables (..),
  Substitution, replaceWithin,
  withinList, strictlyWithinList,
  contains, containsStrictly,
  removeSupersets, removeSubsets,
  tryReplace, anyWithin
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Utils

import qualified Data.Map as Map
import qualified Data.Set as Set

type Substitution = Map

class WithinTraversable t f where
  mapWithinM :: Monad m => (t -> m t) -> f -> m f
  substitute :: Ord t => Substitution t t -> f -> f
  
  mapWithin :: (t -> t) -> f -> f
  mapWithin = mapM_to_fmap mapWithinM
  
  foldWithin :: Monoid m => (t -> m) -> f -> m
  foldWithin g = execWriter . mapWithinM (\t -> tell (g t) >> return t)
  
instance WithinTraversable t f => WithinTraversable t [f] where
  mapWithinM f = mapM (mapWithinM f)
  mapWithin f = map (mapWithin f)
  foldWithin f = foldMap (foldWithin f)
  substitute s = map (substitute s)

isOneToOne :: Ord b => Substitution a b -> Bool
isOneToOne = not . containsDuplicates . Map.elems

tryReplace :: Ord t => Substitution t t -> t -> t
tryReplace map term = Map.findWithDefault term term map

anyWithin :: forall t f .  WithinTraversable t f => (t -> Bool) -> f -> Bool
anyWithin p = getAny . foldWithin (Any . p)

replaceWithin :: (WithinTraversable t f, Ord t) => t -> t -> f -> f
replaceWithin from to = substitute (Map.singleton from to)

withinList :: WithinTraversable t f => f -> [t]
withinList = foldWithin return

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
  
-- |This is here for want of a better place for it
class HasVariables f where
  type Var f
  freeVars :: Ord (Var f) => f -> Set (Var f) 
  
