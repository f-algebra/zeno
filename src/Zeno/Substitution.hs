module Zeno.Substitution (
  Map, Apply (..),
  replace, isOneToOne, try,
  union, unionM, unions,
  singleton, elems, null, fromList
) where

import Prelude ()
import Zeno.Prelude hiding ( Map, null, union )
import Zeno.Traversing
import Zeno.Unique ( MonadUnique )

import qualified Data.Map as DMap
import qualified Data.Set as Set

-- | This is just a 'Data.Map', but I needed to make sure I didn't 
-- accidently use 'Map.union' or the 'Monoid' instance. 
-- The union of two substitutions is not always a valid substitution
newtype Map a b = MapWrap { unwrapMap :: DMap.Map a b }

-- | Allowing the substitution of things of type 't' within something of 
-- type 'f', e.g. variables within a term, or terms within a term.
-- Substitution may generate new variables if there is a naming clash
-- so the output must be able to create unique identifiers, 
-- i.e. is a 'MonadUnique'.
class WithinTraversable t f => Apply t f where
  apply :: MonadUnique m => Map t t -> f -> m f

-- | Applies a substitution consisting of one single replacement,
-- the first argument for the second.
replace :: (MonadUnique m, Apply t f, Ord t) => t -> t -> f -> m f
replace from to = apply (singleton from to)
  
isOneToOne :: Ord b => Map a b -> Bool
isOneToOne = not . isNub . elems

-- | Attempt to apply a mapping to something, 
-- NOT a substituion within something,
-- this only attempts to replace the very top level thing it is given.
-- Returns the original argument back if the mapping fails.
try :: Ord t => Map t t -> t -> t
try (MapWrap map) term = DMap.findWithDefault term term map

-- | Unifies two substitution maps into a single substitution
-- which does both. Can fail with 'mzero'.
union :: (Ord a, Eq b, MonadPlus m) => Map a b -> Map a b -> m (Map a b)
union (MapWrap map1) (MapWrap map2) = do
  guard (and $ DMap.elems inter)
  return 
    $ MapWrap
    $ DMap.union map1 map2
  where
  inter = DMap.intersectionWith (==) map1 map2
  
-- Unifies two substitution maps, which themselves are the result of
-- potentially failing computations. The whole thing fails if either
-- computation fails, or the unioning fails.
unionM :: (Ord a, Eq b, MonadPlus m) => 
  m (Map a b) -> m (Map a b) -> m (Map a b)
unionM mm1 mm2 = do
  m1 <- mm1
  m2 <- mm2
  union m1 m2
  
-- | Fold 'union' over a list.
unions :: (Ord a, Eq b, MonadPlus m) => [Map a b] -> m (Map a b)
unions = foldl1M union

instance HasVariables b => HasVariables (Map a b) where
  type Var (Map a b) = Var b
  freeVars = foldMap freeVars . elems   

instance Empty (Map a b) where
  empty = MapWrap DMap.empty

-- * 'Data.Map' functions

singleton :: a -> b -> Map a b
singleton x y = MapWrap (DMap.singleton x y)

elems :: Map a b -> [b]
elems = DMap.elems . unwrapMap

null :: Map a b -> Bool
null = DMap.null . unwrapMap

fromList :: (Ord a, Foldable f) => f (a, b) -> Map a b
fromList = MapWrap . DMap.fromList . toList

