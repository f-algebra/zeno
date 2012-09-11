module Zeno.Substitution (
  Map, Apply (..),
  replace, isOneToOne,
  union, unionM, unions,
  singleton, elems, null, 
  fromList, toList, mapKeys
) where

import Prelude ()
import Zeno.Prelude hiding ( Map, null, union, toList )
import Zeno.Traversing

import qualified Control.Failure as Fail
import qualified Data.Map as DMap
import qualified Data.Set as Set

-- | This is just a 'Data.Map', but I needed to make sure I didn't 
-- accidently use 'Map.union' or the 'Monoid' instance. 
-- The union of two substitutions is not always a valid substitution
newtype Map a b = MapWrap { unwrapMap :: DMap.Map a b }
  deriving ( Functor, Foldable, Traversable )

-- | Allowing the substitution of things of type 't' within something of 
-- type 'f', e.g. variables within a term, or terms within a term.
-- Substitution may generate new variables if there is a naming clash
-- so the output must be able to create unique identifiers, 
-- i.e. is a 'MonadUnique'.
class (Ord t, WithinTraversable t f) => Apply t f where
  apply :: MonadUnique m => Map t t -> f -> m f
  apply = applyList . return
  
  -- | Applies a list of mappings, one after the other.
  -- Useful if mappings may apply to later mappings.
  -- TODO replace this with a method to collapse such a list into one mapping
  applyList :: MonadUnique m => [Map t t] -> f -> m f
  applyList list = concatEndosM (map apply list)

-- | Applies a substitution consisting of one single replacement,
-- the first argument for the second.
replace :: (MonadUnique m, Apply t f, Ord t) => t -> t -> f -> m f
replace from to = apply (singleton from to)

isOneToOne :: Ord b => Map a b -> Bool
isOneToOne = not . isNub . elems

-- | Unifies two substitution maps into a single substitution
-- which does both. Can fail if the two substitutions are conflicting.
union :: (Ord a, Eq b, MonadFailure m) => Map a b -> Map a b -> m (Map a b)
union (MapWrap map1) (MapWrap map2) = do
  Fail.unless (and $ DMap.elems inter)
  return 
    $ MapWrap
    $ DMap.union map1 map2
  where
  inter = DMap.intersectionWith (==) map1 map2
  
-- | Unifies two substitution maps, which themselves are the result of
-- potentially failing computations. The whole thing fails if either
-- computation fails, or the unioning fails.
unionM :: (Ord a, Eq b, MonadFailure m) => 
  m (Map a b) -> m (Map a b) -> m (Map a b)
unionM mm1 mm2 = do
  m1 <- mm1
  m2 <- mm2
  union m1 m2
  
-- | Fold 'union' over a list.
unions :: (Ord a, Eq b, MonadFailure m) => [Map a b] -> m (Map a b)
unions = foldrM union empty 

instance (HasVariables a, HasVariables b, Var a ~ Var b) => 
    HasVariables (Map a b) where
  type Var (Map a b) = Var b
  freeVars = foldMap freeVars . toList

instance Empty (Map a b) where
  empty = MapWrap DMap.empty


-- * 'Data.Map' functions

singleton :: a -> b -> Map a b
singleton x y = MapWrap (DMap.singleton x y)

elems :: Map a b -> [b]
elems = DMap.elems . unwrapMap

null :: Map a b -> Bool
null = DMap.null . unwrapMap

fromList :: Ord a => [(a, b)] -> Map a b
fromList = MapWrap . DMap.fromList

toList :: Map a b -> [(a, b)]
toList = DMap.toList . unwrapMap

mapKeys :: Ord k2 => (k1 -> k2) -> Map k1 a -> Map k2 a
mapKeys f = MapWrap . DMap.mapKeys f . unwrapMap


