module Zeno.Type (
  Type (..), Typed (..), 
  unflatten, flatten,
  isFun, isVar, fromVar, returns
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Unification
import Zeno.Traversing

data Type a
  = Fun !(Type a) !(Type a)
  | Var !a
  deriving ( Eq, Ord, Functor, Foldable, Traversable )
  
class Typed a where
  type SimpleType a
  typeOf :: a -> Type (SimpleType a)
  
instance Typed (Type a) where
  type SimpleType (Type a) = a
  typeOf = id
  
unflatten :: [Type a] -> Type a
unflatten ts = foldr1 Fun ts

flatten :: Type a -> [Type a]
flatten (Fun f a) = f : flatten a
flatten t = [t]

fromVar :: Type a -> a
fromVar (Var x) = x

isFun :: Type a -> Bool
isFun (Fun {}) = True
isFun _ = False

isVar :: Type a -> Bool
isVar (Var {}) = True
isVar _ = False

returns :: Type a -> Type a
returns = last . flatten

instance WithinTraversable (Type a) (Type a) where
  mapWithinM f (Fun t1 t2) = 
    f =<< return Fun `ap` mapWithinM f t1 `ap` mapWithinM f t2
  mapWithinM f t = f t

