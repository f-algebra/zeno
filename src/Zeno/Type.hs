module Zeno.Type 
(
  Type (..), Typed (..), TVar,
  typeVarName,
  --unflatten, flatten,
  --isFun, isVar, fromVar, returns
) 
where

import Prelude ()
import Zeno.Prelude
import Zeno.Unification
import Zeno.Traversing
import Zeno.Name ( Name )

newtype TVar = TVar { typeVarName :: Name }
  deriving ( Eq, Ord, Show )

data Type a
  = Fun !(Type a) !(Type a)
  | Forall !TVar !(Type a)
  | Data !a
  | Var !TVar
  deriving ( Eq, Ord, Functor, Foldable, Traversable )
  
instance Empty a => Empty (Type a) where
  empty = Var empty
  
class Typed a where
  type Data a
  typeOf :: a -> Type (Data a)
  
instance Typed (Type a) where
  type Data (Type a) = a
  typeOf = id

fromVar :: Type a -> a
fromVar (Var x) = x

isFun :: Type a -> Bool
isFun (Fun {}) = True
isFun _ = False

isVar :: Type a -> Bool
isVar (Var {}) = True
isVar _ = False

{-
unflatten :: [Type a] -> Type a
unflatten ts = foldr1 Fun ts

flatten :: Type a -> [Type a]
flatten (Fun f a) = f : flatten a
flatten t = [t]

returns :: Type a -> Type a
returns = last . flatten
-}

instance WithinTraversable (Type a) (Type a) where
  mapWithinM f (Fun t1 t2) = 
    f =<< return Fun `ap` mapWithinM f t1 `ap` mapWithinM f t2
  mapWithinM f t = f t

instance HasVariables (Type a) where
  type Var (Type a) = TVar
  
  freeVars (Var v) = Set.singleton v
