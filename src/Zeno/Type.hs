module Zeno.Type (
  Type (..), Typed (..), 
  unflattenFunType, flattenFunType, isFunType, 
  fromVarType, returnType
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Id
import Zeno.Unification
import Zeno.Traversing

import qualified Data.Map as Map
import qualified Data.Set as Set

data Type a
  = FunType !(Type a) !(Type a)
  | VarType !a
  deriving ( Eq, Ord, Functor, Foldable, Traversable )
  
class Typed a where
  type SimpleType a
  typeOf :: a -> Type (SimpleType a)
  
instance Typed (Type a) where
  type SimpleType (Type a) = a
  typeOf = id
  
unflattenFunType :: [Type a] -> Type a
unflattenFunType ts = foldr1 FunType ts

flattenFunType :: Type a -> [Type a]
flattenFunType (FunType f a) = f : flattenFunType a
flattenFunType t = [t]

fromVarType :: Type a -> a
fromVarType (VarType x) = x

isFunType :: Type a -> Bool
isFunType (FunType {}) = True
isFunType _ = False

returnType :: Type a -> Type a
returnType = last . flattenFunType

instance WithinTraversable (Type a) (Type a) where
  mapWithinM f (FunType t1 t2) = 
    f =<< return FunType `ap` mapWithinM f t1 `ap` mapWithinM f t2
  mapWithinM f t = f t

