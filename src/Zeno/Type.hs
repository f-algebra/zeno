module Zeno.Type (
  Type (..), InnerType (..), Typed (..), TypeVar (..),
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

type TypeSubstitution a = Substitution TypeVar (Type a)

data TypeVar
  = TypeVar { typevarId :: !Id,
              typevarName :: !(Maybe String) }

data Type a 
  = Type    { typeVars :: !(Set TypeVar),
              typeInner :: !(InnerType a) }
  deriving ( Eq, Ord, Functor, Foldable, Traversable )

data InnerType a
  = FunType !(InnerType a) !(InnerType a)
  | AppType !a ![InnerType a]
  | VarType !TypeVar
  deriving ( Eq, Ord, Functor, Foldable, Traversable )
  
class Typed a where
  type SimpleType a
  typeOf :: a -> Type (SimpleType a)
  
instance Eq TypeVar where
  (==) = (==) `on` typevarId

instance Ord TypeVar where
  compare = compare `on` typevarId
  
instance Show TypeVar where
  show (TypeVar _ (Just name)) = name
  show (TypeVar id _) = show id
  
instance Typed (Type a) where
  type SimpleType (Type a) = a
  typeOf = id
  
unflattenFunType :: [InnerType a] -> InnerType a
unflattenFunType ts = foldr1 FunType ts

flattenFunType :: InnerType a -> [InnerType a]
flattenFunType (FunType f a) = f : flattenFunType a
flattenFunType t = [t]

fromVarType :: InnerType a -> TypeVar
fromVarType (VarType x) = x

isFunType :: InnerType a -> Bool
isFunType (FunType {}) = True
isFunType _ = False

returnType :: InnerType a -> InnerType a
returnType = last . flattenFunType

instance Ord a => Unifiable (Type a) where
  type UniTerm (Type a) = InnerType a
  type UniVar (Type a) = TypeVar
  
  unifier (Type tvars1 t1) (Type tvars2 t2) =
    case unifier t1 t2 of
      NoUnifier -> NoUnifier
      Unifier sub -> validateSub sub
      
    where
    validateSub sub
      | null nfbs = Unifier sub
      | not (all validBind nfb_binds) = NoUnifier
      | otherwise = Unifier (sub' `Map.union` new_subs)
      where
      free_vars = tvars1 ++ tvars2
      bound_vars = Map.keysSet sub
      
      -- Non-free but bound variables (nfb) are okay as long as they 
      -- are map to another variable, in which case we just 
      -- check everything they map to is free, then just flip the mapping
      -- e.g. A -> B becomes B -> A if A is bound but B is free
      nfbs = Set.toList (bound_vars `Set.difference` free_vars)
      nfb_binds = map (fromJust . flip Map.lookup sub) nfbs
      
      validBind (VarType x) = x `Set.member` free_vars
      validBind _ = False
      
      bind_vars = map fromVarType nfb_binds
      new_subs = Map.fromList (bind_vars `zip` (map VarType nfbs))
      sub' = foldr Map.delete sub nfbs
      
  applyUnifier sub (Type tvars inner) =
    Type new_tvars (applyUnifier sub inner)
    where
    new_tvars = tvars `Set.difference` Map.keysSet sub
      

instance Ord a => Unifiable (InnerType a) where
  type UniTerm (InnerType a) = InnerType a
  type UniVar (InnerType a) = TypeVar
  
  unifier t1 t2
    | t1 == t2 = mempty
  unifier (FunType a1 r1) (FunType a2 r2) =
    unifier a1 a2 ++ unifier r1 r2
  unifier (AppType f1 as1) (AppType f2 as2)
    | f1 == f2 && length as1 == length as2 = 
        mconcat (zipWith unifier as1 as2)
    | otherwise = NoUnifier
  unifier t1 (VarType var) = 
    Unifier (Map.singleton var t1)
  unifier (VarType var) t2 = 
    Unifier (Map.singleton var t2)
    
  applyUnifier sub = 
    substitute (Map.mapKeysMonotonic VarType sub)


instance WithinTraversable (InnerType a) (Type a) where
  mapWithinM f (Type vars inner) = return (Type vars) `ap` mapWithinM f inner
  
instance WithinTraversable (InnerType a) (InnerType a) where
  mapWithinM f (FunType t1 t2) = 
    f =<< return FunType `ap` mapWithinM f t1 `ap` mapWithinM f t2
  mapWithinM f (AppType fn ags) = 
    f =<< return (AppType fn) `ap` mapM (mapWithinM f) ags
  mapWithinM f t = f t

