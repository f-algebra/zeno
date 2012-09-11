-- | The representation of variables inside Zeno.
module Zeno.Var (
  ZVar (name, sort), ZVarSort (..),
  ZDataType, ZType, ZTerm, ZAlt,
  ZClause, ZTermMap, ZEquation,
  setType, caseSplit, relabel,
  isConstructor, isConstructorTerm,
  isUniversal, universalVariables, isFunctionCall,
  new, declare, invent, clone, generalise,
  mapUniversal, foldUniversal,
  instantiateTerm, recursiveArguments,
  isDestructible, isHNF, magic
) where

import Prelude ()
import Zeno.Prelude hiding ( sort )
import Zeno.DataType ( DataType )
import Zeno.Type ( Type, Typed (..) )
import Zeno.Name ( Name )
import Zeno.Term ( Term, Alt, TermMap, TermTraversable (..) )
import Zeno.Logic ( Clause, Equation )
import Zeno.Utils
import Zeno.Show ()
import Zeno.Unification
import Zeno.Traversing

import qualified Zeno.Substitution as Substitution
import qualified Zeno.DataType as DataType
import qualified Zeno.Name as Name
import qualified Zeno.Term as Term
import qualified Zeno.Type as Type
import qualified Control.Failure as Fail
import qualified Control.Unique as Unique
import qualified Data.Map as Map
import qualified Data.Set as Set

type ZDataType = DataType ZVar
type ZType = Type ZDataType
type ZTerm = Term ZVar
type ZAlt = Alt ZVar
type ZClause = Clause ZVar
type ZEquation = Equation ZVar
type ZTermMap = TermMap ZVar

data ZVar
  = Var         { name :: !Name,
                  varType :: ZType,
                  sort :: !ZVarSort }

instance Eq ZVar where
  (==) = (==) `on` name
  
instance Ord ZVar where
  compare = compare `on` name
  
instance Show ZVar where
  show = show . name
  
instance Name.Has ZVar where
  get = name
  freshen var = do
    fresh_name <- Name.freshen (name var)
    return $ var { name = fresh_name } 

instance HasVariables ZVar where
  type Var ZVar = ZVar
  freeVars var
    | isConstructor var = Set.empty
    | otherwise = Set.singleton var
    
instance Empty ZVar where
  empty = Var empty empty Universal

-- |The different /sorts/ of variable within Zeno.
data ZVarSort
  = Constructor
  | Universal
                
-- instance Empty ZVar where
  -- empty = Var empty empty Universal

instance Typed ZVar where
  type SimpleType ZVar = ZDataType
  typeOf = varType

setType :: ZVar -> ZType -> ZVar
setType var typ = var { varType = typ }
  
isConstructor :: ZVar -> Bool
isConstructor (sort -> Constructor {}) = True
isConstructor _ = False

isConstructorTerm :: ZTerm -> Bool
isConstructorTerm (Term.function -> Term.Var f_var) =
  isConstructor f_var
isConstructorTerm _ = False

isHNF :: ZTerm -> Bool
isHNF term = Term.isVar term 
          || isConstructorTerm term

isUniversal :: ZVar -> Bool
isUniversal (sort -> Universal {}) = True
isUniversal _ = False
    
instantiateTerm :: MonadUnique m => ZTerm -> m ZTerm
instantiateTerm term
  | Type.isVar (typeOf term) = return term
instantiateTerm term = do
  new_arg <- invent arg_type Universal
  instantiateTerm (Term.App term (Term.Var new_arg))
  where
  Type.Fun arg_type _ = typeOf term
  
caseSplit :: MonadUnique m => ZDataType -> m [ZTerm]
caseSplit = mapM (instantiateTerm . Term.Var) . DataType.constructors
  
isDestructible :: ZTerm -> Bool
isDestructible term = 
  Type.isVar (typeOf term) 
  && not (isConstructorTerm term)
  && not (Term.isCse term)
  
isFunctionCall :: ZTerm -> Bool
isFunctionCall term =
  Term.isFix (Term.function term)
  && Type.isVar (typeOf term)
  
recursiveArguments :: ZTerm -> [ZTerm]
recursiveArguments term 
  | not (Term.isApp term) = []
  | otherwise = filter ((== typ) . typeOf) args
  where
  typ = typeOf term
  args = tail (Term.flattenApp term)

generalise :: MonadUnique m => ZTerm -> m ZVar
generalise term = invent (typeOf term) Universal

new :: MonadUnique m => Maybe String -> ZType -> ZVarSort -> m ZVar
new label typ srt = do
  name <- Name.new label
  return (Var name typ srt)
  
-- | Makes a new magic variable which can have any type 
-- and will be globally unique. This function is unsafe.
magic :: String -> ZVar
magic lbl = Var (Name.unsafe lbl) empty Universal 

declare :: MonadUnique m => String -> ZType -> ZVarSort -> m ZVar
declare = new . Just  
  
invent :: MonadUnique m => ZType -> ZVarSort -> m ZVar
invent = new Nothing

clone :: MonadUnique m => ZVar -> m ZVar
clone var = declare (show (name var)) (varType var) (sort var)

relabel :: String -> ZVar -> ZVar
relabel lbl var = var { name = Name.relabel lbl (name var) }

universalVariables :: Foldable f => f ZVar -> Set ZVar
universalVariables = Set.filter isUniversal . Set.fromList . toList

mapUniversal :: WithinTraversable ZTerm a => (ZVar -> ZVar) -> a -> a
mapUniversal f = mapWithin mapVars
  where 
  mapVars (Term.Var x) = Term.Var (f x)
  mapVars t = t
  
foldUniversal :: (WithinTraversable ZTerm a, Monoid m) =>
  (ZVar -> m) -> a -> m
foldUniversal f = foldWithin foldVars
  where
  foldVars (Term.Var x) = f x
  foldVars _ = mempty
  
  
-- * I think the instances below can be abstracted away from 'ZVar'
-- and put in Zeno.Term
-- but I couldn't be bothered to work out all the constraints.
  
instance Unifiable ZTerm where
  type UniTerm ZTerm = ZTerm
  type UniVar ZTerm = ZVar

  unifier x y = Unique.localGenT (uni x y)
    where
    uni :: (MonadUnique m, MonadFailure m) =>
      ZTerm -> ZTerm -> m (Substitution.Map ZVar ZTerm) 
    uni (Term.Var v1) (Term.Var v2)
      | v1 == v2 = 
        return empty
    uni (Term.Var x) t =
      return (Substitution.singleton x t)
    uni (Term.App f1 a1) (Term.App f2 a2) =
      uni f1 f2 `Substitution.unionM` uni a1 a2
    uni (Term.Lam v1 x1) (Term.Lam v2 x2) = do
      x2' <- Substitution.replace (Term.Var v2) (Term.Var v1) x2
      uni x1 x2'
    uni (Term.Fix v1 x1) (Term.Fix v2 x2) = do
      x2' <- Substitution.replace (Term.Var v2) (Term.Var v1) x2
      uni x1 x2'
    uni (Term.Cse _ t1 as1) (Term.Cse _ t2 as2) = do
      Fail.when (length as1 /= length as2)
      uni t1 t2 `Substitution.unionM` alts_uni
      where
      as1s = sortWith Term.altCon as1
      as2s = sortWith Term.altCon as2
      alts_uni = 
        Substitution.unions
        =<< zipWithM uniA as1s as2s
    uni x1 x2
      -- Pretty sure this should never occur
      | x1 == x2 = assert False $ return empty
    uni _ _ =
      Fail.here
      
    uniA :: (MonadUnique m, MonadFailure m) =>
      ZAlt -> ZAlt -> m (Substitution.Map ZVar ZTerm) 
    uniA (Term.Alt k1 vs1 t1) (Term.Alt k2 vs2 t2) = do
      Fail.when (k1 /= k2)
      t2' <- Substitution.apply replace_vars t2
      uni t1 t2' 
      where
      replace_vars = Substitution.fromList 
        $ zip (map Term.Var vs2) (map Term.Var vs1)


instance TermTraversable t ZVar => Substitution.Apply ZTerm t where
  apply mapping = mapTermsM app
    where
    mapping_list = Substitution.toList mapping
    
    applyMapping :: ZTerm -> ZTerm
    applyMapping term
      | Just (_, target) <- mby_target = target
      | otherwise = term
      where
      mby_target = find (alphaEq term . fst) mapping_list
    
    -- 'app' and 'appT' are mutually recursive, 
    -- 'appT' descends into sub-terms, 
    -- and 'app' applies the substitution to the result of 'appT'.
    app :: forall m . MonadUnique m => ZTerm -> m ZTerm
    app = liftM applyMapping . appT
      where
      free_vars = freeVars mapping
      
      appT :: ZTerm -> m ZTerm
      appA :: ZAlt -> m ZAlt
      
      -- If the variable these 'Lam' or 'Fix'es are binding occurs
      -- freely in the substitution we are applying, then we need to 
      -- freshen this variable, 
      -- i.e. replace the underlying unique identifier.
      appT (Term.Lam var rhs) 
        | var `Set.member` free_vars = do
            var' <- Name.freshen var
            rhs' <- Substitution.replace (Term.Var var) (Term.Var var') rhs
            return (Term.Lam var') `ap` app rhs'
        | otherwise = 
            return (Term.Lam var) `ap` app rhs
           
      appT (Term.Fix var rhs) 
        | var `Set.member` free_vars = do
            var' <- Name.freshen var
            rhs' <- Substitution.replace (Term.Var var) (Term.Var var') rhs
            return (Term.Fix var') `ap` app rhs'
        | otherwise =
            return (Term.Fix var) `ap` app rhs

      appT (Term.App lhs rhs) =
        return Term.App `ap` app lhs `ap` app rhs
      appT (Term.Cse srt term alts) = 
        return (Term.Cse srt) `ap` app term `ap` mapM appA alts
      appT (Term.Var x) = 
        return (Term.Var x)
        
      -- If any of the variables this pattern binds occur freely 
      -- in the substitution, then we must freshen them
      appA (Term.Alt con vars term) = do
        vars' <- mapM maybeFreshenVar vars
        let changed_vars = 
              filter (uncurry (/=)) 
              $ vars `zip` vars'
        term' <- foldrM updateTerm term changed_vars
        return (Term.Alt con vars') `ap` app term'
        where
        updateTerm :: (ZVar, ZVar) -> ZTerm -> m ZTerm
        updateTerm (v, v') = 
          Substitution.replace (Term.Var v) (Term.Var v')
        
        maybeFreshenVar :: ZVar -> m ZVar
        maybeFreshenVar var 
          | var `Set.member` free_vars = Name.freshen var
          | otherwise = return var


