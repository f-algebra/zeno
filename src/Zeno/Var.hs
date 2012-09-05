-- | The representation of variables inside Zeno.
module Zeno.Var (
  ZVar (name, sort), ZVarSort (..),
  ZDataType, ZType, ZTerm, ZAlt,
  ZClause, ZTermMap, ZEquation,
  setType, caseSplit, relabel,
  isConstructor, isConstructorTerm,
  isUniversal, universalVariables,
  distinguishFixes, isFunctionCall,
  new, declare, invent, clone, generalise,
  mapUniversal, foldUniversal,
  instantiateTerm, recursiveArguments,
  isDestructible, isHNF, magic
) where

import Prelude ()
import Zeno.Prelude hiding ( sort )
import Zeno.DataType ( DataType )
import Zeno.Type ( Type, Typed (..) )
import Zeno.Name ( Name, MonadUnique )
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
  show var 
    | isConstructor var = show (name var)
    | otherwise = show (Name.uniqueId $ name var)
  
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

-- |The different /sorts/ of variable within Zeno.
data ZVarSort
  = Constructor
  | Universal
                
instance Empty ZVar where
  empty = Var empty empty Universal

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
  
distinguishFixes :: forall m . MonadUnique m => ZTerm -> m ZTerm
distinguishFixes = mapWithinM distinguish
  where
  distinguish :: ZTerm -> m ZTerm
  distinguish cse@(Term.Cse {}) 
    | Term.FoldCase _ fix <- Term.caseOfSort cse = do
        new_name <- Name.invent
        return $ cse { Term.caseOfSort = Term.FoldCase new_name fix }
  distinguish (Term.Fix var term) = do
    new_var <- clone var
    new_term <- Substitution.replace
      (Term.Var var) (Term.Var new_var) term
    return (Term.Fix new_var new_term)
  distinguish other = 
    return other
    
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

  unifier (Term.Var v1) (Term.Var v2)
    | v1 == v2 = 
      return empty
  unifier (Term.App f1 a1) (Term.App f2 a2) =
    unifier f1 f2 `Substitution.unionM` unifier a1 a2
  unifier (Term.Lam v1 x1) (Term.Lam v2 x2) = do
    x2' <- Substitution.replace (Term.Var v2) (Term.Var v1) x2
    unifier x1 x2'
  unifier (Term.Fix v1 x1) (Term.Fix v2 x2) = do
    x2' <- Substitution.replace (Term.Var v2) (Term.Var v1) x2
    unifier x1 x2'
  unifier (Term.Cse _ t1 as1) (Term.Cse _ t2 as2)
    | length as1 /= length as2 = mzero
    | otherwise = 
      unifier t1 t2 `Substitution.unionM` alts_unifier
    where
    as1s = sortWith Term.altCon as1
    as2s = sortWith Term.altCon as2
    alts_unifier = 
      Substitution.unions
      =<< zipWithM unifier as1s as2s
  unifier x1 x2
    -- Pretty sure this should never occur
    | x1 == x2 = assert False $ return empty
  unifier (Term.Var x) t =
    return (Substitution.singleton x t)
  unifier _ _ =
    mzero
    
instance Unifiable ZAlt where
  type UniTerm ZAlt = ZTerm
  type UniVar ZAlt = ZVar

  unifier (Term.Alt k1 vs1 t1) (Term.Alt k2 vs2 t2)
    | k1 /= k2 = mzero
    | otherwise = do
      t2' <- Substitution.apply replace_vars t2
      unifier t1 t2' 
    where
    replace_vars = Substitution.fromList 
      $ zip (map Term.Var vs2) (map Term.Var vs1)
      
      
instance TermTraversable t ZVar => Substitution.Apply ZTerm t where
  apply map = mapTermsM app
    where
    app :: forall m . MonadUnique m => ZTerm -> m ZTerm 
    
    -- 'app' and 'appT' are mutually recursive, 
    -- 'appT' descends into sub-terms, 
    -- and 'app' applies the substitution to the result of 'appT'.
    app term = do
      term' <- appT term
      return (Substitution.try map term')
      where
      appT :: ZTerm -> m ZTerm
      appA :: ZAlt-> m ZAlt
      
      -- If the variable these 'Lam' or 'Fix'es are binding occurs
      -- freely in the substitution we are applying, then we need to 
      -- freshen this variable, 
      -- i.e. replace the underlying unique identifier.
      appT (Term.Lam var rhs) = do
        rhs' <-
          if var `Set.member` freeVars map
          then Term.freshenVar var rhs
          else return rhs
        return (Term.Lam var) `ap` app rhs'
        
      appT (Term.Fix var rhs) = do
        rhs' <-
          if var `Set.member` freeVars map
          then Term.freshenVar var rhs
          else return rhs
        return (Term.Fix var) `ap` app rhs'
        
      appT (Term.App lhs rhs) =
        return Term.App `ap` app lhs `ap` app rhs
      appT (Term.Cse srt term alts) = 
        return (Term.Cse srt) `ap` app term `ap` mapM appA alts
      appT (Term.Var x) = 
        return (Term.Var x)
        
      -- If any of the variables this pattern binds occur freely 
      -- in the substitution, then we must freshen them
      appA (Term.Alt con vars term) = do
        term' <- theFreshening term
        return (Term.Alt con vars) `ap` app term'
        where
        freshen_me = Set.toList
          $ freeVars map `Set.intersection` Set.fromList vars
        theFreshening = 
          concatEndosM (Term.freshenVar <$> freshen_me)
      
    


