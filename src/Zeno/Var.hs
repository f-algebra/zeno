-- |Here we have the representation of variables inside Zeno; see 'ZVarClass' for the
-- different types of variable we can have.
module Zeno.Var (
  ZVar (name, sort), ZVarSort (..),
  ZDataType, ZType, ZTerm, ZAlt,
  ZClause, ZTermSubstitution, ZEquation,
  Context (..), 
  isConstructor, isConstructorTerm,
  isUniversal, universalVariables,
  distinguishFixes, freeZVars,
  new, declare, invent, clone, generalise,
  mapUniversal, foldUniversal,
  instantiateTerm, recursiveArguments,
  caseSplit, withinContext,
  destructible, isHNF
) where

import Prelude ()
import Zeno.Prelude hiding ( sort )
import Zeno.DataType ( DataType )
import Zeno.Type ( Type, Typed (..) )
import Zeno.Name ( Name, MonadUnique )
import Zeno.Term ( Term, Alt, TermSubstitution )
import Zeno.Logic ( Clause, Equation )
import Zeno.Utils
import Zeno.Show
import Zeno.Unification
import Zeno.Traversing

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
type ZTermSubstitution = TermSubstitution ZVar

data ZVar
  = Var         { name :: !Name,
                  varType :: ZType,
                  sort :: !ZVarSort }

instance Eq ZVar where
  (==) = (==) `on` name
  
instance Ord ZVar where
  compare = compare `on` name

-- |The different /sorts/ of variable within Zeno.
data ZVarSort
  = Constructor
  | Universal
  
data Context
  = Context   { contextFunction :: !(ZTerm -> ZTerm),
                contextArgType :: !ZType }
                
instance Empty ZVar where
  empty = Var empty empty Universal

instance Typed ZVar where
  type SimpleType ZVar = ZDataType
  typeOf = varType
  
instance Show ZVar where
  show = show . name 

isConstructor :: ZVar -> Bool
isConstructor (sort -> Constructor {}) = True
isConstructor _ = False

isConstructorTerm :: ZTerm -> Bool
isConstructorTerm = 
  fromMaybe False . fmap isConstructor . Term.function 
  
isHNF :: ZTerm -> Bool
isHNF term = Term.isVar term 
          || isConstructorTerm term

isUniversal :: ZVar -> Bool
isUniversal (sort -> Universal {}) = True
isUniversal _ = False
  
freeZVars :: (HasVariables a, Var a ~ ZVar) => a -> Set ZVar
freeZVars = Set.filter (not . isConstructor) . freeVars

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
    let new_term = replaceWithin (Term.Var var) (Term.Var new_var) term
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
  
destructible :: ZTerm -> Bool
destructible term = 
  Type.isVar (typeOf term) 
  && not (isConstructorTerm term)
  && not (Term.isCse term)
  
withinContext :: ZTerm -> Context -> Maybe ZTerm
withinContext term (Context cxt_func _) = 
  case unifier (cxt_func empty) term of
    NoUnifier -> Nothing
    Unifier sub ->
      case Map.toList sub of
        [(key, context_gap)] | key == empty ->
          Just context_gap
        _ -> 
          Nothing
  
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
  
declare :: MonadUnique m => String -> ZType -> ZVarSort -> m ZVar
declare = new . Just  
  
invent :: MonadUnique m => ZType -> ZVarSort -> m ZVar
invent = new Nothing

clone :: MonadUnique m => ZVar -> m ZVar
clone var = declare (show (name var)) (varType var) (sort var)

universalVariables :: Foldable f => f ZVar -> Set ZVar
universalVariables = Set.filter isUniversal . Set.fromList . toList

mapUniversal :: WithinTraversable ZTerm a => (ZVar -> ZVar) -> a -> a
mapUniversal f = mapWithin mapVars
  where 
  mapVars (Term.Var x) = Term.Var (f x)
  mapVars t = t
  
foldUniversal :: (WithinTraversable ZTerm a, Monoid m) => (ZVar -> m) -> a -> m
foldUniversal f = foldWithin foldVars
  where
  foldVars (Term.Var x) = f x
  foldVars _ = mempty
  
    
    
