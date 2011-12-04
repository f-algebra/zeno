-- |Here we have the representation of variables inside Zeno; see 'ZVarClass' for the
-- different types of variable we can have.
module Zeno.Var (
  ZVar (name, sort), ZVarSort (..), HasSources (..),
  ZDataType, ZType, ZTerm, ZAlt,
  ZClause, ZTermSubstitution, ZEquation,
  CriticalPath, CriticalPair, 
  substituteTakingSources,
  isConstructor, isConstructorTerm,
  isUniversal, universalVariables,
  distinguishFixes,
  new, declare, invent, clone,
  mapUniversal, foldUniversal
) where

import Prelude ()
import Zeno.Prelude hiding ( sort )
import Zeno.DataType ( DataType )
import Zeno.Type ( Type, Typed (..) )
import Zeno.Name ( Unique, Name, UniqueGen )
import Zeno.Term ( Term, Alt, TermSubstitution )
import Zeno.Clause ( Clause, Equation )
import Zeno.Utils
import Zeno.Unification
import Zeno.Traversing

import qualified Zeno.Name as Name
import qualified Zeno.Term as Term
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
  
type CriticalPath = [Name]
type CriticalPair = (ZTerm, CriticalPath)

-- |The different /sorts/ of variable within Zeno.
data ZVarSort
  = Universal     { sources :: !(Set CriticalPath) }
  | Constructor
  | Bound

instance Typed ZVar where
  type SimpleType ZVar = ZDataType
  typeOf = varType

isConstructor :: ZVar -> Bool
isConstructor (sort -> Constructor {}) = True
isConstructor _ = False

isConstructorTerm :: ZTerm -> Bool
isConstructorTerm = 
  fromMaybe False . fmap isConstructor . Term.function 

isUniversal :: ZVar -> Bool
isUniversal (sort -> Universal {}) = True
isUniversal _ = False

distinguishFixes :: forall g m . (MonadState g m, UniqueGen g) => ZTerm -> m ZTerm
distinguishFixes = mapWithinM distinguish
  where
  distinguish :: ZTerm -> m ZTerm
  distinguish (Term.Fix var term) = do
    new_var <- clone var
    let new_term = replace var new_var term
    return (Term.Fix new_var new_term)
  distinguish other = 
    return other

new :: (MonadState g m, UniqueGen g) => 
  Maybe String -> ZType -> ZVarSort -> m ZVar
new label typ srt = do
  name <- Name.new label
  return (Var name typ srt)
  
declare :: (MonadState g m, UniqueGen g) => 
  String -> ZType -> ZVarSort -> m ZVar
declare = new . Just  
  
invent :: (MonadState g m, UniqueGen g) => ZType -> ZVarSort -> m ZVar
invent = new Nothing

clone :: (MonadState g m, UniqueGen g) => ZVar -> m ZVar
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

class HasSources a where
  allSources :: a -> (Set CriticalPath)
  addSources :: (Set CriticalPath) -> a -> a
  clearSources :: a -> a
  
instance HasSources ZVar where
  allSources (sort -> Universal srs) = srs
  allSources _ = mempty

  addSources more var@(sort -> Universal existing) =
    var { sort = Universal (more ++ existing) }
  addSources _ var = var
  
  clearSources var@(sort -> Universal _) =
    var { sort = Universal mempty }
  clearSources var = var
  
instance HasSources ZTerm where
  allSources = foldUniversal allSources
  addSources srs = mapUniversal (addSources srs)
  clearSources = mapUniversal clearSources
  
substituteTakingSources :: (Ord a, WithinTraversable a f, HasSources a) => 
    Substitution a a -> f -> f
{-# SPECIALISE substituteTakingSources :: 
      ZTermSubstitution -> ZTerm -> ZTerm #-}
{-# SPECIALISE substituteTakingSources :: 
      ZTermSubstitution -> ZClause -> ZClause #-}
substituteTakingSources sub = mapWithin $ \from ->
  case Map.lookup from sub of
    Nothing -> from
    Just to -> addSources (allSources from) to


