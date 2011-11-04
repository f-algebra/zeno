-- |Here we have the representation of variables inside Zeno; see 'ZVarClass' for the
-- different types of variable we can have.
module Zeno.Var (
  ZVar (..), ZVarClass (..), HasSources (..),
  ZDataType, ZType, ZTerm, ZAlt,
  ZClause, ZTermSubstitution, ZEquality,
  CriticalPath, CriticalPair, 
  substituteTakingSources,
  defaultVarClass, isConstructorVar, isConstructorTerm,
  isUniversalVar, universalVariables,
  freshVariable, isVariableFunApp,
) where

import Prelude ()
import Zeno.Prelude
import Zeno.DataType
import Zeno.Type
import Zeno.Id
import Zeno.Term
import Zeno.Clause
import Zeno.Utils
import Zeno.Unification
import Zeno.Traversing

import qualified Data.Map as Map
import qualified Data.Set as Set

type ZDataType = DataType ZVar
type ZType = Type ZDataType
type ZTerm = Term ZVar
type ZAlt = Alt ZVar
type ZClause = Clause ZVar
type ZEquality = Equality ZVar
type ZTermSubstitution = TermSubstitution ZVar

data ZVar
  = ZVar        { varId :: !Id,
  
                  varName :: !(Maybe String),
                  
                  -- |The variable's 'Type'. This is non-strict so that we can tie the
                  -- knot for "variables have types which are made of data-types that
                  -- have constructors which are variables".
                  varType :: ZType,
                  
                  varClass :: !ZVarClass }

instance Eq ZVar where
  (==) = (==) `on` varId
  
instance Ord ZVar where
  compare = compare `on` varId
  
type CriticalPath = [Id]
type CriticalPair = (ZTerm, CriticalPath)

-- |The different /classes/ of variable within Zeno.
data ZVarClass
  = UniversalVar    { varSources :: !(Set CriticalPath) }
  | ConstructorVar
  | FixedVar

instance Typed ZVar where
  type SimpleType ZVar = ZDataType
  typeOf = varType

defaultVarClass :: ZVarClass
defaultVarClass = UniversalVar mempty

isConstructorVar :: ZVar -> Bool
isConstructorVar (varClass -> ConstructorVar {}) = True
isConstructorVar _ = False

isConstructorTerm :: ZTerm -> Bool
isConstructorTerm = 
  fromMaybe False . fmap isConstructorVar . termFunction 

isUniversalVar :: ZVar -> Bool
isUniversalVar (varClass -> UniversalVar {}) = True
isUniversalVar _ = False

isVariableFunApp :: ZTerm -> Bool
isVariableFunApp term@(termFunction -> Just fun) = 
  isApp term && isUniversalVar fun
isVariableFunApp _ = False

freshVariable :: (MonadState s m, IdCounter s) => ZVar -> m ZVar
freshVariable (ZVar id _ typ cls) = do
  new_id <- newIdS
  return (ZVar new_id Nothing typ cls)
      
universalVariables :: Foldable f => f ZVar -> Set ZVar
universalVariables = Set.filter isUniversalVar . Set.fromList . toList

class HasSources a where
  allSources :: a -> (Set CriticalPath)
  addSources :: (Set CriticalPath) -> a -> a
  clearSources :: a -> a
  
instance HasSources ZVar where
  allSources (varClass -> UniversalVar srs) = srs
  allSources _ = mempty

  addSources more var@(varClass -> UniversalVar existing) =
    var { varClass = UniversalVar (more ++ existing) }
  addSources _ var = var
  
  clearSources var@(varClass -> UniversalVar _) =
    var { varClass = UniversalVar mempty }
  clearSources var = var
  
instance (Foldable f, Functor f) => HasSources (f ZVar) where
  {-# SPECIALISE instance HasSources ZTerm #-}
  allSources = concatMap allSources . nubOrd . toList
  addSources srs = fmap (addSources srs)
  clearSources = fmap clearSources
  
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


