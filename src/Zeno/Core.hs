-- |This module re-exports all of the modules that make up the /core/ of Zeno.
-- It also contains a lot of miscellaneous code that doesn't really belong anywhere
-- in particular.
module Zeno.Core (
  module Zeno.Type,
  module Zeno.Var,
  module Zeno.Name,
  module Zeno.Utils,
  module Zeno.Theory,
  module Zeno.Unification,
  module Zeno.Traversing,
  module Zeno.Show
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Var ( ZVar, ZDataType, ZAlt, ZTerm, 
                  ZClause, ZTermSubstitution, ZEquation,
                  CriticalPath, CriticalPair,
                  ZType, ZVarSort, HasSources (..) )
import Zeno.Name ( Name, Unique, UniqueGen (..) )
import Zeno.Type ( Typed (..) )
import Zeno.Utils
import Zeno.Traversing
import Zeno.Unification
import Zeno.Theory ( ZTheory )
import Zeno.Show

{-

-- |The return value from 'instantiateConstructors',
-- 'fst' is the instantiated constructor term,
-- 'snd' is the list of recursive variables in that term.
type ConstructorInstance = (ZTerm, [ZVar])

destructibleType :: Type a -> Bool
destructibleType = isVarType . head . flattenAppType

destructibleTerm :: ZTerm -> Bool
destructibleTerm term 
   = isTerm term
  && destructibleType (getType term)
  && notConstructor
  where
  notConstructor = fromMaybe False $ do
    fun <- termFunction term
    return (not (isConstructorVar fun))
  
destructibleVar :: ZVar -> Bool
destructibleVar = destructibleTerm . Var

recursiveConstructorInstance :: ConstructorInstance -> Bool
recursiveConstructorInstance (term, _) =
  case termFunction term of
    Just var -> isConstructorVar var 
             && isRecursiveConstructor (varClass var)
    Nothing -> False

proofCriticalTerm :: CriticalTerm -> Bool
proofCriticalTerm (term, src) =
  destructibleTerm term
  && not (null src || any invalid (allSources term))
  where
  invalid :: CriticalPath -> Bool
  invalid = orderedSupersetOf src

instantiateConstructors :: forall t m . (IdCounter t, MonadState t m) =>
    CriticalTerm -> m [ConstructorInstance]
instantiateConstructors (term, source) = mapM instantiate dt_cons
  where                          
  term_type = getType term
  VarType dtype@(DataType _ _ _ dt_cons) = 
    head (flattenAppType term_type)
  
  instantiate :: ZVar -> m ConstructorInstance
  instantiate con = do
    new_ids <- replicateM (length arg_types) newIdS
    let makeVar new_id new_type = ZVar new_id Nothing new_type var_class
        args = zipWith ($) (map makeVar new_ids) arg_types
        term = unflattenApp (map Var (con' : args))
        vars = filter ((== res_type) . varType) args
        
    let update :: (Typed a, TypeVar a ~ ZDataType) => a -> a
        update = conUnifier term
        
    return (update term, map update vars)
    where
    conUnifier :: (Typed a, TypeVar a ~ ZDataType) => ZTerm -> a -> a 
    conUnifier con_term = 
      case unify new_con_type term_type of
        NoUnifier -> error $
          "Couldn't unify types in constructor instantiation."
          ++ "\nTerm: " ++ showTyped term
          ++ "\nConstructor: " ++ showTyped con
        Unifier sub -> updateType sub
      where 
      new_con_type = unflattenForAllType poly_vars (getType con_term)

    (poly_vars, con_type) = flattenForAllType (getType con)
    flat_con_types = flattenFunType con_type
    (arg_types, [res_type]) = splitAt (length flat_con_types - 1) flat_con_types
    var_class = UniversalVar [source]
    con' = con { varType = con_type }
    
mergeCriticalTerms :: [CriticalTerm] -> [CriticalTerm] 
mergeCriticalTerms cterms = -- filterEach requiredSource `concatMap` clustered
  filterEach requiredSource cterms
  where
  clustered = clusterBy ((==) `on` fst) cterms

  requiredSource :: (CriticalTerm, [CriticalTerm]) -> Bool
  requiredSource ((_, src), other_terms) 
    = not 
    $ any (flip orderedSupersetOf src)
    $ map snd
    $ other_terms
 -}

