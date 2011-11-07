module Zeno.Clause (
  Equation (..), Clause (..), 
  addAntecedent, removeAntecedent,
  addForall
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Term
import Zeno.Traversing

import qualified Data.Set as Set

data Equation a 
  = Equal       { eqLeft :: !(Term a),
                  eqRight :: !(Term a) }
  deriving ( Eq, Foldable )

data Clause a
  = Clause      { forall :: !(Set a),
                  antecedents :: ![Clause a],
                  consequent :: !(Equation a) }
  deriving ( Eq, Foldable )
            
instance WithinTraversable (Term a) (Equation a) where
  mapWithinM f (Equal t1 t2) = do
    t1' <- mapWithinM f t1
    t2' <- mapWithinM f t2
    return (Equal t1' t2')
  
instance WithinTraversable (Term a) (Clause a) where
  mapWithinM f (Clause vars conds eq) = do
    eq' <- mapWithinM f eq
    conds' <- mapM (mapWithinM f) conds
    return (Clause vars conds' eq')
    
instance HasVariables (Equation a) where
  type Var (Equation a) = a
  freeVars (Equal e1 e2) = freeVars e1 ++ freeVars e2

instance HasVariables (Clause a) where
  type Var (Clause a) = a
  
  freeVars cls = 
    (consVars ++ antsVars) `Set.difference` (forall cls)
    where
    consVars = freeVars (consequent cls)
    antsVars = Set.unions $ map freeVars (antecedents cls)

addForall :: Ord a => a -> Clause a -> Clause a
addForall var cs = cs
  { forall = Set.insert var (forall cs) }
    
addAntecedent :: Clause a -> Clause a -> Clause a
addAntecedent eq cs = cs 
  { antecedents = antecedents cs ++ [eq] }
  
removeAntecedent :: Eq a => Clause a -> Clause a -> Clause a
removeAntecedent eq cs = cs
  { antecedents = delete eq (antecedents cs) }

