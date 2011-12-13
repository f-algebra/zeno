module Zeno.Logic (
  Equation (..), Clause (..), 
  addAntecedent, removeAntecedent, flatten
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
  = Clause      { antecedents :: ![Equation a],
                  consequent :: !(Equation a) }
  deriving ( Eq, Foldable )
            
instance Ord a => WithinTraversable (Term a) (Equation a) where
  mapWithinM f (Equal t1 t2) = do
    t1' <- mapWithinM f t1
    t2' <- mapWithinM f t2
    return (Equal t1' t2')
  
instance Ord a => WithinTraversable (Term a) (Clause a) where
  mapWithinM f (Clause conds eq) = do
    eq' <- mapWithinM f eq
    conds' <- mapM (mapWithinM f) conds
    return (Clause conds' eq')
    
instance HasVariables (Equation a) where
  type Var (Equation a) = a
  freeVars (Equal e1 e2) = freeVars e1 ++ freeVars e2

instance HasVariables (Clause a) where
  type Var (Clause a) = a
  
  freeVars cls = consVars ++ antsVars
    where
    consVars = freeVars (consequent cls)
    antsVars = Set.unions $ map freeVars (antecedents cls)
    
instance TermTraversable Clause where
  mapTermsM f (Clause antes consq) = 
    Clause <$> mapM (mapTermsM f) antes <*> mapTermsM f consq
    
  mapTerms f (Clause antes consq) = 
    Clause (map (mapTerms f) antes) (mapTerms f consq)
    
  termList (Clause antes consq) = 
    concatMap termList (antes ++ [consq])
    
instance TermTraversable Equation where
  mapTermsM f (Equal t1 t2) = 
    Equal <$> f t1 <*> f t2
    
  mapTerms f (Equal t1 t2) = 
    Equal (f t1) (f t2)
    
  termList (Equal t1 t2) = [t1, t2]
    
addAntecedent :: Equation a -> Clause a -> Clause a
addAntecedent eq cs = cs 
  { antecedents = antecedents cs ++ [eq] }
  
removeAntecedent :: Eq a => Equation a -> Clause a -> Clause a
removeAntecedent eq cs = cs
  { antecedents = delete eq (antecedents cs) }

flatten :: Clause a -> [Equation a]
flatten cls = antecedents cls ++ [consequent cls]

