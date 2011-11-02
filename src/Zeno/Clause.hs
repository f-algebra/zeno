module Zeno.Clause (
  Equality (..), Clause (..),
  flipEquality, equalityToPair, pairToEquality, 
  addAntecedent, removeAntecedent,
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Term
import Zeno.Traversing

data Equality a 
  = (:=:)       { equalityLeft :: !(Term a),
                  equalityRight :: !(Term a) }
  deriving ( Eq, Functor, Foldable, Traversable )

data Clause a
  = Clause      { clauseForAll :: !(Set a),
                  clauseConsequent :: !(Equality a),
                  clauseAntecedents :: ![Equality a] }
  deriving ( Eq, Foldable )
            
instance WithinTraversable (Term a) (Equality a) where
  mapWithinM f (t1 :=: t2) = do
    t1' <- mapWithinM f t1
    t2' <- mapWithinM f t2
    return (t1' :=: t2')
  
instance WithinTraversable (Term a) (Clause a) where
  mapWithinM f (Clause vars eq conds) = do
    eq' <- mapWithinM f eq
    conds' <- mapM (mapWithinM f) conds
    return (Clause vars eq' conds')
    
flipEquality :: Equality a -> Equality a 
flipEquality (l :=: r) = r :=: l

equalityToPair :: Equality a -> (Term a, Term a)
equalityToPair (l :=: r) = (l, r)

pairToEquality :: (Term a, Term a) -> Equality a
pairToEquality = uncurry (:=:)

flipClauseConsequent :: Clause a -> Clause a
flipClauseConsequent cs = cs 
  { clauseConsequent = flipEquality (clauseConsequent cs) }
  
addAntecedent :: Equality a -> Clause a -> Clause a
addAntecedent eq cs = cs 
  { clauseAntecedents = clauseAntecedents cs ++ [eq] }
  
removeAntecedent :: Eq a => Equality a -> Clause a -> Clause a
removeAntecedent eq cs = cs
  { clauseAntecedents = delete eq (clauseAntecedents cs) }

