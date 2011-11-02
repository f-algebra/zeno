module Zeno.Term (
  Term (..), TermSubstitution, HasVariables (..),
  isVar, fromVar, isApp, isCse, isLam, isFix,
  flattenApp, unflattenApp, termFunction,
  flattenLam, unflattenLam, isOperator
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Id
import Zeno.Traversing
import Zeno.Utils
import Zeno.Unification

import qualified Data.Map as Map
import qualified Data.Set as Set

data Term a 
  = Var !a 
  | App !(Term a) !(Term a)
  | Lam !a !(Term a)
  | Fix !a !(Term a)
  | Cse     { caseOfId :: !Id,
              caseOfTerm :: !(Term a),
              caseOfAlts :: ![Alt a],
              caseOfDefault :: !(Maybe (Term a)) }
  deriving ( Eq, Ord, Functor, Foldable, Traversable )
  
data Alt a
  = Alt     { altCon :: !a,
              altVars :: ![a],
              altTerm :: !(Term a) }
  deriving ( Eq, Ord, Functor, Foldable, Traversable )
  
type TermSubstitution a = Substitution (Term a) (Term a)

class HasVariables f where
  freeVariables :: Ord a => f a -> Set a 

instance HasVariables Term where
  freeVariables (App e1 e2) = freeVariables e1 ++ freeVariables e2
  freeVariables (Var x) = Set.singleton x
  freeVariables (Lam x e) = Set.delete x (freeVariables e) 
  freeVariables (Fix f e) = Set.delete f (freeVariables e)
  freeVariables cse@(Cse {}) =
    freeVariables (caseOfTerm cse) ++ defVars ++ altVars
    where
    altVars = caseOfAlts cse |> map freeVariables |> Set.unions
    defVars = caseOfDefault cse |> map freeVariables |> fromMaybe mempty
  
instance HasVariables Alt where
  freeVariables (Alt _ vars e) = 
    Set.difference (freeVariables e) (Set.fromList vars)

isVar :: Term a -> Bool
isVar (Var {}) = True
isVar _ = False

isApp :: Term a -> Bool
isApp (App {}) = True
isApp _ = False

isCse :: Term a -> Bool
isCse (Cse {}) = True
isCse _ = False

isLam :: Term a -> Bool
isLam (Lam {}) = True
isLam _ = False

isFix :: Term a -> Bool
isFix (Fix {}) = True
isFix _ = False

fromVar :: Term a -> a
fromVar (Var v) = v

termFunction :: Term a -> Maybe a
termFunction (flattenApp -> (Var x : _)) = Just x
termFunction _ = Nothing

flattenApp :: Term a -> [Term a]
flattenApp (App lhs rhs) = flattenApp lhs ++ [rhs]
flattenApp expr = [expr]

unflattenApp :: [Term a] -> Term a
unflattenApp = foldl1 App

flattenLam :: Term a -> ([a], Term a)
flattenLam (Lam v rhs) = 
  let (vs, rhs') = flattenLam rhs in (v : vs, rhs')
flattenLam expr = ([], expr)

unflattenLam :: [a] -> Term a -> Term a
unflattenLam = flip (foldr Lam)

instance Ord a => Unifiable (Term a) where
  type UniTerm (Term a) = Term a
  type UniVar (Term a) = a

  unifier (Var v1) (Var v2)
    | v1 == v2 = mempty
  unifier (App f1 a1) (App f2 a2) =
    unifier f1 f2 `mappend` unifier a1 a2
  unifier (Lam v1 x1) (Lam v2 x2)
    | v1 == v2 = unifier x1 x2
  unifier (Fix v1 x1) (Fix v2 x2)
    | v1 == v2 = unifier x1 x2
  unifier x1 x2 
    | x1 == x2 = mempty
  unifier (Var x) expr =
    Unifier (Map.singleton x expr)
  unifier _ _ = error "need to implement unification for case"  --NoUnifier
  
  applyUnifier sub =
    substitute (Map.mapKeysMonotonic Var sub)

instance WithinTraversable (Term a) (Term a) where
  mapWithinM f (App lhs rhs) =
    f =<< return App `ap` mapWithinM f lhs `ap` mapWithinM f rhs
  mapWithinM f (Cse id lhs alts def) =
    f =<< return (Cse id) `ap` mapWithinM f lhs 
                          `ap` mapM (mapWithinM f) alts
                          `ap` mapM (mapWithinM f) def
  mapWithinM f (Lam var rhs) =
    f =<< return (Lam var) `ap` mapWithinM f rhs
  mapWithinM f (Fix var rhs) =
    f =<< return (Fix var) `ap` mapWithinM f rhs
  mapWithinM f expr =
    f =<< return expr

instance WithinTraversable (Term a) (Alt a) where
  mapWithinM f (Alt con binds rhs) = 
    return (Alt con binds) `ap` mapWithinM f rhs
  
isOperator :: String -> Bool
isOperator | error "find where isOperator should go" = any (not . isNormalChar)
  where
  isNormalChar :: Char -> Bool
  isNormalChar '_' = True
 -- isNormalChar '$' = True
  isNormalChar '.' = True
  isNormalChar c = isAlphaNum c
    

  
