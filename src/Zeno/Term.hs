{-# LANGUAGE UndecidableInstances #-}
module Zeno.Term (
  Term (..), Alt (..),
  TermMap, TermTraversable (..),
  isVar, fromVar, isApp, isCse, isLam, isFix,
  flattenApp, unflattenApp, flattenLam, unflattenLam,
  function, isNormal, freeCaseTags, isFixTerm, 
  toTermMap, mapCaseBranchesM, foldCaseBranchesM,
  stripLambdas, etaReduce, setFreeTags,
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Name ( Name )
import Zeno.Traversing
import Zeno.Type ( Type, Typed (..) )
import Zeno.Unification

import qualified Zeno.Substitution as Substitution
import qualified Zeno.Type as Type
import qualified Zeno.Name as Name
import qualified Data.Map as Map
import qualified Data.Set as Set

data Term a 
  = Var !a 
  | App !(Term a) !(Term a)
  | Lam !a !(Term a)
  | Fix !a !(Term a)
  | Cse     { -- | This is used for evaluation
              caseOfTag :: a,
              caseOfTerm :: !(Term a),
              caseOfAlts :: ![Alt a] }
  deriving ( Eq, Ord, Functor, Foldable, Traversable )
  
data Alt a
  = Alt     { altCon :: !a,
              altVars :: ![a],
              altTerm :: !(Term a) }
  deriving ( Eq, Ord, Functor, Foldable, Traversable )
  
type TermMap a = Substitution.Map (Term a) (Term a)

instance Empty a => Empty (Term a) where
  empty = Var empty

instance (HasVariables a, Var a ~ a) => HasVariables (Term a) where
  type Var (Term a) = a
  
  freeVars (App e1 e2) = freeVars e1 ++ freeVars e2
  freeVars (Var x) = freeVars x
  freeVars (Lam x e) = Set.delete x (freeVars e) 
  freeVars (Fix f e) = Set.delete f (freeVars e)
  freeVars cse@(Cse {}) =
    freeVars (caseOfTerm cse) ++ altVars
    where
    altVars = concatMap freeVars (caseOfAlts cse)
  
instance (HasVariables a, Var a ~ a) => HasVariables (Alt a) where
  type Var (Alt a) = a
  
  freeVars (Alt _ vars e) = 
    Set.difference (freeVars e) (Set.fromList vars)
    
-- | There are many things which contain 'Term's (e.g. formulae),
-- this class allows us to traverse these. Note that it only applies
-- to top-level terms and does not recurse into sub-terms.
class TermTraversable t a | t -> a where
  mapTermsM :: Monad m => (Term a -> m (Term a)) -> t -> m t

  mapTerms :: (Term a -> Term a) -> t -> t
  mapTerms f = runIdentity . mapTermsM (return . f)   
  
  termList :: t -> [Term a]
  termList = execWriter . mapTermsM (\t -> tell [t] >> return t)
  
instance TermTraversable (Term a) a where
  mapTermsM = ($)
  mapTerms = ($)
  termList = pure
  
instance TermTraversable (Alt a) a where
  mapTermsM f (Alt con vars term) =
    Alt con vars `liftM` f term
  mapTerms f (Alt con vars term) =
    Alt con vars (f term)
  termList =
    return . altTerm
    
instance TermTraversable t a => TermTraversable [t] a where
  mapTermsM f = mapM (mapTermsM f)
  mapTerms f = map (mapTerms f)
  termList = concatMap termList
  
instance (TermTraversable t1 a, TermTraversable t2 a) 
  => TermTraversable (t1, t2) a where
  
  mapTermsM f (x1, x2) = do
    x1' <- mapTermsM f x1
    x2' <- mapTermsM f x2
    return (x1', x2')
    
  mapTerms f (x1, x2) = 
    (mapTerms f x1, mapTerms f x2)
  
  termList (x1, x2) =
    termList x1 ++ termList x2
  
 
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

isFixTerm :: Term a -> Bool
isFixTerm = isFix . head . flattenApp

isNormal :: Ord a => Term a -> Bool
isNormal = not . anyWithin isFix

freeCaseTags :: Ord a => Term a -> Set a
freeCaseTags (App t1 t2) = freeCaseTags t1 `mappend` freeCaseTags t2
freeCaseTags (Lam _ t) = freeCaseTags t
freeCaseTags (Cse tag term alts) = 
  Set.insert tag inner
  where
  inner = concatMap freeCaseTags (term : map altTerm alts)
freeCaseTags _ = mempty

setFreeTags :: a -> Term a -> Term a
setFreeTags new_tag = setFree
  where
  setFree (App t1 t2) = App (setFree t1) (setFree t2)
  setFree (Lam x t) = Lam x (setFree t)
  setFree (Cse _ term alts) = 
    Cse new_tag (setFree term) (map setFreeA alts)
  setFree other = other
    
  setFreeA (Alt con vars term) =
    Alt con vars (setFree term)
    
fromVar :: Term a -> a
fromVar (Var v) = v

function :: Term a -> Term a
function = head . flattenApp

flattenApp :: Term a -> [Term a]
flattenApp (App lhs rhs) = flattenApp lhs ++ [rhs]
flattenApp expr = [expr]

unflattenApp :: [Term a] -> Term a
unflattenApp = foldl1 App

flattenLam :: Term a -> ([a], Term a)
flattenLam (Lam v rhs) = 
  let (vs, rhs') = flattenLam rhs in (v : vs, rhs')
flattenLam expr = ([], expr)

stripLambdas :: Term a -> Term a
stripLambdas = snd . flattenLam

unflattenLam :: [a] -> Term a -> Term a
unflattenLam = flip (foldr Lam)

-- | Converts a mapping produced by a unification, i.e. vars to terms,
-- into one which can be used for substitution, i.e. terms to terms.
toTermMap :: Ord a => Substitution.Map a (Term a) -> TermMap a
toTermMap = Substitution.mapKeys Var

etaReduce :: Eq a => Term a -> Term a
etaReduce (Lam x (App f y))
  | (Var x) == y = etaReduce f
etaReduce other = other

mapCaseBranchesM :: forall a m . 
    (Substitution.Apply (Term a) (Term a), Monad m) => 
  ([(Term a, Term a)] -> Term a -> m (Term a)) -> Term a -> m (Term a)
mapCaseBranchesM func = flip runReaderT [] . doMap
  where
  doMap :: Term a -> ReaderT [(Term a, Term a)] m (Term a)
    
  doMap (Cse srt cse_of alts) =
    return (Cse srt cse_of) `ap` mapM doMapAlt alts
    where
    doMapAlt (Alt con vars term) =
      local ((cse_of, pattern) :)
        $ return (Alt con vars) `ap` doMap term
      where
      pattern = unflattenApp
        $ map Var (con : vars)
        
  doMap term = do
    matches <- ask
    lift (func matches term)

foldCaseBranchesM :: forall a m b . (Substitution.Apply (Term a) (Term a), 
    Monad m, Monoid b) =>
  ([(Term a, Term a)] -> Term a -> m b) -> Term a -> m b
foldCaseBranchesM func = 
  execWriterT . mapCaseBranchesM tellAndReturn
  where
  tellAndReturn :: [(Term a, Term a)] -> Term a -> WriterT b m (Term a)
  tellAndReturn matches term = do
    tell =<< lift (func matches term)
    return term
    
instance (Eq (SimpleType a), Typed a, 
  Show (Type (SimpleType a)), Show (Term a)) 
    => Typed (Term a) where
    
  type SimpleType (Term a) = SimpleType a

  typeOf (Var x) = typeOf x
  typeOf (Fix f _) = typeOf f
  typeOf cse@(Cse {}) = typeOf . altTerm . head  .caseOfAlts $ cse
  typeOf (Lam x e) = Type.Fun (typeOf x) (typeOf e)
  typeOf term@(App e1 e2)
    | t2 /= t1a = error $
        "Argument types do not match in " ++ show term
        ++ "\nApplying " ++ show t2 ++ " to " ++ show t1
    | otherwise = t1r
    where
    t1@(Type.Fun t1a t1r) = typeOf e1
    t2 = typeOf e2
  
-- | Instead of defining 'WithinTraversable (Term a) (Term a)',
-- i.e. that we can traverse terms within terms,
-- we define it for 'TermTraversable' things, so we get 
-- 'WithinTraversable' for all these things for free. 
-- 'Term's are themselves 'TermTraversable' 
-- so we still get it for 'Term a'
instance TermTraversable t a => WithinTraversable (Term a) t where
  mapWithinM f = mapTermsM mapWithinT
    where
    mapWithinT (App lhs rhs) =
      f =<< return App `ap` mapWithinT lhs `ap` mapWithinT rhs
    mapWithinT (Cse srt lhs alts) =
      f =<< return (Cse srt) `ap` mapWithinT lhs 
                             `ap` mapM mapWithinA alts
    mapWithinT (Lam var rhs) =
      f =<< return (Lam var) `ap` mapWithinT rhs
    mapWithinT (Fix var rhs) =
      f =<< return (Fix var) `ap` mapWithinT rhs
    mapWithinT expr =
      f =<< return expr
      
    mapWithinA (Alt con vars term) = 
      Alt con vars `liftM` mapWithinT term
      
