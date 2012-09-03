{-# LANGUAGE UndecidableInstances #-}
module Zeno.Term (
  Term (..), Alt (..), CaseSort (..),
  TermSubstitution, IgnoreAnnotations (..),
  TermTraversable (..),
  isVar, fromVar, isApp, isCse, isLam, isFix, isFoldCase,
  flattenApp, unflattenApp, flattenLam, unflattenLam,
  function, isNormal, isCaseNormal, isFixTerm, 
  caseSortFix, reannotate, freshenCaseSort, 
  mapCaseBranches, mapCaseBranchesM, 
  stripLambdas, etaReduce,
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Name ( Name )
import Zeno.Unique ( MonadUnique )
import Zeno.Traversing
import Zeno.Utils
import Zeno.Type ( Type, Typed (..) )
import Zeno.Unification

import qualified Zeno.Type as Type
import qualified Zeno.Name as Name
import qualified Data.Map as Map
import qualified Data.Set as Set

data Term a 
  = Var !a 
  | App !(Term a) !(Term a)
  | Lam !a !(Term a)
  | Fix !a !(Term a)
  | Cse     { caseOfSort :: CaseSort a,
              caseOfTerm :: !(Term a),
              caseOfAlts :: ![Alt a] }
  deriving ( Eq, Ord, Functor, Foldable, Traversable )
  
data Alt a
  = Alt     { altCon :: !a,
              altVars :: ![a],
              altTerm :: !(Term a) }
  deriving ( Eq, Ord, Functor, Foldable, Traversable )
  
data CaseSort a
  = FoldCase !Name !a
  | SplitCase
  deriving ( Eq, Ord, Functor, Foldable, Traversable )
  
type TermSubstitution a = Substitution (Term a) (Term a)

instance Empty a => Empty (Term a) where
  empty = Var empty

instance HasVariables (Term a) where
  type Var (Term a) = a
  
  freeVars (App e1 e2) = freeVars e1 ++ freeVars e2
  freeVars (Var x) = Set.singleton x
  freeVars (Lam x e) = Set.delete x (freeVars e) 
  freeVars (Fix f e) = Set.delete f (freeVars e)
  freeVars cse@(Cse {}) =
    freeVars (caseOfTerm cse) ++ altVars
    where
    altVars = concatMap freeVars (caseOfAlts cse)
  
instance HasVariables (Alt a) where
  type Var (Alt a) = a
  
  freeVars (Alt _ vars e) = 
    Set.difference (freeVars e) (Set.fromList vars)
    
-- | Represents traversing over top-level 'Term's only, 
-- does not recurse into sub-terms.
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

isFoldCase :: CaseSort a -> Bool
isFoldCase (FoldCase {}) = True
isFoldCase _ = False

caseSortFix :: CaseSort a -> Maybe a
caseSortFix (FoldCase _ fix) = Just fix
caseSortFix _ = Nothing

freshenCaseSort :: MonadUnique m => CaseSort a -> m (CaseSort a)
freshenCaseSort SplitCase = return SplitCase
freshenCaseSort (FoldCase name fix) = do
  new_name <- Name.clone name
  return (FoldCase new_name fix)

isNormal :: Ord a => Term a -> Bool
isNormal = anyWithin isFix

isCaseNormal :: forall a . Ord a => Term a -> Bool
isCaseNormal = Set.null . freeFixes
  where
  freeFixes :: Term a -> Set a
  freeFixes (Var _) = mempty
  freeFixes (App t1 t2) = freeFixes t1 `mappend` freeFixes t2
  freeFixes (Lam _ t) = freeFixes t
  freeFixes (Fix fix t) = Set.delete fix (freeFixes t)
  freeFixes (Cse srt term alts) 
    | FoldCase _ fix <- srt = Set.insert fix inner
    | otherwise = inner
    where
    inner = concatMap freeFixes (term : map altTerm alts)
    
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

etaReduce :: Eq a => Term a -> Term a
etaReduce (Lam x (App f y))
  | (Var x) == y = etaReduce f
etaReduce other = other

mapCaseBranchesM :: Monad m => 
  (Term a -> m (Term a)) -> Term a -> m (Term a)
mapCaseBranchesM f (Cse srt cse_of alts) =
  Cse srt cse_of `liftM` mapM mapAltM alts
  where
  mapAltM (Alt con vars term) = 
    Alt con vars `liftM` mapCaseBranchesM f term
mapCaseBranchesM f other = f other

mapCaseBranches :: (Term a -> Term a) -> Term a -> Term a
mapCaseBranches f = runIdentity . mapCaseBranchesM (Identity . f)
    
-- | Resets all the 'CaseSort' annotations within a term.
-- Only run this on top-level terms, if you are within 'Fix'ed variables
-- this could cause inconsistency.
reannotate :: forall m a t . 
  (MonadUnique m, Ord a, TermTraversable t a) => t -> m t
reannotate = mapTermsM (flip runReaderT (Nothing, mempty) . set)
  where
  set :: Term a -> ReaderT (Maybe a, Set a) m (Term a)
  set (Fix x t) = local (const (Just x, mempty)) $ Fix x `liftM` set t
  set (Lam x t) = local (second (Set.insert x)) $ Lam x `liftM` set t
  set (App t1 t2) = App `liftM` set t1 `ap` set t2
  set (Cse _ term alts) = do
    (mby_fix, free_vars) <- ask
    let fold_split = isVar term 
          && fromVar term `Set.member` free_vars
          && isJust mby_fix
    srt <- if not fold_split
           then return SplitCase
           else do
             new_name <- Name.invent
             return $ FoldCase new_name (fromJust mby_fix)
    term' <- set term
    alts' <- mapM (setAlt fold_split) alts
    return $ Cse srt term' alts'
  set other = return other
  
  setAlt fold_split (Alt con vars term) = do
    term' <- if fold_split 
             then addVars (set term)
             else set term
    return $ Alt con vars term'
    where
    var_set = Set.fromList vars
    addVars = local $ second $ Set.union var_set

-- | A 'Foldable' instance which does not include annotations
newtype IgnoreAnnotations a 
  = IgnoreAnnotations { includeAnnotations :: Term a }

instance Foldable IgnoreAnnotations where
  foldMap f = go . includeAnnotations
    where
    go (Var x) = f x
    go (App x y) = go x ++ go y
    go (Lam x t) = f x ++ go t
    go (Fix x t) = f x ++ go t
    go (Cse _ t as) = go t ++ foldMap (go . altTerm) as

instance Ord a => Unifiable (Term a) where
  type UniTerm (Term a) = Term a
  type UniVar (Term a) = a

  unifier (Var v1) (Var v2)
    | v1 == v2 = mempty
  unifier (App f1 a1) (App f2 a2) =
    unifier f1 f2 `mappend` unifier a1 a2
  unifier (Lam v1 x1) (Lam v2 x2) = 
    unifier x1 (replaceWithin (Var v2) (Var v1) x2)
  unifier (Fix v1 x1) (Fix v2 x2) =
    unifier x1 (replaceWithin (Var v2) (Var v1) x2)
  unifier (Cse _ t1 as1) (Cse _ t2 as2)
    | length as1 /= length as2 = NoUnifier
    | otherwise = unifier t1 t2 `mappend` alts_uni
    where
    as1s = sortWith altCon as1
    as2s = sortWith altCon as2
    alts_uni = mconcat $ zipWith unifier as1s as2s
  unifier x1 x2
    | x1 == x2 = mempty
  unifier (Var x) expr =
    Unifier (Map.singleton x expr)
  unifier _ _ =
    NoUnifier
  
  applyUnifier sub =
    substitute (Map.mapKeysMonotonic Var sub)
    
instance Ord a => Unifiable (Alt a) where
  type UniTerm (Alt a) = Term a
  type UniVar (Alt a) = a

  unifier (Alt k1 vs1 t1) (Alt k2 vs2 t2)
    | k1 /= k2 = NoUnifier
    | otherwise = unifier t1 t2' 
    where
    t2' = substitute (Map.fromList $ map (Var *** Var) $ zip vs2 vs1) t2
    
  applyUnifier sub =
    substitute (Map.mapKeysMonotonic Var sub)
    
instance (Ord a, TermTraversable t a) => 
    WithinTraversable (Term a) t where
    
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
    
  substitute s = mapTerms (substituteT s)
    where
    substituteT sub term = 
      tryReplace sub (subst term)
      where
      subst (Lam var rhs) = 
        Lam var (substituteT sub' rhs)
        where sub' = removeVariable var sub
      subst (Fix var rhs) =
        Fix var (substituteT sub' rhs)
        where sub' = removeVariable var sub
      subst (App lhs rhs) =
        App (substituteT sub lhs) (substituteT sub rhs)
      subst (Cse srt term alts) = 
        Cse srt (substituteT sub term) (map (substituteA sub) alts)
      subst other = other
      
    substituteA sub (Alt con vars term) =
      Alt con vars (substituteT sub' term)
      where
      sub' = concatMap (Endo . removeVariable) vars `appEndo` sub
      
    
instance Empty (CaseSort a) where
  empty = SplitCase
    
instance (Eq (SimpleType a), Typed a, Show (Type (SimpleType a)), Show (Term a)) 
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
    
removeVariable :: Ord a => a -> TermSubstitution a -> TermSubstitution a
removeVariable var = Map.filterWithKey $ \k a ->
  not $ Set.member var $ freeVars k ++ freeVars a
  
isOperator :: String -> Bool
isOperator | error "find where isOperator should go" = any (not . isNormalChar)
  where
  isNormalChar :: Char -> Bool
  isNormalChar '_' = True
 -- isNormalChar '$' = True
  isNormalChar '.' = True
  isNormalChar c = isAlphaNum c

