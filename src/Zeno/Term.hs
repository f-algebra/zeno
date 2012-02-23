{-# LANGUAGE UndecidableInstances #-}
module Zeno.Term (
  Term (..), Alt (..), CaseSort (..),
  TermSubstitution, IgnoreAnnotations (..),
  TermTraversable (..),
  isVar, fromVar, isApp, isCse, isLam, isFix, isFoldCase,
  flattenApp, unflattenApp, flattenLam, unflattenLam,
  function, isNormal, isFixTerm,
  caseSortFix, reannotate, freshenCaseSort
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Name ( Name, UniqueGen )
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
  | SimpleCase
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
class TermTraversable t where
  mapTermsM :: Monad m => (Term a -> m (Term a)) -> t a -> m (t a)

  mapTerms :: (Term a -> Term a) -> t a -> t a
  mapTerms f = runIdentity . mapTermsM (return . f)   
  
  termList :: t a -> [Term a]
  termList = execWriter . mapTermsM (\t -> tell [t] >> return t)
  
instance TermTraversable Term where
  mapTermsM = ($)
  mapTerms = ($)
  termList = pure

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

freshenCaseSort :: 
    (MonadState g m, UniqueGen g) => CaseSort a -> m (CaseSort a)
freshenCaseSort SimpleCase = return SimpleCase
freshenCaseSort (FoldCase name fix) = do
  new_name <- Name.clone name
  return (FoldCase new_name fix)

isNormal :: forall a . Ord a => Term a -> Bool
isNormal = Set.null . freeFixes
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

function :: Term a -> Maybe a
function (flattenApp -> (Var x : _)) = Just x
function _ = Nothing

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

-- | Resets all the 'CaseSort' annotations within a term.
-- Only run this on top-level terms, if you are within 'Fix'ed variables
-- this could cause inconsistency.
reannotate :: forall g m a t . 
    (MonadState g m, UniqueGen g, Ord a, TermTraversable t) => t a -> m (t a)
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
           then return SimpleCase
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
    
instance Ord a => WithinTraversable (Term a) (Term a) where
  mapWithinM f (App lhs rhs) =
    f =<< return App `ap` mapWithinM f lhs `ap` mapWithinM f rhs
  mapWithinM f (Cse srt lhs alts) =
    f =<< return (Cse srt) `ap` mapWithinM f lhs 
                              `ap` mapM (mapWithinM f) alts
  mapWithinM f (Lam var rhs) =
    f =<< return (Lam var) `ap` mapWithinM f rhs
  mapWithinM f (Fix var rhs) =
    f =<< return (Fix var) `ap` mapWithinM f rhs
  mapWithinM f expr =
    f =<< return expr
    
  substitute sub term = 
    tryReplace sub (subst term)
    where
    subst (Lam var rhs) = 
      Lam var (substitute sub' rhs)
      where sub' = removeVariable var sub
    subst (Fix var rhs) =
      Fix var (substitute sub' rhs)
      where sub' = removeVariable var sub
    subst (App lhs rhs) =
      App (substitute sub lhs) (substitute sub rhs)
    subst (Cse srt term alts) = 
      Cse srt (substitute sub term) (map (substitute sub) alts)
    subst other = other
    
instance Ord a => WithinTraversable (Term a) (Alt a) where
  mapWithinM f (Alt con binds rhs) = 
    return (Alt con binds) `ap` mapWithinM f rhs
    
  substitute sub (Alt con vars term) =
    Alt con vars (substitute sub' term)
    where
    sub' = concatMap (Endo . removeVariable) vars `appEndo` sub
    
instance Empty (CaseSort a) where
  empty = SimpleCase
    
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

