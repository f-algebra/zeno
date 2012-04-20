-- | Beta-reduction
module Zeno.Evaluation (
  normalise, strictTerm, criticalTerm, unrollFix
) where                    

import Prelude ()
import Zeno.Prelude
import Zeno.Var ( ZVar, ZTerm, ZEquation )
import Zeno.Term ( TermTraversable, mapTerms )
import Zeno.Traversing
import Zeno.Show
import Zeno.Utils ( orderedSupersetOf )

import qualified Zeno.Facts as Facts
import qualified Zeno.Logic as Logic
import qualified Zeno.Var as Var
import qualified Zeno.Term as Term

import qualified Data.Set as Set
import qualified Data.Map as Map

data EvalEnv
  = EvalEnv     { unrolledFixes :: Set ZVar
                , backgroundRewrites :: Map ZTerm ZTerm }

type Eval = Reader EvalEnv

addUnrolled :: Maybe ZVar -> Eval a -> Eval a
addUnrolled Nothing = id
addUnrolled (Just var) = local $ \env ->
  env { unrolledFixes = Set.insert var (unrolledFixes env) }

isUnrolled :: ZVar -> Eval Bool
isUnrolled var = asks (Set.member var . unrolledFixes) 

tryRewrite :: ZTerm -> Eval ZTerm
tryRewrite term = do
  rewrites <- asks backgroundRewrites
  return $ fromMaybe term 
         $ Map.lookup term rewrites

normalise :: (TermTraversable t ZVar, Facts.Reader m) => t -> m t
normalise terms = do
  facts <- Facts.ask
  return $ evaluate facts terms

evaluate :: TermTraversable t ZVar => [ZEquation] -> t -> t
evaluate eqs = mapTerms (flip runReader startingEnv . eval)
  where
  startingEnv = EvalEnv mempty (Map.fromList consEqs)
  eq_pairs = map Logic.toPair eqs
  consEqs = mapMaybe consEq eq_pairs
  
  consEq (t1, t2)
    | Var.isConstructorTerm t2 = Just (t1, t2)
    | Var.isConstructorTerm t1 = Just (t2, t1)
    | otherwise = Nothing

criticalTerm :: forall m . Facts.Reader m => ZTerm -> m ZTerm 
criticalTerm = crit <=< normalise
  where
  crit :: ZTerm -> m ZTerm
  crit (Term.Cse (Term.FoldCase {}) term _) = crit term
  crit (Term.Cse Term.SplitCase term _) = return term
  crit term = do
    mby_unrolled <- unrollFix term
    maybe (return term) crit mby_unrolled

strictTerm :: forall m . Facts.Reader m => ZTerm -> m ZTerm
strictTerm = strict <=< normalise
  where
  strict :: ZTerm -> m ZTerm
  strict (Term.Cse _ term _) = strict term
  strict term = do
    mby_unrolled <- unrollFix term
    maybe (return term) strict mby_unrolled

unrollFix :: Facts.Reader m => ZTerm -> m (Maybe ZTerm)
unrollFix (Term.flattenApp -> fix_term@(Term.Fix fix_var fix_rhs) : args) =
  liftM Just $ normalise $ Term.unflattenApp (unrolled_fix : args)
  where
  unrolled_fix = replaceWithin (Term.Var fix_var) fix_term fix_rhs
unrollFix _ = return Nothing

eval :: ZTerm -> Eval ZTerm
eval (Term.Var x) = return (Term.Var x)
eval (Term.Lam x t) = Term.Lam x <$> eval t
eval (Term.Fix f t) = Term.Fix f <$> return t
eval (Term.Cse cse_srt cse_of cse_alts) =
  addUnrolled (Term.caseSortFix cse_srt) $ do
    cse_of' <- (eval <=< tryRewrite) cse_of
    cse_alts' <- mapM evalAlt cse_alts
    if not (Var.isConstructorTerm cse_of')
    then return (Term.Cse cse_srt cse_of' cse_alts')
    else eval (matchAlt cse_of' cse_alts')
  where
  evalAlt alt = do
    term' <- eval (Term.altTerm alt)
    return $ alt { Term.altTerm = term' }
  
  matchAlt term alts =
    substitute sub . Term.altTerm $ match
    where
    (Term.Var con : term_args) = Term.flattenApp term
    Just match = case find ((== con) . Term.altCon) alts of
      Nothing -> error $ "Failed match: " ++ show term ++ " with " ++ show alts
      Just match -> Just match
    bound_vars = map Term.Var . Term.altVars $ match
    sub = Map.fromList $ bound_vars `zip` term_args 
    
eval other = do
  flattened <- mapM eval (Term.flattenApp other)
  evalApp flattened
  where
  evalApp :: [ZTerm] -> Eval ZTerm
  evalApp (Term.Lam lam_var lam_rhs : arg : rest) = do
    lam_rhs' <- eval $ replaceWithin (Term.Var lam_var) arg lam_rhs
    evalApp (lam_rhs' : rest)
  evalApp app@(fix_term@(Term.Fix fix_var fix_rhs) : args) = do
    already_unrolled <- isUnrolled fix_var
    let did_nothing = return (Term.unflattenApp app)
    if already_unrolled
    then did_nothing
    else do
      let unrolled_fix = replaceWithin (Term.Var fix_var) fix_term fix_rhs
      unrolled <- evalApp (unrolled_fix : args)
      if not (Term.isCaseNormal unrolled)
      then did_nothing
      else eval unrolled
  evalApp app = 
    return (Term.unflattenApp app)

