-- | Beta-reduction
module Zeno.Evaluation (
  normalise, strictTerm, evaluate
) where                    

import Prelude ()
import Zeno.Prelude
import Zeno.ReaderWriter
import Zeno.Var ( ZVar, ZTerm, ZEquation )
import Zeno.Term ( TermTraversable, mapTerms )
import Zeno.Traversing
import Zeno.Show
import Zeno.Utils ( orderedSupersetOf )

import qualified Zeno.Logic as Logic
import qualified Zeno.Var as Var
import qualified Zeno.Term as Term

import qualified Data.Set as Set
import qualified Data.Map as Map

data EvalEnv
  = EvalEnv     { unrolledFixes :: Set ZVar
                , backgroundRewrites :: Map ZTerm ZTerm }

type Eval = Reader EvalEnv

unroll :: Maybe ZVar -> Eval a -> Eval a
unroll Nothing = id
unroll (Just var) = local $ \env ->
  env { unrolledFixes = Set.insert var (unrolledFixes env) }

isUnrolled :: ZVar -> Eval Bool
isUnrolled var = asks (Set.member var . unrolledFixes) 

tryRewrite :: ZTerm -> Eval ZTerm
tryRewrite term = do
  rewrites <- asks backgroundRewrites
  return $ fromMaybe term 
         $ Map.lookup term rewrites
   
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
      
normalise :: TermTraversable t ZVar => t -> t
normalise = evaluate []

strictTerm :: ZTerm -> ZTerm
strictTerm = strict . normalise
  where
  strict :: ZTerm -> ZTerm
  strict term@(Term.flattenApp -> fix_term@(Term.Fix fix_var fix_rhs) : args) =
    strict unrolled
    where
    unrolled_fix = replaceWithin (Term.Var fix_var) fix_term fix_rhs
    unrolled = normalise $ Term.unflattenApp (unrolled_fix : args)
  strict (Term.Cse _ term _) = strict term
  strict other = other

{-
criticalPair :: ZTerm -> Maybe CriticalPair
criticalPair term 
  | valid = Just cpair
  | otherwise = Nothing
  where
  cpair@(cterm, cpath) = runWriter (critical term)
  
  valid = not (null cpath)
       && Var.destructible cterm
       && all (not . orderedSupersetOf cpath) (Var.allSources cterm)
  
  critical :: ZTerm -> Writer CriticalPath ZTerm
  critical (Term.flattenApp -> fix_term@(Term.Fix fix_var fix_rhs) : args) =
    critical unrolled
    where
    unrolled_fix = replaceWithin (Term.Var fix_var) fix_term fix_rhs
    unrolled = normalise $ Term.unflattenApp (unrolled_fix : args)
  critical (Term.Cse Term.SplitCase cse_term _) =
    return cse_term
  critical (Term.Cse (Term.FoldCase name _) cse_term _) = do
    tell [name]
    critical cse_term
  critical term = 
    return term
  -}

eval :: ZTerm -> Eval ZTerm
eval (Term.Var x) = return (Term.Var x)
eval (Term.Lam x t) = Term.Lam x <$> eval t
eval (Term.Fix f t) = Term.Fix f <$> return t
eval (Term.Cse cse_srt cse_of cse_alts) =
  unroll (Term.caseSortFix cse_srt) $ do
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
      if not (Term.isNormal unrolled)
      then did_nothing
      else eval unrolled
  evalApp app = 
    return (Term.unflattenApp app)

