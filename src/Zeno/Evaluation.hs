-- | Beta-reduction
module Zeno.Evaluation (
  normalise, strictTerm, criticalPair
) where                    

import Prelude ()
import Zeno.Prelude
import Zeno.ReaderWriter
import Zeno.Var ( ZVar, ZTerm, CriticalPath, CriticalPair )
import Zeno.Term ( TermTraversable, mapTerms )
import Zeno.Traversing
import Zeno.Show
import Zeno.Utils ( orderedSupersetOf )

import qualified Zeno.Logic as Logic
import qualified Zeno.Var as Var
import qualified Zeno.Term as Term

import qualified Data.Set as Set
import qualified Data.Map as Map

type Eval = Reader (Set ZVar)

fixed :: Maybe ZVar -> Eval a -> Eval a
fixed Nothing = id
fixed (Just var) = local (Set.insert var)

normalise :: TermTraversable t => t ZVar -> t ZVar
normalise = mapTerms (flip runReader mempty . eval)

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
  critical (Term.Cse Term.SimpleCase cse_term _) =
    return cse_term
  critical (Term.Cse (Term.FoldCase name _) cse_term _) = do
    tell [name]
    critical cse_term
  critical term = 
    return term
  
{-
criticalP :: ZTerm -> WriterT CriticalPath Eval ZTerm 
criticalP term
  | fix_term@(Term.Fix fix_var fix_rhs) : args <- Term.flattenApp term = do
    already_unrolled <- ask
    if fix_var `Set.member` already_unrolled
    then return term
    else do
      let unrolled_fix = replaceWithin (Term.Var fix_var) fix_term fix_rhs
      unrolled <- lift $ eval $ Term.unflattenApp (unrolled_fix : args)
      criticalP unrolled
  
criticalP other = 
  return other
-}

eval :: ZTerm -> Eval ZTerm
eval (Term.Var x) = return (Term.Var x)
eval (Term.Lam x t) = Term.Lam x <$> eval t
eval (Term.Fix f t) = Term.Fix f <$> return t
eval (Term.Cse cse_srt cse_of cse_alts) =
  fixed (Term.caseSortFix cse_srt) $ do
    cse_of' <- eval cse_of
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
    already_unrolled <- ask
    let did_nothing = return (Term.unflattenApp app)
    if fix_var `Set.member` already_unrolled
    then did_nothing
    else do
      let unrolled_fix = replaceWithin (Term.Var fix_var) fix_term fix_rhs
      unrolled <- evalApp (unrolled_fix : args)
      if not (Term.isNormal unrolled)
      then did_nothing
      else eval unrolled
  evalApp app = 
    return (Term.unflattenApp app)

