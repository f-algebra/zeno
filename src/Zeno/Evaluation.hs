-- | Beta-reduction
module Zeno.Evaluation (
  normalise
) where                    

import Prelude ()
import Zeno.Prelude
import Zeno.Var ( ZVar, ZTerm, ZEquation )
import Zeno.Term ( TermTraversable, mapTermsM )
import Zeno.Traversing
import Zeno.Show ()
import Zeno.Utils ( orderedSupersetOf )
import Zeno.Unique ( MonadUnique )

import qualified Zeno.Unique as Unique
import qualified Zeno.Logic as Logic
import qualified Zeno.Var as Var
import qualified Zeno.Term as Term
import qualified Zeno.Substitution as Substitution

import qualified Data.Set as Set
import qualified Data.Map as Map

data EvalEnv
  = EvalEnv     { unrolledFixes :: Set ZVar }
  
instance Empty EvalEnv where
  empty = EvalEnv mempty

type Eval = ReaderT EvalEnv Unique.Gen

normalise :: (MonadUnique m, TermTraversable t ZVar) => t -> m t
normalise = Unique.abstractGen
  . mapTermsM (flip runReaderT empty . eval)

addUnrolled :: Maybe ZVar -> Eval a -> Eval a
addUnrolled Nothing = id
addUnrolled (Just var) = local $ \env ->
  env { unrolledFixes = Set.insert var (unrolledFixes env) }

isUnrolled :: ZVar -> Eval Bool
isUnrolled var = asks (Set.member var . unrolledFixes) 

eval :: ZTerm -> Eval ZTerm
eval (Term.Var x) = return (Term.Var x)
eval (Term.Lam x t) = Term.Lam x <$> eval t
eval (Term.Fix f t) = Term.Fix f <$> eval t
eval (Term.Cse cse_srt cse_of cse_alts) =
  addUnrolled (Term.caseSortFix cse_srt) $ do
    cse_of' <- eval cse_of
    cse_alts' <- mapM evalAlt cse_alts
    if not (Var.isConstructorTerm cse_of')
    then return (Term.Cse cse_srt cse_of' cse_alts')
    else do
      matched_alt <- matchAlt cse_of' cse_alts'
      eval matched_alt
  where
  evalAlt alt = do
    term' <- eval (Term.altTerm alt)
    return $ alt { Term.altTerm = term' }
  
  matchAlt term alts =
    Substitution.apply var_binding 
    $ Term.altTerm match
    where
    (Term.Var con : term_args) = Term.flattenApp term
    Just match = case find ((== con) . Term.altCon) alts of
      Nothing -> error 
        $ "Failed match: " ++ show term ++ " with " ++ show alts
      Just match -> Just match
    bound_vars = map Term.Var . Term.altVars $ match
    var_binding = Substitution.fromList 
      $ bound_vars `zip` term_args 
    
eval other = do
  flattened <- mapM eval (Term.flattenApp other)
  evalApp flattened
  where
  evalApp :: [ZTerm] -> Eval ZTerm
  evalApp (Term.Lam lam_var lam_rhs : arg : rest) = do
    lam_rhs' <- Substitution.replace (Term.Var lam_var) arg lam_rhs
    lam_rhs'' <- eval lam_rhs'
    evalApp (lam_rhs'' : rest)
  evalApp app@(fix_term@(Term.Fix fix_var fix_rhs) : args) = do
    already_unrolled <- isUnrolled fix_var
    let did_nothing = return (Term.unflattenApp app)
    if already_unrolled
    then did_nothing
    else do
      unrolled_fix <- Substitution.replace
        (Term.Var fix_var) fix_term fix_rhs
      unrolled <- evalApp (unrolled_fix : args)
      if not (Term.isCaseNormal unrolled)
      then did_nothing
      else eval unrolled
  evalApp app = 
    return (Term.unflattenApp app)
    

