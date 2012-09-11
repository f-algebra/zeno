-- | Beta-reduction
module Zeno.Evaluation (
  normalise, unrollFix
) where                    

import Prelude ()
import Zeno.Prelude
import Zeno.Var ( ZVar, ZTerm, ZEquation )
import Zeno.Term ( TermTraversable, mapTermsM )
import Zeno.Traversing
import Zeno.Show ()

import qualified Control.Unique as Unique
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

-- | Beta-reduces a term. Needs 'MonadUnique' as it does substitution
-- and hence might have to create fresh names.
normalise :: (MonadUnique m, TermTraversable t ZVar) => t -> m t
normalise = Unique.abstractGen 
  . mapTermsM (flip runReaderT empty . eval . clearAllTags)
  
-- | Takes a term which is a function call and unrolls the body
-- of the function once. Returns the original term if it 
-- is not a function call.
unrollFix :: MonadUnique m => ZTerm -> m ZTerm
unrollFix term 
  | fix_term@(Term.Fix fix_var fix_body) : args 
      <- Term.flattenApp term = do
    fix_body' <- 
      Substitution.replace (Term.Var fix_var) fix_term 
      $ fix_body
    normalise 
      $ Term.unflattenApp (fix_body' : args)
unrollFix term = 
  return term

clearAllTags :: ZTerm -> ZTerm
clearAllTags = mapWithin clear
  where
  clear (Term.Cse _ term alts) = 
    Term.Cse empty term alts
  clear other = other
  
addUnrolled :: ZVar -> Eval a -> Eval a
addUnrolled var = local $ \env ->
  env { unrolledFixes = Set.insert var (unrolledFixes env) }

isUnrolled :: ZVar -> Eval Bool
isUnrolled var = asks (Set.member var . unrolledFixes) 

eval :: ZTerm -> Eval ZTerm
eval (Term.Var x) = return (Term.Var x)
eval (Term.Lam x t) = Term.Lam x <$> eval t
eval (Term.Fix f t) = Term.Fix f <$> eval t
eval (Term.Cse cse_tag cse_of cse_alts) =
  addUnrolled cse_tag $ do
    cse_of' <- eval cse_of
    cse_alts' <- mapM evalAlt cse_alts
    if not (Var.isConstructorTerm cse_of')
    then return (Term.Cse cse_tag cse_of' cse_alts')
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
      let tagged_fix = Term.setFreeTags fix_var unrolled_fix
      unrolled <- evalApp (tagged_fix : args)
      if fix_var `Set.member` Term.freeCaseTags unrolled
      then did_nothing
      else eval unrolled
  evalApp app = 
    return (Term.unflattenApp app)
    

