module Zeno.Engine.Factoring (
  value, pattern
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Traversing
import Zeno.Unification 
import Zeno.Name ( Name )
import Zeno.Var ( ZTerm, ZVar )
import Zeno.Type ( typeOf )
import Zeno.Context ( Context (..) )

import qualified Zeno.Substitution as Substitution
import qualified Zeno.Var as Var
import qualified Zeno.Term as Term
import qualified Zeno.Evaluation as Eval
import qualified Zeno.Engine.Checker as Checker
import qualified Zeno.Context as Context

import qualified Control.Failure as Fail
import qualified Data.Map as Map
import qualified Data.Set as Set

isDeforestedTerm :: ZTerm -> Bool
isDeforestedTerm term
  | func : args <- Term.flattenApp term =
      Term.isFix func && all Term.isVar args
isDeforestedTerm _ = False
      
-- | Attempts to factor a value context out of a given term
value :: (MonadUnique m, MonadFailure m) => ZTerm -> m ZTerm
value old_term = 
    assert (isDeforestedTerm old_term) $ do
  
  Fail.unless (all Term.isVar args)
    
  -- Try to find a value context to factor out
  value_cxt <- Checker.guessContext old_term

  unrolled_body <- Eval.normalise
    $ Term.unflattenApp (old_fix_term : args)
  
  -- Declare a new variable for the function we are creating
  new_fix_var <- 
    Var.invent (Context.fillType value_cxt) Var.Universal
  
  -- This term will replace every instance of the old fix var in
  -- the unrolled term body. It is the new fix var surrounded by the context
  -- we are factoring out, with appropriate lambdas to apply the
  -- arguments to the new var within the context. We just re-use
  -- the lambda variables from the outer term for ease.
  let new_fix_in_context = 
        Term.unflattenLam arg_vars
        $ Context.fill value_cxt 
        $ Term.unflattenApp
        $ map Term.Var
        $ new_fix_var : arg_vars

  replaced <- Substitution.replace 
    (Term.Var old_fix_var) new_fix_in_context unrolled_body
  new_body <- Eval.normalise replaced
  
  -- Attempt to remove the context from every branch of the new body.
  factored_body <- 
    Term.mapCaseBranchesM (factorBranch value_cxt) new_body
  
  -- Since we are at this point the factoring succeeded, so this is the
  -- new function we have created
  let new_fix = Term.Fix new_fix_var 
        $ Term.unflattenLam arg_vars
        $ factored_body
  
  -- Return this new function, with the arguments reapplied 
  -- and within the context
  return
    $ Context.fill value_cxt
    $ Term.unflattenApp 
    $ (new_fix : args)
  where
  func : args = Term.flattenApp old_term
  Term.Fix old_fix_var old_fix_term = func
  arg_vars = map Term.fromVar args
  
  -- | Attempt to factor a given context out of a given term, under a set
  -- of term bindings
  factorBranch :: (MonadUnique m, MonadFailure m) => 
    Context -> [(ZTerm, ZTerm)] -> ZTerm -> m ZTerm
  factorBranch value_cxt bindings term = do
    term' <- Substitution.applyList binding_maps term
    value_cxt' <- Substitution.applyList binding_maps value_cxt
    Context.matches value_cxt' term'
    where
    var_bindings = filter (Term.isVar . fst) bindings
    binding_maps = map (uncurry Substitution.singleton) var_bindings
    
    
-- | Factor any free-patterns out of a term
pattern :: MonadUnique m => ZTerm -> m ZTerm
pattern old_term =
    assert (isDeforestedTerm old_term) $ do
    
  -- Put every free pattern in the term at the top level of the term
  patterns_applied <- foldrM applyPattern old_term free_patterns
  
  -- Evaluating this will hopefully remove all these free patterns from
  -- within the fix of the term, leaving them solely on the top level 
  Eval.normalise patterns_applied
  where
  func : args = Term.flattenApp old_term
  Term.Fix old_fix_var old_fix_term = func
  
  free_patterns = filter isFreePattern 
    $ withinList old_fix_term
  
  -- | A free pattern is a case-of on a term which only contains free 
  -- variables which are also free within the original term.
  -- It is a pattern which can be evaluated outside of the recursive function. 
  isFreePattern :: ZTerm -> Bool
  isFreePattern (Term.Cse _ cse_of _) = 
    Term.isApp cse_of 
    && freeVars cse_of `Set.isSubsetOf` freeVars old_term
  isFreePattern _ = False
    
  -- | Places a given term down every branch of a given pattern,
  -- applying the rewrite of that pattern binding down each such branch.
  applyPattern cse_term term = 
      assert (Term.isCse cse_term) $ do 
    cse_alts' <- mapM applyAlt (Term.caseOfAlts cse_term)
    return 
      $ cse_term { Term.caseOfAlts = cse_alts' }
    where
    replaceCaseTermWith = 
      Substitution.replace (Term.caseOfTerm cse_term)
    
    applyAlt alt = do
      term' <- replaceCaseTermWith (Term.altPattern alt) term
      return
        $ alt { Term.altTerm = term' }
