module Zeno.Engine.Factoring (
  value
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

-- | Attempts to factor a value context out of a given term
value :: (MonadUnique m, MonadFailure m) => ZTerm -> m ZTerm
value old_term = do
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
    
  