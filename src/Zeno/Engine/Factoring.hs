module Zeno.Engine.Factoring (
  value
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Traversing
import Zeno.Unification 
import Zeno.Name ( Name )
import Zeno.Unique ( MonadUnique )
import Zeno.Var ( ZTerm, ZVar )
import Zeno.Type ( typeOf )
import Zeno.Context ( Context (..) )

import qualified Zeno.Substitution as Substitution
import qualified Zeno.Var as Var
import qualified Zeno.Term as Term
import qualified Zeno.Evaluation as Eval
-- import qualified Zeno.Engine.Checker as Checker
import qualified Zeno.Context as Context
import qualified Data.Map as Map

-- | Attempts to factor a value context out of a given term,
-- returns 'mzero' on failure
value :: (MonadUnique m, MonadPlus m) => ZTerm -> m ZTerm
value old_term = do
  -- Try to find a value context to factor out
  mzero
  value_cxt <- undefined 
    -- Checker.guessContext old_term

  -- Declare a new variable for the function we are creating
  new_fix_var <- 
    Var.invent (Context.fillType value_cxt) Var.Universal
  
  -- This term will replace every instance of the old fix var in
  -- the term body. It is the new fix var surrounded by the context
  -- we are factoring out, with appropriate lambdas to apply the
  -- arguments to the new var within the context
  let new_fix_in_context = 
        Term.unflattenLam lam_vars
        $ Context.fill value_cxt 
        $ Term.unflattenApp
        $ map Term.Var
        $ new_fix_var : lam_vars

  replaced <- Substitution.replace 
    (Term.Var old_fix_var) new_fix_in_context old_body
  new_body <- Eval.normalise replaced
   
  -- Attempt to remove the context from every branch of the new body.
  factored_body <- 
    Term.mapCaseBranchesM (Context.matches value_cxt) new_body
  
  -- Since we are at this point the factoring succeeded, so this is the
  -- new function we have created
  let new_fix = Term.Fix new_fix_var 
        $ Term.unflattenLam lam_vars
        $ factored_body
  
  -- Return this new function, with the arguments reapplied 
  -- and within the context
  return
    $ trace ("MEEP:\n" ++ show new_fix_in_context ++ "\napplied to\n" ++ show old_body ++ "\ngives\n" ++ show replaced ++ "\nafter eval\n" ++ show new_body ++ "\nafter factoring\n" ++ show factored_body ++ "\neventually\n" ++ show new_fix)
    $ Context.fill value_cxt
    $ Term.unflattenApp 
    $ (new_fix : args)
  where
  func : args = Term.flattenApp old_term
  Term.Fix old_fix_var old_fix_term = func
  (lam_vars, old_body) = Term.flattenLam old_fix_term
  
  
