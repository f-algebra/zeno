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

import qualified Zeno.Var as Var
import qualified Zeno.Term as Term
import qualified Zeno.Evaluation as Eval
import qualified Zeno.Context as Context
import qualified Data.Map as Map

-- | Factors a given value context out of a given term
value :: (MonadUnique m, MonadPlus m) => Context -> ZTerm -> m ZTerm
value value_cxt old_term 
  | not (Term.isFix func) = error (show old_term)
  | otherwise = do
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
        
  let replacement_sub = Map.singleton 
        (Term.Var old_fix_var) new_fix_in_context
      new_body = Eval.evaluate []
        $ substitute replacement_sub old_body
   
  -- Attempt to remove the context from every branch of the new body.
  -- Lifted to 'MonadPlus' so if it fails down any branch
  -- the whole function returns 'mzero'
  factored_body <- liftMaybe
    $ Term.mapCaseBranchesM (Context.matches value_cxt) new_body
  
  -- Since we are at this point the factoring succeeded, so this is the
  -- new function we have created
  let new_fix = Term.Fix new_fix_var 
        $ Term.unflattenLam lam_vars
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
  (lam_vars, old_body) = Term.flattenLam old_fix_term
  
  
