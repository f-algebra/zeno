module Zeno.Engine.Simplification  (
  floatLazyArgsOut
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Traversing
import Zeno.Unification 
import Zeno.Name ( Name )
import Zeno.Var ( ZTerm, ZVar )
import Zeno.Type ( typeOf )
import Zeno.Context ( Context (..) )

import qualified Zeno.Evaluation as Eval
import qualified Control.Unique as Unique
import qualified Zeno.Type as Type
import qualified Zeno.Term as Term
import qualified Zeno.Var as Var
import qualified Zeno.Context as Context
import qualified Zeno.Testing as Test
import qualified Data.Map as Map
import qualified Data.Set as Set
  
-- | Takes a function (some 'Term.Fix') and floats any arguments which 
-- are not recursed upon outside the fix.
floatLazyArgsOut :: MonadUnique m => ZTerm -> m ZTerm
floatLazyArgsOut = 
  Unique.abstractGen . (Eval.normalise <=< mapWithinM floatLazy)

-- The body of 'floatLazyArgsOut', which is mapped over every sub-term
floatLazy :: ZTerm -> Unique.Gen ZTerm
floatLazy orig_term@(Term.Fix old_fix_var old_fix_body) = do
  -- Define a new fixed variable
  -- with the new "smaller" type (less arguments)
  new_fix_var <- 
    Var.declare (show old_fix_var) new_fix_type Var.Universal
    
  -- Create the new function body by replacing recursive calls
  -- with the new fix variable, and removing lazy arguments
  let new_body = mapWithin (replaceFunctionCalls new_fix_var) raw_body
  
  -- Using this new body we can construct the new 'Fix'
  -- we also reannotate the case-splits since we have a new fix var
  let new_fix = Term.Fix new_fix_var  
        $ Term.unflattenLam strict_vars new_body
        
  -- The resulting term needs to have the same arguments as the
  -- original, so we create a set of outer lambdas and only pass
  -- the strict arguments to the inner 'Fix'
  let new_term = Term.etaReduce 
        $ Term.unflattenLam all_vars
        $ Term.unflattenApp 
        $ new_fix : (map Term.Var strict_vars)
        
  -- Whether the original function definition is equivalent to the new one
  let nothing_changed = new_term `alphaEq` orig_term
        
  -- If there were no lazy variables we assert that the function 
  -- definition does not change
  let sanityCheck = not (null lazy_var_ixs) || nothing_changed
  
  return
    $ assert sanityCheck
    $ if nothing_changed then orig_term else new_term 
  where
  (all_vars, raw_body) = Term.flattenLam old_fix_body
  
  -- Find all of the lazy arguments, the ones that will be
  -- floated out of the fix
  lazy_var_ixs = findIndices (not . isStrictArg) all_vars
  strict_vars = deleteIndices lazy_var_ixs all_vars
  
  -- Calculate the type of the new fixed function,
  -- with the same return type but only taking the strict arguments
  return_type = Type.returns (typeOf old_fix_var)
  arg_types = map typeOf strict_vars
  new_fix_type = Type.unflatten $ arg_types ++ [return_type]

  -- A strict argument is pattern matched at some point
  -- TODO: this definition could probably be improved
  -- as the argument also needs to decrease in recursive calls
  isStrictArg :: ZVar -> Bool
  isStrictArg arg_var = anyWithin matchesVar raw_body 
    where
    matchesVar (Term.Cse _ cse_of _) = cse_of == (Term.Var arg_var)
    matchesVar _ = False
    
  -- Any call to the function we are floating variables out of
  -- should have the non-strict arguments removed
  replaceFunctionCalls :: ZVar -> ZTerm -> ZTerm
  replaceFunctionCalls new_fix_var term
    | Term.isVar func
    , this_fix_var == old_fix_var 
    , length args == length all_vars = 
        assert checkLazyArgsUnchanged
        $ assert (length new_args == length strict_vars)
        $ new_term
    where
    func : args = Term.flattenApp term
    Term.Var this_fix_var = func
    
    -- 'new_term' has the lazy arguments removed from the function call
    new_args = deleteIndices lazy_var_ixs args
    new_term = Term.unflattenApp (Term.Var new_fix_var : new_args)
    
    -- Check that all of the lazy arguments are unchanged
    -- in the recursive function calls
    checkLazyArgsUnchanged = 
      all (\i -> (args !! i) == Term.Var (all_vars !! i)) lazy_var_ixs  
      
  replaceFunctionCalls _ other = other
    
floatLazy other = 
  return other

