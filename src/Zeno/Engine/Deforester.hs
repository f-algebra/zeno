module Zeno.Engine.Deforester  (
  simplify
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Traversing
import Zeno.Unification 
import Zeno.Name ( Name )
import Zeno.Unique ( MonadUnique  )
import Zeno.Var ( ZTerm, ZVar, ZTermMap )
import Zeno.Type ( typeOf )
import Zeno.Term ( TermTraversable (..) )
import Zeno.Context ( Context (..) )
import Zeno.Engine.Simplification ( floatLazyArgsOut )
import Zeno.Core ( Zeno )

import qualified Zeno.Substitution as Substitution
import qualified Zeno.Evaluation as Eval
import qualified Zeno.Type as Type
import qualified Zeno.Term as Term
import qualified Zeno.Var as Var
import qualified Zeno.Context as Context
import qualified Zeno.Show as Show
import qualified Zeno.Engine.Factoring as Factoring

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Test.HUnit as HUnit

simplify :: forall m . MonadUnique m => ZTerm -> m ZTerm
simplify = mapWithinM simp
  where
  simp :: ZTerm -> m ZTerm
  simp term 
    | not (Var.isFunctionCall term) = return term
    | otherwise = do
      term2 <- floatLazyArgsOut term
      term3 <- fromMaybeT (return term2) 
        $ deforest term2
      
      term4 <- fromMaybeT (return term3)
        $ (liftM (traceMe (show term3 ++ "\n==>\n"))) $ Factoring.value term3
      
      return term4
      

deforest :: forall m . (MonadUnique m, MonadPlus m) => 
  ZTerm -> m ZTerm
deforest term = do
  -- The inner term with the 'Term.Fix' of the leftmost function removed
  -- i.e. we have unrolled the function call
  unrolled <- Eval.normalise
    $ Term.unflattenApp (inner_fix_body : inner_args) 
  
  -- We then push the outer context down into every branch 
  cxt_applied <- 
    Term.mapCaseBranchesM (Eval.normalise . Context.fill outer_cxt)
    $ unrolled
  
  -- Create a new function variable for the new function 
  -- we are inventing
  fun_var <- Var.invent fun_type Var.Universal
    
  -- Apply 'deforestBranch' down each branch of the unrolled
  -- innermost function which has had the context pushed inside it, 
  -- hopefully creating a body for our new function.
  -- If you don't get this bit then read the damn paper.
  deforested_body <- 
    Term.mapCaseBranchesM (deforestBranch fun_var)
    $ cxt_applied
  
  -- Create the new fix term for our new function
  -- remembering to reannotate since we have a new fix variable
  new_fix <- Term.reannotate
    $ Term.Fix fun_var
    $ Term.unflattenLam free_vars deforested_body
    
  -- Apply every free variable as an argument to our new function
  let new_term = Term.unflattenApp (new_fix : map Term.Var free_vars)
  
  if new_term `alphaEq` term
    then return term
    else return new_term
  where
  -- The inner_term we will unfold to perform deforestation
  -- and the context that surrounds it
  (outer_cxt, inner_term) = Context.innermost term
  (inner_func : inner_args) = Term.flattenApp inner_term 
  Term.Fix inner_fix_var inner_fix_body = inner_func
  
  free_vars = Set.toList 
    $ concatMap freeVars inner_args
  arg_types = map typeOf free_vars
  
  -- The type of the new function we are creating
  fun_type = Type.unflatten (arg_types ++ [typeOf term])
    
  -- A name for our new function
  name = "[" 
    ++ intercalate " " (map show free_vars) 
    ++ " -> " ++ Show.simple term ++ "]"
    
  -- Applied down every branch of the pattern matches 
  -- in the innermost term.
  -- Fails (returns 'Nothing') if any recursive calls 
  -- to the unrolled innermost term remain.
  deforestBranch :: ZVar -> ZTerm -> m ZTerm
  deforestBranch new_fun_var b_term = do
    -- The new variables to generalise recursive innermost function calls
    gen_vars <- mapM makeGenVar rec_calls
    
    -- Generalise innermost recursive calls
    gen_map <- Substitution.unions 
      $ zipWith Substitution.singleton rec_calls 
      $ map Term.Var gen_vars
    gen_b_term <- Substitution.apply gen_map b_term
        
    -- Simplify this generalised term    
    simp_gen_b_term <- simplify gen_b_term
    
    -- Replace any instances of the original term that have now
    -- been generalised with the new function we are inventing
    replace_gen_vars <- Substitution.unions
      $ zipWith makeReplacement gen_vars new_rec_calls
    new_b_term <- Substitution.apply replace_gen_vars simp_gen_b_term
        
    -- We fail if any recursive innermost calls remain
    -- i.e. if any generalisation variables remain
    guard $ not $ any (flip elem gen_vars) new_b_term
    
    return new_b_term
    where
    -- 'rec_calls' is the set of recursive calls to the unrolled 
    -- innermost function down this branch
    sub_terms = Set.fromList (withinList b_term)
    rec_calls = Set.toList $ Set.filter isRecCall sub_terms
    
    isRecCall term 
      | (Term.Var func : args) <- Term.flattenApp term = 
          func == inner_fix_var
          && length args == length inner_args
      | otherwise = False
    
    -- Creates a new variable to generalise an innermost recursive call
    makeGenVar rec_call =
      assert (typeOf rec_call == typeOf inner_term)
      $ Var.invent (typeOf rec_call) Var.Universal
    
    -- What a recursive call to the inner function must unify with
    -- i.e. it will have structurally smaller arguments
    inner_rec_call = Term.unflattenApp 
      $ (Term.Var inner_fix_var) : inner_args
      
    -- The substitutions which will match the original call to the
    -- innermost function to its recursive call
    unifying_substs :: [ZTermMap]
    unifying_substs =
      map (Map.mapKeysMonotonic Term.Var)
      $ mergeUnifiers 
      $ map (unifier inner_rec_call) rec_calls
    
    -- Recursive calls to the new function we are inventing
    -- unified with recursive calls to the innermost function
    new_rec_calls =
      map (\sub -> substitute sub repl_term) unifying_substs
      where
      -- A recursive call to the new function we are inventing
      repl_term = Term.unflattenApp
        $ map Term.Var (new_fun_var : free_vars)
      
    makeReplacement :: ZVar -> ZTerm -> ZTermMap
    makeReplacement gen_var = 
      Substitution.singleton 
        (Context.fill outer_cxt (Term.Var gen_var))


                               
