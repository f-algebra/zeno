module Zeno.Simplifier  (
  simplify
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Traversing
import Zeno.Unification
import Zeno.Name ( Name )
import Zeno.Unique ( MonadUnique, Unique )
import Zeno.Var ( ZTerm, ZClause, ZDataType, ZType, ZAlt,
                  ZVar, ZTermSubstitution, ZEquation )
import Zeno.Type ( typeOf )
import Zeno.Term ( TermTraversable (..) )
import Zeno.Utils ( orderedSupersetOf )
import Zeno.Context ( Context (..) )

import qualified Zeno.Evaluation as Eval
import qualified Zeno.DataType as DataType
import qualified Zeno.Type as Type
import qualified Zeno.Term as Term
import qualified Zeno.Var as Var
import qualified Zeno.Unique as Unique
import qualified Zeno.Context as Context
import qualified Data.Map as Map
import qualified Data.Set as Set

simplify :: MonadUnique m => ZTerm -> m ZTerm
simplify term = do
  return undefined
  

deforest :: forall m . MonadUnique m => ZTerm -> m (Maybe ZTerm)
deforest term = do
  fun_var <- Var.declare name fun_type Var.Universal
  
  return Nothing
  where
  free_vars = Set.toList $ Var.freeZVars term
  arg_types = map typeOf free_vars
  
  -- The type of the new function we are creating
  fun_type = Type.unflatten (arg_types ++ [typeOf term])
  
  -- The inner_term we will unfold to perform deforestation
  -- and the context that surrounds it
  (outer_cxt, inner_term) = extractInnermost term
  (inner_func : inner_args) = Term.flattenApp inner_term 
  Term.Fix inner_fix_var inner_fix_body = inner_func
  
  -- The inner term with the 'Term.Fix' of the leftmost function removed
  -- i.e. we have unrolled the function call
  unrolled = Eval.normalise 
    $ Term.unflattenApp (inner_fix_body : inner_args) 
  
  -- A name for our new function
  name = "[" 
    ++ intercalate " " (map show free_vars) 
    ++ " -> " ++ show term ++ "]"
    
  -- Applied down every branch of the pattern matches 
  -- in the innermost term.
  -- Fails (returns 'Nothing') if any recursive calls 
  -- to the unrolled innermost term remain.
  deforestBranch :: ZVar -> ZTerm -> MaybeT m ZTerm
  deforestBranch new_fun_var b_term = do
    -- The new variables to generalise recursive innermost function calls
    gen_vars <- mapM makeGenVar rec_calls
    
    -- Generalise innermost recursive calls
    let gen_subst = Map.unions 
          $ zipWith Map.singleton rec_calls (map Term.Var gen_vars)
        gen_b_term = substitute gen_subst b_term
        
    -- Simplify this generalised term    
    simp_gen_b_term <- simplify gen_b_term
    
    -- Replace any instances of the original term with the new function
    -- we are inventing
    let replacement = Map.unions
          $ zipWith makeReplacement gen_vars new_rec_calls
        new_b_term = substitute replacement simp_gen_b_term
        
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
    isRecCall _ = False
    
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
    unifying_substs = mergeUnifiers 
      $ map (unifier inner_rec_call) rec_calls
    
    -- Recursive calls to the new function we are inventing
    -- unified with recursive calls to the innermost function
    new_rec_calls =
      map (\sub -> substitute sub repl_term) unifying_substs
      where
      -- A recursive call to the new function we are inventing
      repl_term = Term.unflattenApp
        $ map Term.Var (new_fun_var : free_vars)
      
    makeReplacement :: ZVar -> ZTerm -> ZTermSubstitution
    makeReplacement gen_var new_rec_call = 
      Map.singleton ( Context.fill outer_cxt (Term.Var gen_var)
                    , Context.fill outer_cxt new_rec_call )
      
        
-- | Splits a term into an innermost function call and an outermost context
extractInnermost :: ZTerm -> (Context, ZTerm)
extractInnermost term
  | (left_args, fix_arg:right_args) <- break isFuncCall args =
      let (inner_cxt, inner_term) = extractInnermost fix_arg
          cxt_func t = Term.unflattenApp $ 
                        func:(left_args ++ (t:right_args))
          outer_cxt = Context.new cxt_func (typeOf fix_arg)
      in (Context.compose inner_cxt outer_cxt, inner_term)
  | otherwise = 
      (Context.identity (typeOf term), term)
  where
  (func:args) = Term.flattenApp term
  isFuncCall = Term.isFix . head . Term.flattenApp

  
-- | Takes a function (some 'Term.Fix') and floats any arguments which are
-- not recursed upon outside the fix.
floatNonStrictArgsOut :: ZTerm -> ZTerm
floatNonStrictArgsOut (Term.Fix fix_var fix_body) = 
  Term.etaReduce overall_term
  where
  (all_vars, raw_body) = Term.flattenLam fix_body
  strict_vars = filter isStrictArg all_vars
  new_body = Term.unflattenLam strict_vars raw_body
  new_fix = Term.Fix fix_var new_body
  inner_term = Term.unflattenApp (new_fix:(map Term.Var strict_vars))
  overall_term = Term.unflattenLam all_vars inner_term
  
  isStrictArg :: ZVar -> Bool
  isStrictArg arg_var = anyWithin matchesVar raw_body 
    where
    matchesVar (Term.Cse _ cse_of _) = cse_of == (Term.Var arg_var)
    matchesVar _ = False
    
floatNonStrictArgsOut other = other
   



