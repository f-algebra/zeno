module Zeno.Engine.Deforester  (
  simplify,
  _test
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
import Zeno.Engine.Simplifications ( floatLazyArgsOut )
import Zeno.Core ( Zeno )

import qualified Zeno.Facts as Facts
import qualified Zeno.Evaluation as Eval
import qualified Zeno.DataType as DataType
import qualified Zeno.Type as Type
import qualified Zeno.Term as Term
import qualified Zeno.Var as Var
import qualified Zeno.Unique as Unique
import qualified Zeno.Context as Context
import qualified Zeno.Testing as Test
import qualified Zeno.Show as Show
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Test.HUnit as HUnit

simplify :: MonadUnique m => ZTerm -> m ZTerm
simplify term = do
  term2 <- floatLazyArgsOut term
  fromMaybe term2 
    `liftM` deforest term2


deforest :: forall m . MonadUnique m => ZTerm -> m (Maybe ZTerm)
deforest term 
  | Term.isFix inner_func = runMaybeT $ do
  -- Create a new function variable for the new function 
  -- we are inventing
  fun_var <- Var.declare name fun_type Var.Universal
  
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
  (outer_cxt, inner_term) = extractInnermost term
  (inner_func : inner_args) = Term.flattenApp inner_term 
  Term.Fix inner_fix_var inner_fix_body = inner_func
  
  free_vars = Set.toList
    $ Set.unions 
    $ map Var.freeZVars 
    $ inner_args
  arg_types = map typeOf free_vars
  
  -- The type of the new function we are creating
  fun_type = Type.unflatten (arg_types ++ [typeOf term])
  
  -- The inner term with the 'Term.Fix' of the leftmost function removed
  -- i.e. we have unrolled the function call
  unrolled = Eval.evaluate []
    $ Term.unflattenApp (inner_fix_body : inner_args) 
  
  -- We then push the outer context down into every branch 
  cxt_applied = 
    Term.mapCaseBranches (Eval.evaluate [] . Context.fill outer_cxt)
    $ unrolled
    
  -- A name for our new function
  name = "[" 
    ++ intercalate " " (map show free_vars) 
    ++ " -> " ++ Show.simple term ++ "]"
    
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
    unifying_substs :: [ZTermSubstitution]
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
      
    makeReplacement :: ZVar -> ZTerm -> ZTermSubstitution
    makeReplacement gen_var new_rec_call = 
      Map.singleton (Context.fill outer_cxt (Term.Var gen_var))
                    new_rec_call
      
deforest _ = 
  return Nothing
        
-- | Splits a term into an innermost function call
-- and an outermost context
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
  

-- * Tests

_test = Test.list 
  [ _test_extractInnermost 
  , _test_deforestSimple
  , _test_deforestHOF
  , _test_valueFactoring ]
  
assertSimpEq :: ZTerm -> ZTerm -> Zeno HUnit.Test
assertSimpEq t1 t2 = do
  t1' <- simplify t1
  t2' <- simplify t2
  return $ Test.assertAlphaEq t1' t2'

  
-- | Test that the innermost function of "rev (rev xs)" is "rev xs"
-- that the innermost of "rev (app xs ys)" is "app xs ys"
-- and that the outer context of both is "rev _"
_test_extractInnermost = 
  Test.label "extractInnermost" 
    $ Test.run $ do
  Test.loadPrelude
  Test.newVar "xs" "list"
  var_ys <- Test.newVar "ys" "list"
  let floatedTerm = floatLazyArgsOut <=< Test.term
  
  rr_xs <- floatedTerm "rev (rev xs)"
  rapp_xs_ys <- floatedTerm "rev (app xs ys)"
  r_ys <- floatedTerm "rev ys"
  r_xs <- floatedTerm "rev xs"
  app_xs_ys <- floatedTerm"app xs ys"
  
  -- Attempt to split "rev (rev xs)" into
  -- ("rev _", "rev xs")
  let (cxt1, r_xs') = extractInnermost rr_xs
  
  -- Attempt to split "rev (app xs ys)" into 
  -- ("rev _", "app xs ys")
  let (cxt2, app_xs_ys') = extractInnermost rapp_xs_ys
  
  -- Check that the inner terms are correct
  let test1 = Test.assertAlphaEq r_xs' r_xs
      test2 = Test.assertAlphaEq app_xs_ys' app_xs_ys
  
  -- Check that the outer context of both is "rev _"
  -- by filling it with "ys" and seeing if it is equal to "rev ys"
  let cxt1_ys = Context.fill cxt1 (Term.Var var_ys)
      cxt2_ys = Context.fill cxt2 (Term.Var var_ys)
      test3 = Test.assertAlphaEq cxt1_ys r_ys
      test4 = Test.assertAlphaEq cxt2_ys r_ys
  
  return $ Test.list [test1, test2, test3, test4]
  
  
-- | Test some simple deforestations
_test_deforestSimple = 
  Test.label "Deforesting revapp"
    $ Test.run $ do
  Test.loadPrelude
  Test.newVar "xs" "list"
  Test.newVar "n" "nat"
  
  -- We will simplify "rev (xs ++ [n])"
  rev_app <- Test.term "rev (app xs (cons n nil))"
  desired_form <- Test.term simple_revapp
  
  assertSimpEq rev_app desired_form
  where
  simple_revapp = unlines $
    [ "("
    , "fix (f:list->list) in "
    , "fun (ys:list) -> "
    , "case ys of | nil -> cons n nil "
    , "| cons y ys' -> app (f ys') (cons y nil)"
    , ") xs" ]
    
    
-- | Simplify some higher-order functions
_test_deforestHOF =
  Test.label "Deforesting HOF" 
    $ Test.run $ do
  Test.loadPrelude
  
  Test.newVar "xs" "list"
  Test.newVar "f" "nat -> nat"
  Test.newVar "g" "nat -> nat"
  
  mapmap1 <- Test.term "map f (map g xs)"
  mapmap2 <- Test.term "map (fun (x:nat) -> f (g x)) xs"
  
  assertSimpEq mapmap1 mapmap2
  
  
-- | Test simplifications which require value factoring
_test_valueFactoring =
  Test.label "Testing value factoring"
    $ Test.run $ do
  Test.loadPrelude
  
  var_xs <- Test.newVar "xs" "list"
  revrev <- Test.term "rev (rev xs)"
  
  assertSimpEq revrev (Term.Var var_xs)

