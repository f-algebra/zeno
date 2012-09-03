module Zeno.Tests.Context (
  tests
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Engine.Simplification ( floatLazyArgsOut )

import qualified Zeno.Term as Term
import qualified Zeno.Context as Context
import qualified Zeno.Testing as Test

tests = Test.label "Context"
  $ Test.list 
  [ test_innermost ]

  
-- | Test that the innermost function of "rev (rev xs)" is "rev xs"
-- that the innermost of "rev (app xs ys)" is "app xs ys"
-- and that the outer context of both is "rev _"
test_innermost = 
  Test.label "innermost" 
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
  let (cxt1, r_xs') = Context.innermost rr_xs
  
  -- Attempt to split "rev (app xs ys)" into 
  -- ("rev _", "app xs ys")
  let (cxt2, app_xs_ys') = Context.innermost rapp_xs_ys
  
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

