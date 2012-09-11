module Zeno.Tests.Context (
  tests
) where

import Prelude ()
import Zeno.Prelude

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
  
  Test.newVar "n" "nat"
  Test.newVar "m" "nat"
  Test.newVar "xs" "list"
  var_ys <- Test.newVar "ys" "list"
  
  -- Attempt to split "rev (rev xs)" into
  -- ("rev _", "rev xs")
  rr_xs <- Test.term "rev (rev xs)"
  cxt1_ys <- Test.term "rev ys"
  r_xs <- Test.term "rev xs"
  
  let (cxt1, r_xs') = Context.innermost rr_xs
      cxt1_ys' = Context.fill cxt1 (Term.Var var_ys)
      
  let test1 = Test.assertAlphaEq r_xs' r_xs
      test2 = Test.assertAlphaEq cxt1_ys' cxt1_ys
  
  
  -- Attempt to split this app-rev-app example
  app_rev_app <- Test.term "app (rev (app xs (cons n nil))) (cons m nil)"
  cxt2_ys <- Test.term "app (rev ys) (cons m nil)"
  app_xs_n <- Test.term "app xs (cons n nil)"
  
  let (cxt2, app_xs_n') = Context.innermost app_rev_app
      cxt2_ys' = Context.fill cxt2 (Term.Var var_ys)
      
  let test3 = Test.assertAlphaEq app_xs_n' app_xs_n
      test4 =  Test.assertAlphaEq cxt2_ys' cxt2_ys
  

  -- Check that "rev ys" splits into "_" and "rev ys"
  -- to test whether it correctly returns null contexts
  r_ys <- Test.term "rev ys"
  let (cxt_3, r_ys') = Context.innermost r_ys
      test5 = Test.assert (Context.null cxt_3)
      test6 = Test.assertAlphaEq r_ys r_ys'
  
  return $ Test.list [test1, test2, test3, test4, test5, test6]

