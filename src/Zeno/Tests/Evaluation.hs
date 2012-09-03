module Zeno.Tests.Evaluation (
  tests
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Evaluation ( evaluate )
import qualified Zeno.Testing as Test

tests = Test.label "Evaluation"
  $ Test.list 
  [ test_evaluate1 ]

-- | Test some function evaluations, making sure they don't unroll too far
test_evaluate1 = 
  Test.label "evaluate functions" 
    $ Test.run $ do
  Test.loadPrelude
 
  var_xs <- Test.newVar "xs" "list"
  var_n <- Test.newVar "n" "nat"
  var_m <- Test.newVar "m" "nat"
  
  -- Two simple function evaluations, and the desired result of both
  cons_m_app_n_xs <- Test.term 
    "cons m (app (cons n nil) xs)"
  rev_nmxs <- Test.term 
    "rev (cons n (cons m nil))"
  list_mnxs <- Test.term 
    "cons m (cons n xs)"
  list_mn <- Test.term
    "cons m (cons n nil)"
    
  let test1 = Test.assertAlphaEq (evaluate [] cons_m_app_n_xs) list_mnxs
      test2 = Test.assertAlphaEq (evaluate [] rev_nmxs) list_mn
    
  -- Evaluating within a pattern match, and its desired result
  case_app <- Test.term 
    "case n of | 0 -> nil | succ n' -> app (cons n' nil) xs"
  case_result <- Test.term
    "case n of | 0 -> nil | succ n' -> cons n' xs"
    
  let test3 = Test.assertAlphaEq (evaluate [] case_app) case_result
    
  -- Evaluating within a lambda
  lam_rev <- Test.term 
    "fun (x:nat) -> rev (cons x xs)"
  lam_result <- Test.term
    "fun (x:nat) -> app (rev xs) (cons x nil)"
     
  let test4 = Test.assertAlphaEq (evaluate [] lam_rev) lam_result
  
  return
    $ Test.list [test1, test2, test3, test4]


