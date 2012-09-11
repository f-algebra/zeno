module Zeno.Tests.Unification (
  tests
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Unification

import Zeno.Engine.Simplification ( floatLazyArgsOut )

import qualified Zeno.Substitution as Substitution
import qualified Zeno.Term as Term
import qualified Zeno.Testing as Test
import qualified Control.Failure as Fail

tests = Test.label "Unification"
  $ Test.list 
  [ test_simpleUni 
  , test_alphaEq ]
  
test_simpleUni = 
  Test.label "Simple unification" 
    $ Test.run $ do
  Test.loadPrelude
  
  var_xs <- Test.newVar "xs" "list"
  let term_xs = Term.Var var_xs 
 
  map1 <- Fail.success
    $ unifier term_xs term_xs
  let test1 = Test.assert $ Substitution.null map1
  
  return
    $ Test.list [test1]
    
  
test_alphaEq = 
  Test.label "Alpha equality"
    $ Test.run $ do
  Test.loadPrelude
  
  Test.newVar "n" "nat"
  Test.newVar "xs" "list"
    
  t1 <- Test.term some_term
  t2 <- Test.term some_term
  
  let floatedTerm = floatLazyArgsOut <=< Test.term
  revapp1 <- floatedTerm revapp
  revapp2 <- floatedTerm revapp
  
  let test1 = Test.assertAlphaEq t1 t2  
      test2 = Test.assertAlphaEq revapp1 revapp2
  
  return
    $ Test.list [test1, test2]
  where
  revapp = "rev (app xs (cons n nil))"
  some_term = unlines $
    [ "(fix (f:nat->nat) in fun (x:nat) -> "
    , " case x of 0 -> n | succ x' -> succ (f x')) n" ]
 
  
