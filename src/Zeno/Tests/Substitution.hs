module Zeno.Tests.Substitution (
  tests
) where

import Prelude ()
import Zeno.Prelude

import qualified Zeno.Term as Term
import qualified Zeno.Substitution as Substitution
import qualified Zeno.Testing as Test

tests = Test.label "Substitution"
  $ Test.list 
  [ test_substitution ]
  
test_substitution =
    Test.run $ do
  Test.loadPrelude
  
  Test.newVar "n" "nat"
  Test.newVar "xs" "list"
  var_ys <- Test.newVar "ys" "list"
  
  rev_ys <- Test.term "rev ys"
  app <- Test.term "app xs (cons n nil)"
  revapp <- Test.term "rev (app xs (cons n nil))"
  
  revapp' <- Substitution.replace (Term.Var var_ys) app rev_ys
  
  return
    $ Test.assertAlphaEq revapp revapp'
