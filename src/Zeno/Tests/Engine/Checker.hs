module Zeno.Tests.Engine.Checker (
  tests
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Var ( ZTerm )

import qualified Zeno.Term as Term
import qualified Zeno.Context as Context
import qualified Zeno.Testing as Test
import qualified Zeno.Engine.Checker as Checker
import qualified Control.Failure as Fail

tests = Test.label "Checker"
  $ Test.list
  [ test_explore
  , test_guessContext ]
  
-- How do I test this?
test_explore = do
  Test.label "Explore"
    $ Test.run $ do
  Test.loadPrelude
  
  return
    $ Test.list []
  

test_guessContext = do
  Test.label "Context guessing"
    $ Test.run $ do
  Test.loadPrelude
  Test.newVar "xs" "list"
  Test.newVar "n" "nat"
  
  -- Test that the context of "rev (xs ++ [n])" is "cons n _"
  revapp <- Test.term "rev (app xs (cons n nil))"
  guessed_cxt <- Fail.success
    $ Checker.guessContext revapp
  var_ys <- Test.newVar "ys" "list"
  let cxt_ys = Context.fill guessed_cxt (Term.Var var_ys)
  cons_n_ys <- Test.term "cons n ys"
  let test1 = Test.assert $ cxt_ys == cons_n_ys

  -- Test that the context of a recursive identity function
  -- over "n" is just "n", i.e. a constant context
  
  
  return 
    $ Test.list [test1]
  
