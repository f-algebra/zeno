module Zeno.Tests.Engine.Factoring (
  tests
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Engine.Simplification ( floatLazyArgsOut )

import qualified Zeno.Term as Term
import qualified Zeno.Engine.Factoring as Factoring
import qualified Zeno.Testing as Test
import qualified Control.Failure as Fail

tests = Test.label "Factoring"
  $ Test.list 
  [ test_valueFactoring
  , test_nullFactoring
  , test_constContext ]


-- | Test that the deforested form of "rev (xs ++ [n])"
-- factors into "n :: (rev xs)"
test_valueFactoring =
  Test.label "Value factoring"
    $ Test.run $ do
  Test.loadPrelude
  
  Test.newVar "xs" "list"
  Test.newVar "n" "nat"
  
  simpl_term <- Test.term simpl_revapp
  goal_term <- Test.term "cons n (rev xs)"
  factored_term <- Fail.success 
    $ Factoring.value simpl_term
  
  return
    $ Test.assertAlphaEq factored_term goal_term
  where
  simpl_revapp = unlines $
    [ "("
    , "fix (f:list->list) in "
    , "fun (ys:list) -> "
    , "case ys of | nil -> cons n nil "
    , "| cons y ys' -> app (f ys') (cons y nil)"
    , ") xs" ]
    
-- | Test constant context factoring, which can be used to
-- reduce a recursive identity function into a non-recursive one.
test_constContext =
  Test.label "Constant context"
    $ Test.run $ do
  Test.loadPrelude
  
  var_n <- Test.newVar "n" "nat"
  
  id_n <- Test.term id_n_def
  factored <- Fail.success
    $ Factoring.value id_n
    
  return
    $ Test.assertAlphaEq factored (Term.Var var_n)
  where
  id_n_def = unlines
    [ "(fix (idr:nat->nat) in fun (x:nat) -> "
    , "  case x of 0 -> 0 | succ y -> succ (idr y)) n" ]

    
-- | Test that nothing happens to a function
-- which cannot be factored
test_nullFactoring =
  Test.label "Null factoring"
    $ Test.run $ do
  Test.loadPrelude
  
  Test.newVar "xs" "list"
  Test.newVar "n" "nat"
  
  app_n <- floatLazyArgsOut 
    =<< Test.term "app xs (cons n nil)"
  
  mby_factored <- Fail.toMaybe 
    $ Factoring.value app_n
  
  return
    $ Test.assert (isNothing mby_factored)
    
  
