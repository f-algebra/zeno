module Zeno.Tests.Engine.Factoring (
  tests
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Engine.Simplification ( floatLazyArgsOut )

import qualified Zeno.Engine.Factoring as Factoring
import qualified Zeno.Testing as Test

tests = Test.label "Factoring"
  $ Test.list 
  [ test_valueFactoring
  , test_nullFactoring ]

  
-- | Test that the deforested form of "rev (xs ++ [n])"
-- factors into "n :: (rev xs)"
test_valueFactoring = do
  Test.label "Value factoring"
    $ Test.run $ do
  Test.loadPrelude
  
  Test.newVar "xs" "list"
  Test.newVar "n" "nat"
  
  simpl_term <- Test.term simpl_revapp
  goal_term <- Test.term "cons n (rev xs)"
  Just factored_term <- runMaybeT 
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
    
    
-- | Test that nothing happens to a function
-- which cannot be factored
test_nullFactoring = do
  Test.label "Null factoring"
    $ Test.run $ do
  Test.loadPrelude
  
  Test.newVar "xs" "list"
  Test.newVar "n" "nat"
  
  app_n <- floatLazyArgsOut 
    =<< Test.term "app xs (cons n nil)"
  
  mby_factored <- runMaybeT 
    $ Factoring.value app_n
  
  return
    $ Test.assert (isNothing mby_factored)
    
  
