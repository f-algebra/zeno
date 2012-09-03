module Zeno.Tests.Engine.Deforester (
  tests
) where

import Zeno.Core ( Zeno )
import Zeno.Var ( ZTerm )
import Zeno.Engine.Deforester ( simplify )

import qualified Zeno.Term as Term
import qualified Zeno.Testing as Test

tests = Test.label "Deforester"
  $ Test.list 
  [ test_deforestSimple
  , test_deforestHOF
  , test_valueFactoring ]
  
assertSimpEq :: ZTerm -> ZTerm -> Zeno Test.Test
assertSimpEq t1 t2 = do
  t1' <- simplify t1
  t2' <- simplify t2
  return $ Test.assertAlphaEq t1' t2'

  
-- | Test some simple deforestations
test_deforestSimple = 
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
test_deforestHOF =
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
test_valueFactoring =
  Test.label "Testing value factoring"
    $ Test.run $ do
  Test.loadPrelude
  
  var_xs <- Test.newVar "xs" "list"
  revrev <- Test.term "rev (rev xs)"
  
  assertSimpEq revrev (Term.Var var_xs)
