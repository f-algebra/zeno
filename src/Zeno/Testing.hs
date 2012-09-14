module Zeno.Testing (
  Test, execute,
  run, term, loadPrelude, newVar,
  label, list, assert, assertAlphaEq
) where

import Prelude ()
import Zeno.Prelude hiding ( assert )
import Zeno.Var ( ZTerm, ZVar )
import Zeno.Core ( Zeno )
import Zeno.Show ()
import Zeno.Unification

import qualified Zeno.Term as Term
import qualified Zeno.Var as Var
import qualified Zeno.Core as Zeno
import qualified Control.Unique as Unique
import qualified Zeno.Parsing.ZML as ZML
import qualified Zeno.Logic as Logic
import qualified Test.HUnit as HUnit

type Test = HUnit.Test

prelude :: String
prelude = unsafePerformIO
  $ readFile "prelude.zthy"

execute :: Test -> IO ()
execute test = do
  HUnit.runTestTT test
  return ()
  
run :: HUnit.Testable t => Zeno t -> Test
run = HUnit.test . flip evalState empty

term :: String -> Zeno ZTerm
term = ZML.readTerm

list :: [Test] -> Test
list = HUnit.TestList

label :: String -> Test -> Test
label = HUnit.TestLabel

assert :: HUnit.Assertable t => t -> Test
assert = HUnit.TestCase . HUnit.assert

assertAlphaEq :: ZTerm -> ZTerm -> Test
assertAlphaEq x y = 
  label (show (Logic.Equal x y))
  $ assert (x `alphaEq` y)

newVar :: String -> String -> Zeno ZVar
newVar name_s typ_s = do
  typ <- ZML.readType typ_s
  new_var <- Var.declare name_s typ Var.Universal
  Zeno.defineTerm name_s (Term.Var new_var)
  return new_var
  
-- Move this into the counter example finding module
-- it's a assertion that will dynamically check equality
assertEq :: ZTerm -> ZTerm -> a -> a
assertEq t1 t2 x = x
 
loadPrelude :: Zeno ()
loadPrelude =
  mapM_ process 
    $ ZML.readLines prelude
  where
  process ("let", arg) = ZML.readBinding arg
  process ("type", arg) = ZML.readTypeDef arg
 

