module Zeno.Tests.All (
  runTests
) where

import qualified Zeno.Tests.Context as Context
import qualified Zeno.Tests.Evaluation as Eval
import qualified Zeno.Tests.Engine.Deforester as Deforester
import qualified Zeno.Tests.Engine.Simplification as Simp
import qualified Zeno.Tests.Engine.Factoring as Factoring
import qualified Zeno.Tests.Prelude as Prelude
import qualified Zeno.Testing as Test

import qualified Test.HUnit.Text as HUnit

tests = Test.list
  [ {- Prelude.tests
  , Context.tests
  , Eval.tests 
  , Simp.tests
  , Factoring.tests
  , -} Deforester.tests ]
  
runTests :: IO ()
runTests = do
  HUnit.runTestTT tests
  return ()

