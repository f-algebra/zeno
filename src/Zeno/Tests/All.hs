module Zeno.Tests.All (
  runTests
) where

import qualified Zeno.Tests.Substitution as Substitution
import qualified Zeno.Tests.Unification as Unification
import qualified Zeno.Tests.Context as Context
import qualified Zeno.Tests.Evaluation as Eval
import qualified Zeno.Tests.Engine.Deforester as Deforester
import qualified Zeno.Tests.Engine.Simplification as Simp
import qualified Zeno.Tests.Engine.Factoring as Factoring
import qualified Zeno.Tests.Engine.Checker as Checker
import qualified Zeno.Tests.Prelude as Prelude
import qualified Zeno.Testing as Test

tests = Test.list
  [ Prelude.tests
  , Substitution.tests
  , Unification.tests
  , Context.tests
  , Eval.tests 
  , Checker.tests
  , Simp.tests 
  , Factoring.tests
  , Deforester.tests ]
  
runTests :: IO ()
runTests = Test.execute tests

