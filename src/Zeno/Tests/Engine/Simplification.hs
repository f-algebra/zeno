module Zeno.Tests.Engine.Simplification (
  tests
) where

import Zeno.Engine.Simplification ( floatLazyArgsOut )
import qualified Zeno.Testing as Test

tests = Test.label "Simplification" 
  $ Test.list 
  [ test_floatLazy ]
  

-- | Test that for "app" it properly floats the second argument 
-- outside the 'Fix'
test_floatLazy =
  Test.label "floatLazy" 
    $ Test.run $ do
  Test.loadPrelude
  
  -- 'app2' is what 'app' should look like after argument floating
  app2 <- Test.term properly_floated_app
  
  app <- Test.term "app"
  floated_app <- floatLazyArgsOut app
  
  return 
    $ Test.assertAlphaEq floated_app app2
  where
  properly_floated_app = unlines $
    [ "fun (xs:list) (ys:list) -> "
    , "("
    , "fix (f:list->list) in "
    , "fun (zs:list) -> "
    , "case zs of | nil -> ys "
    , "| cons z zs' -> cons z (f zs')"
    , ") xs" ]
