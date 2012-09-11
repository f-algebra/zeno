module Zeno.Tests.Prelude (
  tests
) where

import Prelude ()
import Zeno.Prelude hiding ( assert )
import Test.HUnit ( Test (..), assert )

tests = TestLabel "Prelude"
  $ TestList 
  [ test_deleteIndices
  , test_takeIndices
  , test_isNub ]
  
testCase = TestCase . assert

test_deleteIndices = testCase
  $ deleteIndices [1, 3, 6] [0..7] == [0, 2, 4, 5, 7]
  
test_takeIndices = testCase
  $ takeIndices [1, 3, 6] [0..7] == [1, 3, 6]
  
test_isNub = TestList $ 
  [ testCase $ isNub [0, 2, 1]
  , testCase $ not $ isNub [3, 1, 2, 1, 5] 
  , testCase $ isNub ([] :: [Int]) ]

