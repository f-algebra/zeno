module Main where

import Prelude ()
import Zeno.Prelude
import Zeno.Core
import Zeno.Show
import Zeno.Parsing.ZLisp
import Zeno.Evaluation

import qualified Zeno.Core as Zeno

main :: IO ()
main = do 
  zthy <- readFile "test.zthy"
  print $ Zeno.theory $ flip execState Zeno.initialState $ parse zthy
