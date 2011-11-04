module Main where

import Prelude ()
import Zeno.Prelude
import Zeno.Core
import Zeno.Parsing.Z
import Zeno.Show

main :: IO ()
main = do 
  zthy <- readFile "test.zthy"
  print $ flip execState emptyTheory $ parse zthy
