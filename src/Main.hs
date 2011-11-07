module Main where

import Prelude ()
import Zeno.Prelude
import Zeno.Core
import Zeno.Parsing.Z
import Zeno.Show
import Zeno.Evaluation

import qualified Zeno.Theory as Thy

main :: IO ()
main = do 
  zthy <- readFile "test.zthy"
  print $ flip execState Thy.empty $ parse zthy
