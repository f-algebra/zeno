module Main where

import Prelude ()
import Zeno.Prelude
import Zeno.Core
import Zeno.Show
import Zeno.Evaluation

import qualified Zeno.Core as Zeno
import qualified Zeno.Parsing.ZML as ZML 

interpret :: String -> Zeno ()
interpret str =
  case ZML.readLine str of
    (Nothing, _) -> return ()
    (Just cmd, rest) -> command cmd >> interpret rest

command :: (String, String) -> Zeno ()
command ("type", arg) = ZML.readTypeDef arg

main :: IO ()
main = do 
  zthy <- readFile "test.zthy"
  print $ Zeno.theory $ flip execState Zeno.initialState $ interpret zthy
