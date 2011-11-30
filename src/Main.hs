module Main where

import Prelude ()
import Zeno.Utils ( flipPair )
import Zeno.Prelude
import Zeno.Core ( ZenoState, Zeno )
import Zeno.Show
import Zeno.Evaluation

import qualified Zeno.Core as Zeno
import qualified Zeno.Parsing.ZML as ZML 

-- NEED TO MAKE ZENO A ERROR MONAD TOO

zenoState :: IORef ZenoState
zenoState = unsafePerformIO (newIORef Zeno.initialState)

runZeno :: Zeno a -> IO a
runZeno zeno = do
  (a, s) <- atomicModifyIORef zenoState (runState (runErrorT zeno))
  case a of
    Left err -> 

flush :: IO ()
flush = do
  output <- runZeno Zeno.flush
  mapM_ putStrLn output

interpret :: String -> IO ()
interpret str =
  case ZML.readLine str of
    (Nothing, _) -> return ()
    (Just cmd, rest) -> do
      runZeno (command cmd) 
      flush
      interpret rest

command :: (String, String) -> Zeno ()
command ("type", arg) = ZML.readTypeDef arg
command ("let", arg) = ZML.readBinding arg
command ("prop", arg) = ZML.readProp arg
command ("eval", arg) = do
  term <- ZML.readTerm arg
  Zeno.println (show (evaluate term))
command (other, _) = 
  error $ "Command \"" ++ other ++ "\" not recognized."

main :: IO ()
main = do 
  zthy <- readFile "test.zthy"
  interpret zthy


