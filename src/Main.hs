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
zenoState = unsafePerformIO (newIORef empty)

runZeno :: a -> Zeno a -> IO a
runZeno on_error zeno = do
  either_err <- atomicModifyIORef zenoState modifyState
  case either_err of
    Left err -> do
      putStrLn $ "Uncaught exception: " ++ show err
      return on_error
    Right a -> return a
  where
  modifyState st = 
    case Zeno.runZeno zeno st of
      Left err -> (st, Left err)
      Right (a, st') -> (st', Right a)

flush :: IO ()
flush = do
  output <- runZeno [] Zeno.flush
  mapM_ putStrLn output

interpret :: String -> IO ()
interpret str =
  case ZML.readLine str of
    (Nothing, _) -> return ()
    (Just cmd, rest) -> do
      runZeno () (command cmd) 
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


