module Main where

import Prelude ()
import Zeno.Prelude
import Zeno.Utils ( flipPair )
import Zeno.Core ( ZenoState, Zeno )
import Zeno.Show
import Zeno.Evaluation

import qualified Zeno.Core as Zeno
import qualified Zeno.Parsing.ZML as ZML 
import qualified Zeno.Engine.Simplifier as Simplifier

zenoState :: IORef ZenoState
zenoState = unsafePerformIO (newIORef empty)

runZeno :: Zeno a -> IO a
runZeno zeno = 
  atomicModifyIORef zenoState (flipPair . runState zeno)

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
command ("simplify", arg) = do
  term <- ZML.readTerm arg
  Zeno.println (show (Simplifier.run term))
command (other, _) = 
  error $ "Command \"" ++ other ++ "\" not recognized."

main :: IO ()
main = do 
  zthy <- readFile "test.zthy"
  interpret zthy


