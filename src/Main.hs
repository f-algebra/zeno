module Main where

import Prelude ()
import Zeno.Prelude
import Zeno.Utils ( flipPair )
import Zeno.Core ( ZenoState, Zeno )
import Zeno.Show
import Zeno.Evaluation ( normalise )

import qualified Zeno.Engine.Deforester as Deforester
import qualified Zeno.Simplifier as Simplifier
import qualified Zeno.Core as Zeno
import qualified Zeno.Parsing.ZML as ZML 

zenoState :: IORef ZenoState
zenoState = unsafePerformIO (newIORef empty)

runZeno :: Zeno a -> IO a
runZeno zeno = 
  atomicModifyIORef zenoState (flipPair . runState zeno)

flush :: IO ()
flush = do
  output <- runZeno Zeno.flush
  mapM_ (\s -> putStrLn ("\n" ++ s ++ "\n")) output

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
  Zeno.print (show (normalise term))
command ("simplify", arg) = do
  term <- ZML.readTerm arg
  term' <- Simplifier.run (Deforester.run term)
  Zeno.print $
    case term' of
      Nothing -> show term ++ "\ncould not be simplified."
      Just term' -> show term ++ "\nsimplified to\n" ++ show term'
command (other, _) = 
  error $ "Command \"" ++ other ++ "\" not recognized."

main :: IO ()
main = do 
  zthy <- readFile "test.zthy"
  interpret zthy


