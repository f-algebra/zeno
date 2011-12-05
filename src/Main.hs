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
<<<<<<< HEAD
import qualified Zeno.Engine.Simplifier as Simplifier
=======
>>>>>>> 7996c10bf5c7913cfb8e5f22f726d782e7355b49

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
<<<<<<< HEAD
  Zeno.println (show (evaluate term))
command ("simplify", arg) = do
  term <- ZML.readTerm arg
  Zeno.println (show (Simplifier.run term))
=======
  Zeno.print (show (normalise term))
command ("simplify", arg) = do
  term <- ZML.readTerm arg
  term' <- Simplifier.run (Deforester.run term)
  Zeno.print $
    case term' of
      Nothing -> show term ++ "\ncould not be simplified."
      Just term' -> show term ++ "\nsimplified to\n" ++ show term'
>>>>>>> 7996c10bf5c7913cfb8e5f22f726d782e7355b49
command (other, _) = 
  error $ "Command \"" ++ other ++ "\" not recognized."

main :: IO ()
main = do 
  zthy <- readFile "test.zthy"
  interpret zthy


