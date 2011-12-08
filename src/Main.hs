module Main where

import Prelude ()
import Zeno.Prelude
import Zeno.Utils ( flipPair )
import Zeno.Core ( ZenoState, Zeno )
import Zeno.Show
import Zeno.Evaluation ( normalise )

import qualified Zeno.Engine.Simplifier as Simplifier
import qualified Zeno.Engine.Inventor as Inventor
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
command ("evaluate", arg) = do
  term <- ZML.readTerm arg
  Zeno.print (show (normalise term))
command ("simplify", arg) = do
  term <- ZML.readTerm arg
  term' <- runMaybeT (Simplifier.run term)
  Zeno.print $
    case term' of
      Nothing -> show term ++ "\ncould not be simplified."
      Just term' -> show term ++ "\nsimplified to\n" ++ show term'
command ("invent", arg) = do
  (func, args, res) <- ZML.readSpec arg
  mby_def <- runMaybeT (Inventor.run args res)
  Zeno.print $ 
    case mby_def of
      Nothing -> "Couldn't invent a definition for " ++ func
      Just def -> "Found " ++ func ++ " = " ++ show def
command (other, _) = 
  error $ "Command \"" ++ other ++ "\" not recognized."

main :: IO ()
main = do 
  zthy <- readFile "test.zthy"
  interpret zthy


