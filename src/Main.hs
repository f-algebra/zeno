module Main where

import Prelude ()
import Zeno.Prelude
import Zeno.Utils ( flipPair )
import Zeno.Core ( ZenoState, Zeno )
import Zeno.Var ( ZTerm )
import Zeno.Show
import Zeno.Evaluation ( normalise )

import qualified Zeno.Engine.Simplifier as Simplifier
import qualified Zeno.Engine.Inventor as Inventor
import qualified Zeno.Engine.Checker as Checker

import qualified Zeno.Term as Term
import qualified Zeno.Var as Var
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
command ("explore", arg) = do
  term <- ZML.readTerm arg
  let raw_term = snd (Term.flattenLam term)
  potentials <- Checker.explore raw_term
  mby_context <- runMaybeT $ Checker.guessContext raw_term
  cxt_filler <- Term.Var <$> Var.declare "..." empty Var.Bound
  Zeno.print $ 
    "Potential values for " ++ show term ++ " are:\n" 
    ++ intercalate "\n" (map show potentials)
  case mby_context of
    Nothing -> return ()
    Just cxt -> Zeno.print $
      "Guessed context: " ++ show (cxt cxt_filler)
command ("evaluate", arg) = do
  term <- ZML.readTerm arg
  Zeno.print (show (normalise term))
command ("simplify", arg) = do
  term <- ZML.readTerm arg
  term' <- runMaybeT (Simplifier.run term)
  Zeno.print $
    case term' of
      Nothing -> show term ++ "\ncould not be simplified."
      Just (term', prove_me) ->
        show term ++ "\n\nsimplifies to\n\n" ++ showWithDefinitions term'
        ++ "\n\nif the following hold\n\n" ++ (intercalate "\n\n" . map show) prove_me
command ("invent", arg) = do
  (func, args, res) <- ZML.readSpec arg
  mby_def <- runMaybeT (Inventor.run args res)
  Zeno.print $ 
    case mby_def of
      Nothing -> "Couldn't invent a definition for " ++ func
      Just def -> "Found " ++ func ++ " = " ++ show def
command ("check", arg) = do
  Just prop <- Zeno.lookupProp arg
  mby_cex <- runMaybeT (Checker.run prop)
  Zeno.print $ 
    case mby_cex of
      Nothing -> "Could not find counter-example."
      Just cex -> showSubstitution cex
command (other, _) = 
  return () 
  -- error $ "Command \"" ++ other ++ "\" not recognized."

main :: IO ()
main = do 
  zthy <- readFile "test.zthy"
  interpret zthy


