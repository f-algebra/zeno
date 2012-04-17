module Main where

import Prelude ()
import Zeno.Prelude
import Zeno.Utils ( flipPair )
import Zeno.Core ( ZenoState, Zeno )
import Zeno.Var ( ZTerm )
import Zeno.Show
import Zeno.Evaluation ( normalise )

import qualified Zeno.Engine.Simplifier as Simplifier
import qualified Zeno.Engine.Checker as Checker

import qualified Zeno.Facts as Facts
import qualified Zeno.Evaluation as Eval
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
  potentials <- Facts.none $ Checker.explore raw_term
  mby_context <- Facts.none $ runMaybeT $ Checker.guessContext raw_term
  Zeno.print $ 
    "Potential values for " ++ show term ++ " are:\n" 
    ++ intercalate "\n" (map show potentials)
  case mby_context of
    Nothing -> return () {-
    Just (Var.Constant kterm) -> 
      Zeno.print $ "Guessed constant term: " ++ show kterm -}
    Just (Var.Context cxt typ) -> do
      cxt_filler <- Var.declare ("{" ++ show typ ++ "}") typ Var.Universal
      Zeno.print $ "Guessed context: " ++ show (cxt (Term.Var cxt_filler))
command ("evaluate", arg) = do
  term <- ZML.readTerm arg
  term' <- Facts.none $ Eval.normalise term
  Zeno.print (show term')
command ("simplify", arg) = do
  term <- ZML.readTerm arg
  term' <- map fromJust $ Simplifier.run term
  Zeno.print (showWithDefinitions term')
{-
  Zeno.print $
    case term' of
      Nothing -> show term ++ "\ncould not be simplified."
      Just (term', prove_me) ->
        show term ++ "\n\nsimplifies to:\n\n" ++ showWithDefinitions term'
        ++ "\n\nusing the following properties:\n\n" 
        ++ (intercalate "\n\n" . map showWithDefinitions) prove_me
-}
{-
command ("invent", arg) = do
  (func, args, res) <- ZML.readSpec arg
  mby_def <- undefined -- (Inventor.run ...)
  Zeno.print $ 
    case mby_def of
      Nothing -> "Couldn't invent a definition for " ++ func
     -- Just def -> "Found " ++ func ++ " = " ++ show def
command ("check", arg) = do
  Just prop <- Zeno.lookupProp arg
  mby_cex <- Facts.none $ runMaybeT $ Checker.falsify prop
  Zeno.print $ 
    case mby_cex of
      Nothing -> "Could not find counter-example."
      Just cex -> showSubstitution cex -}
command (other, _) = 
  return () 
  -- error $ "Command \"" ++ other ++ "\" not recognized."

main :: IO ()
main = do 
  zthy <- readFile "test.zthy"
  interpret zthy


