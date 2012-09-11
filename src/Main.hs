module Main where

import Prelude ()
import Zeno.Prelude
import Zeno.Utils ( flipPair )
import Zeno.Core ( ZenoState, Zeno )
import Zeno.Var ( ZTerm )
import Zeno.Show
import Zeno.Tests.All ( runTests )

import qualified Zeno.Engine.Deforester as Deforester
import qualified Zeno.Engine.Simplification as Simplification
import qualified Zeno.Engine.Factoring as Factoring

import qualified Zeno.Evaluation as Eval
import qualified Zeno.Term as Term
import qualified Zeno.Var as Var
import qualified Zeno.Core as Zeno
import qualified Zeno.Parsing.ZML as ZML

-- TODO: tidy up reannotation, find some better way?
-- think I have a fix: only give cse-splits a 'Name', then
-- before unrolling, log every free case-split id beneath the fix
-- then if any remain after reduction the unrolling wasn't complete...
-- will need some method of (re)annotating these names,
-- maybe freshen all Cse's in substitution mapping elements

-- Potential unsoundness: The 'union' operation on substitution maps
-- respects only structural equality, whereas the substitution operation
-- itself uses alpha-equality.

zenoState :: IORef ZenoState
zenoState = unsafePerformIO (newIORef empty)

test :: IO ()
test = runTests

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
{-
command ("explore", arg) = do
  term <- ZML.readTerm arg
  let raw_term = snd (Term.flattenLam term)
  potentials <- Facts.none $ Checker.explore raw_term
  cxt <- runMaybeT $ Facts.none $ Checker.guessContext raw_term
  Zeno.print $ 
    "Potential values for " ++ show term ++ " are:\n" 
    ++ intercalate "\n" (map show potentials)
  Zeno.print $ "Guessed context: " ++ show cxt
  -}
command ("evaluate", arg) = do
  term <- ZML.readTerm arg
  term' <- Eval.normalise term
  Zeno.print (show term')
command ("simplify", arg) = do
  term <- ZML.readTerm arg
  simpler <- Deforester.simplify term
  Zeno.print $ 
    "Simplified: " ++ show term ++ "\n\n" ++ (show simpler)
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


