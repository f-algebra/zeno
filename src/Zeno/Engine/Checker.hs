module Zeno.Engine.Checker (
  ZCounterExample,
  run, explore, guessContext
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Traversing
import Zeno.Unification
import Zeno.Core ( ZenoState )
import Zeno.Var ( ZTerm, ZClause, ZDataType, ZType, 
                  ZVar, ZTermSubstitution )
import Zeno.Reduction
import Zeno.Type ( typeOf )

import qualified Zeno.DataType as DataType
import qualified Zeno.Type as Type
import qualified Zeno.Term as Term
import qualified Zeno.Var as Var
import qualified Zeno.Core as Zeno
import qualified Zeno.Evaluation as Eval
import qualified Data.Map as Map

type ZCounterExample = Substitution ZVar ZTerm
type Check = State ZenoState

maxDepth :: Int
maxDepth = 6

run :: (MonadState ZenoState m, MonadPlus m) => ZClause -> m ZCounterExample
run cls = do
  state <- get
  let (mby_cex, state') = runState (check maxDepth [] cls) state
  case mby_cex of
    Nothing -> mzero
    Just cex -> do
      put state'
      return cex
      
firstM :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
firstM _ [] = return Nothing
firstM f (a:as) = do
  mby_b <- f a
  maybe (firstM f as) (return . Just) mby_b
      
explore :: forall m . MonadState ZenoState m => ZTerm -> m [ZTerm]
explore = expl maxDepth 
  where
  expl :: Int -> ZTerm -> m [ZTerm]
  expl depth term 
    | depth > 0
    , Term.isVar strict_term
    , Type.isVar strict_type = do
        cons <- Var.caseSplit dtype
        concatMapM explCon cons
    where
    strict_term = Eval.strictTerm term
    Term.Var strict_var = strict_term 
    strict_type = typeOf strict_var
    Type.Var dtype = strict_type
    
    explCon :: ZTerm -> m [ZTerm]
    explCon con_term = id
      $ expl (depth - 1) 
      $ Eval.normalise
      $ replaceWithin strict_term con_term term
    
  expl _ term 
    | Var.isConstructorTerm term = return [term]
    | otherwise = return []
    

guessContext :: (MonadPlus m, MonadState ZenoState m) => ZTerm -> m (ZTerm -> ZTerm)
guessContext term = do
  potentials <- explore term
  return undefined
  

check :: Int -> [ZVar] -> ZClause -> Check (Maybe ZCounterExample)
check 0 [] _ = return Nothing
check depth [] cls = 
  check (depth - 1) new_vars cls
  where
  new_vars 
    = nubOrd 
    $ filter (Type.isVar . typeOf)
    $ map Term.fromVar
    $ filter Term.isVar 
    $ map Eval.strictTerm
    $ Term.termList cls
  
check depth (split_var : other_vars) cls = do
  con_terms <- Var.caseSplit 
             $ Type.fromVar 
             $ typeOf split_var
  firstM checkCon con_terms
  where
  checkCon :: ZTerm -> Check (Maybe ZCounterExample)
  checkCon con_term =
    case reduced of
      ReducedTo [] -> return Nothing
      ReducedToFalse -> return success
      ReducedTo sub_clses -> do
        first <- firstM (check depth other_vars) sub_clses
        return (addSplit <$> first)
    where
    reduced 
      = reduce
      $ Eval.normalise
      $ replaceWithin (Term.Var split_var) con_term cls
      
    addSplit = Map.insert split_var con_term
    success = Just (addSplit mempty)
