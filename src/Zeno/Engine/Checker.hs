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
    , Term.isVar st_term
    , Var.destructible st_term = do
        cons <- Var.caseSplit st_dtype
        concatMapM explCon cons
    where
    st_term = Eval.strictTerm term
    st_var = Term.fromVar st_term
    st_dtype = Type.fromVar (typeOf st_var)
    
    explCon :: ZTerm -> m [ZTerm]
    explCon con_term = id
      $ expl (depth - 1) 
      $ Eval.normalise
      $ replaceWithin (Term.Var st_var) con_term term
    
  expl _ term 
    | Var.isConstructorTerm term = return [term]
    | otherwise = return []

guessContext :: (MonadPlus m, MonadState ZenoState m) => 
  ZTerm -> m (ZTerm -> ZTerm, ZType)
guessContext term = do
  potentials <- explore term
  case matchContext potentials of
    Nothing -> mzero
    Just context -> return context
  where
  matchContext :: [ZTerm] -> Maybe (ZTerm -> ZTerm, ZType)
  matchContext terms = do
    guard (Var.isConstructorTerm fst_con)
    guard (all (== fst_con) other_cons)
    guard (length gap_is == 1)
    case matchContext gaps of
      Nothing -> Just (context, gap_type)
      Just (inner_context, inner_type) -> Just (context . inner_context, inner_type)
    where
    flattened = map Term.flattenApp terms
    (fst_con:other_cons) = map head flattened
    args = map tail flattened
    paired_args = transpose args
    gap_is = findIndices (\(a:as) -> any (not . alphaEq a) as) paired_args
    gap_i = head gap_is
    gaps = map (!! gap_i) args
    gap_type = typeOf (head gaps)
    context fill = Term.unflattenApp $ fst_con:(setAt gap_i fill (head args))
  
check :: Int -> [ZVar] -> ZClause -> Check (Maybe ZCounterExample)
check 0 [] _ = return Nothing
check depth [] cls = 
  check (depth - 1) new_vars cls
  where
  new_vars 
    = nubOrd 
    $ filter (Type.isVar . typeOf)
    $ filter (not . Var.isConstructor)
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
