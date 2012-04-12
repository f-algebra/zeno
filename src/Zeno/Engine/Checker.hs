module Zeno.Engine.Checker (
  ZCounterExample,
  run, explore, guessContext,
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Traversing
import Zeno.Unification
import Zeno.Core ( ZenoState )
import Zeno.Unique ( MonadUnique )
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
  cls' <- unwrapFunctor $ Term.reannotate cls
  let (mby_cex, state') = runState (check maxDepth [] cls') state
  case mby_cex of
    Nothing -> mzero
    Just cex -> do
      put state'
      return cex
      
explore :: forall m . MonadUnique m => ZTerm -> m [ZTerm]
explore term = do
  term' <- Term.reannotate term
  explored <- expl maxDepth term'
  return $ nubOrd $ checkCount explored
  where
  checkCount ts = assert (length ts >= maxDepth) ts
  
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
    explCon con_term = do
      explored <- expl (depth - 1) 
                $ Eval.normalise
                $ replaceWithin (Term.Var st_var) con_term term
      return 
        $ map (replaceWithin con_term (Term.Var st_var)) 
        $ explored
    
  expl _ term 
    | Var.isConstructorTerm term = return [term]
    | otherwise = return []

guessContext :: MonadUnique m => ZTerm -> m Var.Context
guessContext term = do
  potentials <- explore term
  if null potentials
  then return idContext
  else do
    let (p:ps) = potentials
    if all (alphaEq p) ps
    then return idContext --return (Constant p)
    else 
      case matchContext potentials of
        Nothing -> return idContext
        Just (context, gap_type) -> return (Var.Context context gap_type)
  where
  idContext :: Var.Context
  idContext = 
    Var.Context { Var.contextFunction = id
                , Var.contextArgType = typeOf term }
  
  matchContext :: [ZTerm] -> Maybe (ZTerm -> ZTerm, ZType)
  matchContext terms = do
    guard (Var.isConstructorTerm fst_con)
    guard (all (== fst_con) other_cons)
    assert (not $ null gap_is) $ return ()
    guard (length gap_is == 1)
    Just $ case matchContext gaps of
      Nothing -> (context, gap_type)
      Just (inner_context, inner_type) -> (context . inner_context, inner_type)
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
