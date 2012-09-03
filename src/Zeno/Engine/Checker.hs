module Zeno.Engine.Checker (
  ZCounterExample,
  falsify, explore, guessContext, inconsistent
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Traversing
import Zeno.Unification
import Zeno.Core ( ZenoState )
import Zeno.Unique ( MonadUnique )
import Zeno.Context ( Context )
import Zeno.Var ( ZTerm, ZClause, ZDataType, ZType, 
                  ZVar, ZTermSubstitution, ZEquation )
import Zeno.Term ( TermTraversable (..) )
import Zeno.Reduction
import Zeno.Type ( typeOf )

import qualified Zeno.Context as Context
import qualified Zeno.DataType as DataType
import qualified Zeno.Type as Type
import qualified Zeno.Logic as Logic
import qualified Zeno.Facts as Facts
import qualified Zeno.Term as Term
import qualified Zeno.Var as Var
import qualified Zeno.Core as Zeno
import qualified Zeno.Evaluation as Eval

import qualified Data.Set as Set
import qualified Data.Map as Map

type ZCounterExample = Substitution ZVar ZTerm

maxDepth :: Int
maxDepth = 6
  
strictVars :: (Facts.Reader m, TermTraversable t ZVar) => t -> m [ZVar]
strictVars terms = do
  s_terms <- mapM Eval.strictTerm (Term.termList terms)
  return 
    . nubOrd
    . map Term.fromVar
    . filter Var.isDestructible
    . filter Term.isVar 
    $ s_terms

inconsistent :: forall m . (Facts.Reader m, MonadUnique m) => m Bool
inconsistent = invalid maxDepth [] =<< Facts.ask 
  where
  invalid :: Int -> [ZVar] -> [ZEquation] -> m Bool
  invalid 0 [] _ = return False
  invalid depth [] eqs = do
    s_vars <- liftM nubOrd $ concatMapM strictVars eqs
    invalid (depth - 1) s_vars eqs
  invalid depth (split_var : other_vars) eqs = do
    con_terms <- Var.caseSplit 
               $ Type.fromVar 
               $ typeOf split_var
    allM invalidCon con_terms
    where
    invalidCon :: ZTerm -> m Bool
    invalidCon con_term = do 
      reduced <- liftM (concatMap reduce)
        $ mapM Eval.normalise
        $ map (replaceWithin (Term.Var split_var) con_term) eqs
      case reduced of
        ReducedToFalse -> return True
        ReducedTo eqs' -> invalid depth other_vars eqs'
  
explore :: forall m . (MonadUnique m, Facts.Reader m) => 
  ZTerm -> m [ZTerm]
explore term = do
  term' <- Term.reannotate term
  explored <- expl maxDepth term'
  return $ nubOrd $ checkCount explored
  where
  checkCount ts = assert (length ts >= maxDepth) ts
  
  expl :: Int -> ZTerm -> m [ZTerm]
  expl depth term = do
    if depth == 0
    then finished
    else do
      ct_term <- Eval.criticalTerm term
      if not (Var.isDestructible ct_term)
      then finished
      else do
        cons <- Var.caseSplit (Type.fromVar (typeOf ct_term))
        case ct_term of
          Term.Var ct_var -> concatMapM (explInd ct_var) cons
          ct_term -> concatMapM (explSplit ct_term) cons 
    where
    finished 
      | Var.isConstructorTerm term = return [term]
      | otherwise = return []

    explInd :: ZVar -> ZTerm -> m [ZTerm]
    explInd ct_var con_term = do
      explore_me <- Eval.normalise term'
      explored <- expl (depth - 1) explore_me
      return
        $ replaceWithin con_term ct_vterm explored
     where
     ct_vterm = Term.Var ct_var
     term' = replaceWithin ct_vterm con_term term
      
    explSplit :: ZTerm -> ZTerm -> m [ZTerm]
    explSplit ct_term con_term = 
      Facts.add new_fact $ do
        explore_me <- Eval.normalise term
        expl (depth - 1) explore_me
      where
      new_fact = Logic.Equal ct_term con_term

guessContext :: (Facts.Reader m, MonadUnique m, MonadPlus m) => 
  ZTerm -> m Context
guessContext term = do
  potentials <- explore term
  guard (not $ null potentials)
  let (p:ps) = potentials
  if all (alphaEq p) ps
  then return (Context.new (const p) empty)
  else do
    (context, gap_type) <- 
      liftMaybe (matchContext potentials)
    return
      $ Context.new context gap_type
  where
  id_context = Context.identity (typeOf term)
  
  matchContext :: [ZTerm] -> Maybe (ZTerm -> ZTerm, ZType)
  matchContext terms = do
    guard (Var.isConstructorTerm fst_con)
    guard (all (== fst_con) other_cons)
    assert (not $ null gap_is) $ return ()
    guard (length gap_is == 1)
    return $
      case matchContext gaps of
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
    context fill = Term.unflattenApp 
      $ fst_con:(setAt gap_i fill (head args))

falsify :: forall m . (MonadUnique m, Facts.Reader m) 
  => ZClause -> m (Maybe ZCounterExample)
falsify cls = do
  cls' <- Term.reannotate cls
  check maxDepth [] cls'
  where
  check :: Int -> [ZVar] -> ZClause -> m (Maybe ZCounterExample)
  check 0 [] _ = return Nothing
  check depth [] cls = do
    s_vars <- strictVars cls
    check (depth - 1) s_vars cls
  check depth (split_var : other_vars) cls = do
    con_terms <- Var.caseSplit 
               $ Type.fromVar 
               $ typeOf split_var
    firstM checkCon con_terms
    where
    checkCon :: ZTerm -> m (Maybe ZCounterExample)
    checkCon con_term = do 
      reduced <- liftM reduce
        $ Eval.normalise
        $ replaceWithin (Term.Var split_var) con_term cls
      case reduced of
        ReducedTo [] -> return Nothing
        ReducedToFalse -> return success
        ReducedTo sub_clses -> do
          first <- firstM (check depth other_vars) sub_clses
          return (addSplit <$> first)
      where
      addSplit = Map.insert split_var con_term
      success = Just (addSplit mempty)

