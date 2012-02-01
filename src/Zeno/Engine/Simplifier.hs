module Zeno.Engine.Simplifier (
  run
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Utils ( replace )
import Zeno.Traversing
import Zeno.Unification
import Zeno.Core ( ZenoState )
import Zeno.Evaluation ( normalise )
import Zeno.Var ( ZTerm, ZVar, ZAlt, ZEquation, ZClause ) 
import Zeno.Type ( typeOf )
import Zeno.Show

import qualified Zeno.Name as Name
import qualified Zeno.Var as Var
import qualified Zeno.Core as Zeno
import qualified Zeno.Term as Term
import qualified Zeno.Logic as Logic
import qualified Zeno.Type as Type
import qualified Zeno.Engine.Inventor as Inventor
import qualified Zeno.Engine.Checker as Checker

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid

run :: (MonadState ZenoState m, MonadPlus m) => ZTerm -> m (ZTerm, [ZClause])
run term = do
  state <- get
  let (term', state', (_, prove_these)) = runRWS (simplify term) mempty state
  if null prove_these
  then mzero
  else do
    put state'
    return (term', prove_these)

freshenAltVars :: MonadState ZenoState m => ZAlt -> m ZAlt
freshenAltVars (Term.Alt con vars term) = do
  new_vars <- mapM Var.clone vars
  let new_term = substitute (Map.fromList (zip vars new_vars)) term
  return (Term.Alt con new_vars new_term)
    

type Simplify = RWS (Set ZVar) (Any, [ZClause]) ZenoState
         
fixed :: Maybe ZVar -> Simplify a -> Simplify a
fixed Nothing = id
fixed (Just var) = local (Set.insert var)

isFixed :: ZVar -> Simplify Bool
isFixed var = asks (Set.member var)


simplify :: ZTerm -> Simplify ZTerm
simplify (Term.Lam x t) = 
  Term.Lam x <$> simplify t
  
simplify term@(Term.Fix fix_var fix_term)
  | Term.isApp fix_body,
    [indx] <- findIndices containsArg flat_body = do 
      let context val = Term.unflattenApp $ setAt indx val flat_body
          context_gap = flat_body !! indx
      return (floatContextOut context context_gap)
  where
  (fix_args, fix_body) = Term.flattenLam fix_term
  flat_body = Term.flattenApp fix_body
  args_set = Set.fromList fix_args
  containsArg = not . Set.null . Set.intersection args_set . Var.freeZVars
  applyFixArgs term = Term.unflattenApp (term : map Term.Var fix_args)
  
  floatContextOut :: (ZTerm -> ZTerm) -> ZTerm -> ZTerm
  floatContextOut context new_term = id
    $ Term.unflattenLam fix_args
    $ context
    $ applyFixArgs
    $ Term.Fix fix_var
    $ Term.unflattenLam fix_args
    $ normalise
    $ replaceWithin (Term.Var fix_var) cxt_fix_var new_term
    where
    cxt_fix_var = id
      $ Term.unflattenLam fix_args
      $ context 
      $ applyFixArgs (Term.Var fix_var)
  
simplify term@(Term.Cse outer_sort outer_term outer_alts)
  | Term.isCse outer_term = do
      new_inner_alts <- mapM pushIntoAlt (Term.caseOfAlts outer_term)
      tell (Any True, mempty)
      simplify 
        $ outer_term { Term.caseOfAlts = new_inner_alts,
                       Term.caseOfSort = inner_sort }
  where
  inner_sort = Term.caseOfSort outer_term 
    
  pushIntoAlt :: ZAlt -> Simplify ZAlt
  pushIntoAlt alt = do
    inner_sort' <- Term.freshenCaseSort inner_sort
    let new_term = Term.Cse inner_sort' (Term.altTerm alt) outer_alts
    return $ alt { Term.altTerm = new_term }
    
simplify cse_term@(Term.Cse cse_sort cse_of cse_alts) = 
  fixed (Term.caseSortFix cse_sort) $ do
    unrolled_fixes <- ask
    if Term.isFoldCase cse_sort
       && Term.isFix fun 
       && not (fix_var `elem` unrolled_fixes)
    then simplify $ cse_term { Term.caseOfTerm = normalised }
    else do
      cse_alts' <- mapM simplifyAlt cse_alts
      let new_term = cse_term { Term.caseOfAlts = cse_alts' }
      case mapMaybe context cse_alts' of
        [] -> return new_term
        cxt:rest -> do
          let maybe_unified = map (unifyWithContext cxt) cse_alts'
          if any isNothing maybe_unified
          then return new_term
          else do
            let unified = map fromJust maybe_unified
            return $ cxt $ new_term { Term.caseOfAlts = unified }
  where
  (fun:args) = Term.flattenApp cse_of 
  Term.Fix fix_var fix_term = fun
  unfolded_fix = replaceWithin (Term.Var fix_var) fun fix_term
  normalised = normalise $ Term.unflattenApp (unfolded_fix : args)
  
  simplifyAlt :: ZAlt -> Simplify ZAlt
  simplifyAlt (Term.Alt con vars term) =
    Term.Alt con vars <$> simplify term'
    where
    alt_match = Term.unflattenApp . map Term.Var $ (con:vars)
    cse_of_vars = Var.freeZVars cse_of
    term' = 
      -- Guard against an invalid substitution w.r.t. variable scope
      if any (`Set.member` cse_of_vars) vars
      then term
      else normalise $ replaceWithin cse_of alt_match term
  
  unifyWithContext :: (ZTerm -> ZTerm) -> ZAlt -> Maybe ZAlt
  unifyWithContext cxt (Term.Alt con vars term) = do
    gap <- Var.withinContext term cxt
    return (Term.Alt con vars gap)
    
  context :: ZAlt -> Maybe (ZTerm -> ZTerm)
  context (Term.Alt _ vars term)
    | not (Term.isApp term) = Nothing
    | otherwise = 
      case findIndices containsVar flat_term of
        [indx] -> 
          Just $ \gap -> Term.unflattenApp $ setAt indx gap flat_term
        _ -> 
          Nothing
    where
    flat_term = Term.flattenApp term
    vars_set = Set.fromList vars
    containsVar = not . Set.null . Set.intersection vars_set . Var.freeZVars 

simplify term@(Term.App {}) = do
  (simples, (_, proofs)) <-
    listen $ mapM simplify (Term.flattenApp term)
  let simple = Term.unflattenApp simples
  if not (null proofs)
  then simplify (normalise simple)
  else if not (Term.isFixTerm simple)
  then return simple
  else do
    more_simple <- simplifyApp simple
    mby_hnf <- runMaybeT $ do
      cxt <- Checker.guessContext more_simple
      guard (not (Checker.isConstantContext cxt))
      Inventor.fill cxt more_simple
    case mby_hnf of
      Nothing -> return more_simple
      Just hnf_term -> do
        let prop = Logic.Clause [] (Logic.Equal more_simple hnf_term)
        tell (mempty, [prop])
        return      
          $ trace (show term ++ " !> " ++ showWithDefinitions hnf_term) 
          $ hnf_term
  where
  simplifyApp :: ZTerm -> Simplify ZTerm
  simplifyApp orig_term@(Term.flattenApp -> (fun@(Term.Fix fix_var fix_term) : args)) 
    | Type.isVar (typeOf orig_term) = do
      already_unrolled <- isFixed fix_var
      if already_unrolled
      then return orig_term
      else do
        (simplified, (cse_float_occurred, inner_proofs)) <- id
    --      $ censor (const mempty)
          $ listen 
          $ simplify normalised
        if not (getAny cse_float_occurred)
        then return orig_term
        else do
          state <- get
          new_var <- Var.declare var_name fun_type Var.Bound
          let new_term = Term.unflattenApp $ map Term.Var (new_var : free_vars)
              (term', state', proofs) =
                runRWS (deforest simplified) (makeDFEnv new_term) state
          if not (new_var `elem` Var.freeZVars term') 
          then trace ("failed!\n" ++ show simplified) $ return orig_term
          else do
            new_fix <- simplify
                     $ Term.Fix new_var (Term.unflattenLam free_vars term')
            new_term <- fmap normalise
                      $ Term.reannotate
                      $ Term.unflattenApp 
                      $ new_fix : map Term.Var free_vars
            let prove_me = Logic.Clause [] (Logic.Equal orig_term new_term)
            tell (mempty, inner_proofs ++ proofs ++ [prove_me])
            put state'
            return new_term 
    where
    unrolled = replaceWithin (Term.Var fix_var) fun fix_term
    normalised = normalise $ Term.unflattenApp (unrolled : args)
    free_vars_set = Var.freeZVars orig_term
    free_vars = toList free_vars_set
    fun_type = Type.unflatten $ (map typeOf free_vars) ++ [typeOf orig_term]
    var_name = "[" ++ (intercalate " " . map show) free_vars 
      ++ " -> " ++ show orig_term ++ "]"
    
    makeDFEnv new_term = DFEnv
      { dfHasSplit = False,
        dfRewrites = [(free_vars_set, Logic.Equal orig_term new_term)] }
    
simplify other = 
  return other

  
type Deforest = RWS DFEnv [ZClause] ZenoState

data DFEnv 
  = DFEnv     { dfHasSplit :: !Bool,
                dfRewrites :: ![(Set ZVar, ZEquation)] }
                
caseSplit :: ZVar -> [ZVar] -> Deforest a -> Deforest a
caseSplit split_var con_args = local $ \env ->
  env { dfRewrites = concatMap split (dfRewrites env),
        dfHasSplit = True }
  where
  rec_args = filter ((== typeOf split_var) . typeOf) con_args
  
  split rewrite@(free_vars, Logic.Equal from to)
    | Set.member split_var free_vars = map splitOn rec_args
    | otherwise = [rewrite]
    where
    splitOn new_var = (free_vars', Logic.Equal from' to')
      where
      free_vars' = 
        Set.insert new_var
        $ Set.delete split_var 
        $ free_vars
      from' = normalise $ replaceWithin (Term.Var split_var) (Term.Var new_var) from
      to' = normalise $ replaceWithin (Term.Var split_var) (Term.Var new_var) to

attemptRewrite :: ZTerm -> Deforest ZTerm
attemptRewrite term 
  | not (Type.isVar (typeOf term)) = return term
  | otherwise = do
      has_split <- asks dfHasSplit
      if not has_split
      then return term
      else do
        rewrites <- asks dfRewrites
        first_rewrite <- concatMapM attempt rewrites
        case Monoid.getFirst first_rewrite of
          Nothing -> return term
          Just new_term -> return new_term
  where
  attempt :: (Set ZVar, ZEquation) -> Deforest (Monoid.First ZTerm)
  attempt (free_vars, Logic.Equal from to) = 
    fmap Monoid.First $ runMaybeT $ do
      guard $ free_vars `Set.isSubsetOf` Var.freeZVars term
      if from `alphaEq` term
      then return to
      else do
        let prove_me = Logic.Clause mempty $ Logic.Equal from term
        mby_cex <- runMaybeT (Checker.run prove_me)
        guard (isNothing mby_cex)
        tell [prove_me]
        return to
      
deforest :: ZTerm -> Deforest ZTerm
deforest (Term.Lam x t) = 
  Term.Lam x <$> deforest t 
  
deforest top_term@(Term.Cse cse_sort cse_term alts) = do 
  cse_term' <- deforest cse_term
  alts' <- mapM deforestAlt alts
  let new_cse = Term.Cse cse_sort cse_term' alts'
  attemptRewrite new_cse
  where
  deforestAlt (Term.Alt con vars alt_term) =
    maybeSplit $ Term.Alt con vars <$> deforest alt_term 
    where
    maybeSplit
      | Term.isVar cse_term = caseSplit (Term.fromVar cse_term) vars
      | otherwise = id
      
deforest app@(Term.App fun arg) = do 
  new_app <- Term.App <$> deforest fun <*> deforest arg
  attemptRewrite new_app

deforest other = 
  return other
  
