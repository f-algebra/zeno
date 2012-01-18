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
    [indx] <- findIndices containsArg flat_body, False = do 
    let context val = Term.unflattenApp $ setAt indx val flat_body
    
    return undefined
  where
  (fix_args, fix_body) = Term.flattenLam fix_term
  flat_body = Term.flattenApp fix_body
  args_set = Set.fromList fix_args
  containsArg = not . Set.null . Set.intersection args_set . Var.freeZVars
  
simplify term@(Term.Cse outer_name outer_fx outer_term outer_alts)
  | Term.isCse outer_term = do
    new_inner_alts <- mapM pushIntoAlt (Term.caseOfAlts outer_term)
    tell (Any True, mempty)
    simplify 
      $ outer_term { Term.caseOfAlts = new_inner_alts,
                     Term.caseOfFix = outer_fx }
  where
  inner_fx = Term.caseOfFix outer_term 
    
  pushIntoAlt :: MonadState ZenoState m => ZAlt -> m ZAlt
  pushIntoAlt alt = do
    cse_name <- Name.invent
    let new_term = Term.Cse cse_name inner_fx (Term.altTerm alt) outer_alts
    return $ alt { Term.altTerm = new_term }

simplify cse_term@(Term.Cse {})
  | Term.isFix fun = 
    simplify $ cse_term { Term.caseOfTerm = normalised }
  where
  (fun:args) = Term.flattenApp (Term.caseOfTerm cse_term)
  Term.Fix fix_var fix_term = fun
  unfolded_fix = replaceWithin (Term.Var fix_var) fun fix_term
  normalised = normalise $ Term.unflattenApp (unfolded_fix : args)
    
simplify cse_term@(Term.Cse cse_name cse_fix cse_of cse_alts) = 
  fixed cse_fix $ do
    cse_alts' <- mapM (simplifyAlt cse_of) cse_alts
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
  simplifyAlt :: ZTerm -> ZAlt -> Simplify ZAlt
  simplifyAlt cse_of (Term.Alt con vars term) =
    Term.Alt con vars <$> simplify term'
    where
    alt_match = Term.unflattenApp . map Term.Var $ (con:vars)
    cse_of_vars = Var.freeZVars cse_of
    term' = 
      if any (`Set.member` cse_of_vars) vars
      then term
      else normalise $ replaceWithin cse_of alt_match term
  
  unifyWithContext :: (ZTerm -> ZTerm) -> ZAlt -> Maybe ZAlt
  unifyWithContext cxt (Term.Alt con vars term) =
    case unifier (cxt empty) term of
      NoUnifier -> Nothing
      Unifier sub ->
        case Map.toList sub of
          [(key, context_gap)] | key == empty ->
            Just (Term.Alt con vars context_gap)
          _ -> 
            Nothing
    
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
  flattened <- mapM simplify (Term.flattenApp term)
  simplifyApp flattened --(Term.flattenApp term)
  where
  simplifyApp :: [ZTerm] -> Simplify ZTerm
  simplifyApp flattened@(fun@(Term.Fix fix_var fix_term) : args) 
    | Type.isVar (typeOf original_term) = do
      already_unrolled <- isFixed fix_var
      if already_unrolled
      then return original_term
      else do
        (simplified, (cse_float_occurred, _)) <- id
          $ censor (first (const mempty))
          $ listen 
          $ simplify normalised
        if not $ getAny cse_float_occurred
        then return original_term
        else do
          state <- get
          new_var <- Var.declare var_name fun_type Var.Bound
          let new_term = Term.unflattenApp $ map Term.Var (new_var : free_vars)
              (term', state', proofs) =
                runRWS (deforest simplified) (makeDFEnv new_term) state
              new_fix_term = updateCaseFixes new_var term'
          new_fix <- simplify 
                   $ Term.Fix new_var (Term.unflattenLam free_vars new_fix_term)
          if new_fix `alphaEq` fun
          then return original_term
          else do
            let new_term = Term.unflattenApp (new_fix : map Term.Var free_vars)
                prove_me = Logic.Clause [] (Logic.Equal original_term new_term)
            tell (mempty, proofs ++ [prove_me])
            put state'
            return new_term
    where
    original_term = Term.unflattenApp flattened
    unrolled = replaceWithin (Term.Var fix_var) fun fix_term
    normalised = normalise $ Term.unflattenApp (unrolled : args)
    free_vars_set = Var.freeZVars original_term
    free_vars = toList free_vars_set
    fun_type = Type.unflatten $ (map typeOf free_vars) ++ [typeOf original_term]
    var_name = "[" ++ (intercalate " " . map show) free_vars 
      ++ " -> " ++ show original_term ++ "]"
    
    makeDFEnv new_term = DFEnv
      { dfFixVar = fix_var,
        dfRewrites = [(free_vars_set, Logic.Equal original_term new_term)] }
        
    updateCaseFixes new_var term = up term
      where
      up (Term.Lam x t) = Term.Lam x (up t)
      up (Term.App l r) = Term.App (up l) (up r)
      up (Term.Cse n _ t ts) = Term.Cse n (Just new_var) (up t) (map upA ts)
        where
        upA (Term.Alt c v t) = Term.Alt c v (up t)
      up other = other
    

  simplifyApp app = 
    return (Term.unflattenApp app)
    
simplify other = 
  return other

  
type Deforest = RWS DFEnv [ZClause] ZenoState

data DFEnv 
  = DFEnv     { dfFixVar :: !ZVar,
                dfRewrites :: ![(Set ZVar, ZEquation)] }
                
caseSplit :: ZVar -> [ZVar] -> Deforest a -> Deforest a
caseSplit split_var con_args = local $ \env ->
  env { dfRewrites = concatMap split (dfRewrites env) }
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
      from' = replace split_var new_var from
      to' = replace split_var new_var to

attemptRewrite :: ZTerm -> Deforest ZTerm
attemptRewrite term 
  | not (Type.isVar (typeOf term)) = return term
  | otherwise = do
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
      if from == term
      then return to
      else do
        invented_func <- Inventor.run [from] term
        let guessed_term = Term.App invented_func from
            prove_me = Logic.Equal guessed_term term
        tell $ pure $ Logic.Clause mempty prove_me
        return 
          $ normalise 
          $ Term.App invented_func to
      
deforest :: ZTerm -> Deforest ZTerm
deforest (Term.Lam x t) = 
  Term.Lam x <$> deforest t 
  
deforest top_term@(Term.Cse cse_id fxs cse_term alts) = do 
  cse_term' <- deforest cse_term
  alts' <- mapM deforestAlt alts
  let new_cse = Term.Cse cse_id fxs cse_term' alts'
  old_fix <- asks dfFixVar
  if old_fix `elem` Term.IgnoreAnnotations new_cse
  then attemptRewrite new_cse
  else return new_cse
  where
  deforestAlt (Term.Alt con vars alt_term) =
    maybeSplit $ Term.Alt con vars <$> deforest alt_term 
    where
    maybeSplit
      | Term.isVar cse_term = caseSplit (Term.fromVar cse_term) vars
      | otherwise = id
      
deforest app@(Term.App fun arg) = do 
  new_app <- Term.App <$> deforest fun <*> deforest arg
  old_fix <- asks dfFixVar
  if old_fix `elem` Term.IgnoreAnnotations new_app
  then attemptRewrite new_app
  else return new_app

deforest other = 
  return other
  
