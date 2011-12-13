module Zeno.Engine.Simplifier (
  run
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Utils ( replace )
import Zeno.Traversing
import Zeno.Core ( ZenoState )
import Zeno.Evaluation ( normalise )
import Zeno.Var ( ZTerm, ZVar, ZAlt, ZEquation, ZClause ) 
import Zeno.Type ( typeOf )
import Zeno.Show

import qualified Zeno.Var as Var
import qualified Zeno.Core as Zeno
import qualified Zeno.Term as Term
import qualified Zeno.Logic as Logic
import qualified Zeno.Type as Type
import qualified Zeno.Engine.Inventor as Inventor

import qualified Data.Set as Set
import qualified Data.Monoid as Monoid

run :: (MonadState ZenoState m, MonadPlus m) => ZTerm -> m (ZTerm, [ZClause])
run term = do
  state <- get
  let (term', state', (any, prove_these)) = runRWS (expand term) mempty state
  if not (getAny any) 
  then mzero
  else do
    put state'
    return (term', prove_these)


type Expand = RWS (Set ZVar) (Any, [ZClause]) ZenoState
         
fixed :: Maybe ZVar -> Expand a -> Expand a
fixed Nothing = id
fixed (Just var) = local (Set.insert var)

isFixed :: ZVar -> Expand Bool
isFixed var = asks (Set.member var)
  
  
expand :: ZTerm -> Expand ZTerm
expand (Term.Lam x t) = 
  Term.Lam x <$> expand t
  
expand term@(Term.Cse outer_fx outer_term outer_alts)
  | Term.isCse outer_term = expand new_term
  where
  new_term =
    outer_term { Term.caseOfAlts = map pushIntoAlt (Term.caseOfAlts outer_term),
                 Term.caseOfFix = outer_fx }
  
  inner_fx = Term.caseOfFix outer_term 
    
  pushIntoAlt :: ZAlt -> ZAlt
  pushIntoAlt alt = alt { Term.altTerm = new_term }
    where
    new_term = Term.Cse inner_fx (Term.altTerm alt) outer_alts

expand term@(Term.Cse cse_fix cse_of cse_alts) = 
  fixed cse_fix $ do
    cse_of' <- expand cse_of
    if Term.isCse cse_of'
    then expand $ term { Term.caseOfTerm = cse_of' } 
    else do
      cse_alts' <- mapM (expandAlt cse_of') cse_alts
      return $ Term.Cse cse_fix cse_of' cse_alts'
  where
  expandAlt :: ZTerm -> ZAlt -> Expand ZAlt
  expandAlt cse_of (Term.Alt con vars term) =
    Term.Alt con vars <$> expand term'
    where
    alt_match = Term.unflattenApp . map Term.Var $ (con:vars)
    term' = normalise $ replaceWithin cse_of alt_match term

expand original_term@(Term.App {}) =
  expandApp (Term.flattenApp original_term)
  where
  expandApp :: [ZTerm] -> Expand ZTerm
  expandApp (fun@(Term.Fix fix_var fix_term) : args) 
    | Type.isVar (typeOf original_term) = do
      already_unrolled <- isFixed fix_var
      if already_unrolled
      then return original_term
      else do
        expanded <- expand normalised
        state <- get
        new_var <- Var.declare var_name fun_type Var.Bound
        let new_term = Term.unflattenApp $ map Term.Var (new_var : free_vars)
            (term', state', proofs) = 
              runRWS (deforest expanded) (makeDFEnv new_term) state
        if fix_var `elem` Term.IgnoreAnnotations term'
        then return expanded
        else do
          let new_fix_term = updateCaseFixes new_var term'
              new_fix = Term.Fix new_var (Term.unflattenLam free_vars new_fix_term)
              new_term = Term.unflattenApp (new_fix : map Term.Var free_vars)
              prove_me = Logic.Clause [] (Logic.Equal original_term new_term)
          tell (Any True, proofs ++ [prove_me])
          put state'
          return new_term
    where
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
      up (Term.Cse _ t ts) = Term.Cse (Just new_var) (up t) (map upA ts)
        where
        upA (Term.Alt c v t) = Term.Alt c v (up t)
      up other = other
    

  expandApp app = 
    return (Term.unflattenApp app)
    
expand other = 
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
  
deforest top_term@(Term.Cse fxs cse_term alts) = do 
  cse_term' <- deforest cse_term
  alts' <- mapM deforestAlt alts
  let new_cse = Term.Cse fxs cse_term' alts'
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
  
