module Zeno.Engine.Simplifier (
  run
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Utils ( replace )
import Zeno.Traversing
import Zeno.Core ( ZenoState )
import Zeno.Evaluation ( normalise )
import Zeno.Var ( ZTerm, ZVar, ZAlt, ZEquation ) 
import Zeno.Type ( typeOf )
import Zeno.Show

import qualified Zeno.Var as Var
import qualified Zeno.Core as Zeno
import qualified Zeno.Term as Term
import qualified Zeno.Clause as Clause
import qualified Zeno.Type as Type
import qualified Zeno.Engine.Inventor as Inventor

import qualified Data.Set as Set
import qualified Data.Monoid as Monoid

run :: (MonadState ZenoState m, MonadPlus m) => ZTerm -> m ZTerm
run term = do
  state <- get
  let (term', state', any) = runRWS (expand term) mempty state
  if not (getAny any) 
  then mzero
  else do
    put state'
    return term'
    

type Expand = RWS (Set ZVar) Any ZenoState

success :: Expand ()
success = tell (Any True)
         
fixed :: [ZVar] -> Expand a -> Expand a
fixed = local . Set.union . Set.fromList

isFixed :: ZVar -> Expand Bool
isFixed var = asks (Set.member var)


type Deforest = RWS DFEnv () ZenoState

data DFEnv 
  = DFEnv     { dfFixVar :: !ZVar,
                dfRewrites :: ![(Set ZVar, ZEquation)] }
                
caseSplit :: ZVar -> [ZVar] -> Deforest a -> Deforest a
caseSplit split_var con_args = local $ \env ->
  env { dfRewrites = concatMap split (dfRewrites env) }
  where
  rec_args = filter ((== typeOf split_var) . typeOf) con_args
  
  split rewrite@(free_vars, Clause.Equal from to)
    | Set.member split_var free_vars = map splitOn rec_args
    | otherwise = [rewrite]
    where
    splitOn new_var = (free_vars', Clause.Equal from' to')
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
  attempt (free_vars, Clause.Equal from to) = 
    fmap Monoid.First $ runMaybeT $ do
      guard $ free_vars `Set.isSubsetOf` Var.freeZVars term
      if from == term
      then return to
      else do
        invention <- Inventor.run [from] term
        return 
          $ normalise 
          $ Term.App invention to
      
deforest :: ZTerm -> Deforest ZTerm
deforest (Term.Lam x t) = 
  Term.Lam x <$> deforest t 
  
deforest top_term@(Term.Cse fxs cse_term alts) = do 
  cse_term' <- deforest cse_term
  alts' <- mapM deforestAlt alts
  let new_cse = Term.Cse fxs cse_term' alts'
  old_fix <- asks dfFixVar
  if old_fix `elem` new_cse
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
  if old_fix `elem` new_app
  then attemptRewrite new_app
  else return new_app

deforest other = 
  return other

  
expand :: ZTerm -> Expand ZTerm         
expand (Term.Lam x t) = 
  Term.Lam x <$> expand t
  
expand term@(Term.Cse outer_fxs outer_term outer_alts)
  | Term.isCse outer_term = expand new_term
  where
  new_term =
    outer_term { Term.caseOfAlts = map pushIntoAlt (Term.caseOfAlts outer_term),
                 Term.caseOfFixes = outer_fxs }
  
  inner_fxs = Term.caseOfFixes outer_term 
    
  pushIntoAlt :: ZAlt -> ZAlt
  pushIntoAlt alt = alt { Term.altTerm = new_term }
    where
    new_term = Term.Cse inner_fxs (Term.altTerm alt) outer_alts

expand term@(Term.Cse cse_fixed cse_of cse_alts) = 
  fixed cse_fixed $ do
    cse_of' <- expand cse_of
    if Term.isCse cse_of'
    then expand $ term { Term.caseOfTerm = cse_of' } 
    else do
      cse_alts' <- mapM (expandAlt cse_of') cse_alts
      return $ Term.Cse cse_fixed cse_of' cse_alts'
  where
  expandAlt :: ZTerm -> ZAlt -> Expand ZAlt
  expandAlt cse_of (Term.Alt con vars term) =
    Term.Alt con vars <$> expand term'
    where
    alt_match = Term.unflattenApp . map Term.Var $ (con:vars)
    term' = normalise $ replaceWithin cse_of alt_match term

expand app@(Term.App {}) =
  expandApp (Term.flattenApp app)
  where
  expandApp :: [ZTerm] -> Expand ZTerm
  expandApp (fun@(Term.Fix fix_var fix_term) : args) 
    | Type.isVar (typeOf app) = do
      already_unrolled <- isFixed fix_var
      if already_unrolled
      then return app
      else do
        expanded <- expand normalised
        state <- get
        new_var <- Var.declare var_name fun_type Var.Bound
        let new_term = Term.unflattenApp $ map Term.Var (new_var : free_vars)
            (term', state', ()) = runRWS (deforest expanded) (makeDFEnv new_term) state
        if fix_var `elem` term'
        then return expanded
        else do
          success
          put state'
          let new_fix = Term.Fix new_var (Term.unflattenLam free_vars term')
          (return . Term.unflattenApp) (new_fix : map Term.Var free_vars) 
    where
    unrolled = replaceWithin (Term.Var fix_var) fun fix_term
    normalised = normalise $ Term.unflattenApp (unrolled : args)
    free_vars_set = Var.freeZVars app
    free_vars = toList free_vars_set
    fun_type = Type.unflatten $ (map typeOf free_vars) ++ [typeOf app]
    var_name = "[" ++ (intercalate " " . map show) free_vars 
      ++ " -> " ++ show app ++ "]"
    
    makeDFEnv new_term = DFEnv
      { dfFixVar = fix_var,
        dfRewrites = [(free_vars_set, Clause.Equal app new_term)] }

  expandApp app = 
    return (Term.unflattenApp app)
    
expand other = 
  return other

  
