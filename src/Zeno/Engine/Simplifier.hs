module Zeno.Engine.Simplifier (
  run
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Traversing
import Zeno.Core ( ZenoState )
import Zeno.Evaluation ( normalise )
import Zeno.Var ( ZTerm, ZVar, ZAlt )
import Zeno.Type ( typeOf )
import Zeno.Show

import qualified Zeno.Var as Var
import qualified Zeno.Core as Zeno
import qualified Zeno.Term as Term
import qualified Zeno.Type as Type
import qualified Zeno.Engine.Inventor as Inventor
import qualified Data.Set as Set

run :: MonadState ZenoState m => ZTerm -> m (Maybe ZTerm)
run term = do
  state <- get
  let (term', state', any) = runRWS (expand term) mempty state
  if not (getAny any) 
  then return Nothing
  else do
    put state'
    return (Just term')
    

type Expand = RWS (Set ZVar) Any ZenoState

success :: Expand ()
success = tell (Any True)
         
fixed :: [ZVar] -> Expand a -> Expand a
fixed = local . Set.union . Set.fromList

isFixed :: ZVar -> Expand Bool
isFixed var = asks (Set.member var)


type Deforest = RWS DFEnv () ZenoState

data DFEnv 
  = DFEnv     { dfVar :: ZVar,
                dfArgs :: Map ZVar [ZVar],
                dfRemoving :: ZVar,
                dfResult :: ZTerm }
                
possibleResults :: Deforest [ZTerm]
possibleResults = do
  args <- asks dfArgs
  result <- asks dfResult
  
                
caseSplit :: ZVar -> [ZVar] -> Deforest a -> Deforest a
caseSplit split_var con_args deforest = do
  old_args <- asks dfArgs
  let new_args = map maybeAddArgs old_args
  local (\env -> env { dfArgs = new_args }) deforest
  where
  rec_args = Set.fromList $ filter ((== typeOf split_var) . typeOf) con_args
  
  maybeAddArgs var_set
    | Set.member split_var var_set = Set.union rec_args var_set
    | otherwise = var_set


deforest :: ZTerm -> Deforest ZTerm
deforest (Term.Lam x t) = 
  Term.Lam x <$> deforest t
deforest (Term.Cse fxs term alts) = do 
  term' <- deforest term
  alts' <- mapM deforestAlt alts
  let new_cse = Term.Cse fxs term' alts'
  removing <- asks dfRemoving
  if not (removing `elem` new_cse)
  then return new_cse
  else do
    mby_invented <- Inventor.run ..?
  where
  deforestAlt (Term.Alt con vars term) = 
    Term.Alt con vars <$> deforest term

  
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
        new_var <- Var.invent fun_type Var.Bound
        let (term', state', ()) = runRWS (deforest expanded) (makeDFEnv new_var) state
        if fix_var `elem` term'
        then return expanded
        else do
          put state'
          let new_fix = Term.Fix new_var (Term.unflattenLam free_vars term')
          (return . Term.unflattenApp) (new_fix : map Term.Var free_vars) 
    where
    unrolled_fix = replaceWithin (Term.Var fix_var) fun fix_term
    normalised = normalise $ Term.unflattenApp (unrolled_fix : args)
    free_vars = toList (freeVars app :: Set ZVar)
    fun_type = Type.unflatten $ (map typeOf free_vars) ++ [typeOf app]
    
    makeDFEnv new_var = DFEnv 
      { dfVar = new_var,
        dfArgs = map Set.singleton free_vars,
        dfRemoving = fix_var,
        dfResult = app }

  expandApp app = 
    return (Term.unflattenApp app)
    
expand other = 
  return other

  
