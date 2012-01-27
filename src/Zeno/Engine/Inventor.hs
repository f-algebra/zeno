module Zeno.Engine.Inventor (
  run, fill
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Traversing
import Zeno.Unification
import Zeno.Core ( Zeno, ZenoState )
import Zeno.Evaluation ( normalise, criticalPair )
import Zeno.Var ( ZTerm, ZVar, ZAlt, ZType, ZEquation )
import Zeno.Type ( typeOf )
import Zeno.Show
 
import qualified Zeno.Name as Name
import qualified Zeno.Var as Var
import qualified Zeno.Core as Zeno
import qualified Zeno.Term as Term
import qualified Zeno.Type as Type
import qualified Zeno.Logic as Logic
import qualified Zeno.Engine.Checker as Checker

import qualified Data.Set as Set
import qualified Data.Map as Map


run :: (MonadState ZenoState m, MonadPlus m) => 
  ZVar -> ZTerm -> ZTerm -> m ZTerm
run = undefined
  

type Fill = MaybeT (RWS [ZEquation] () ZenoState)

applyRewrites :: ZTerm -> Fill ZTerm
applyRewrites term = do
  rewrites <- ask
  let sub = Map.unions $ map Logic.rewriteL2R rewrites
  return $ normalise $ substitute sub term
  
splitRewrites :: ZVar -> ZTerm -> Fill a -> Fill a
splitRewrites var con_term = local (concatMap split)
  where
  rec_vars = map Term.fromVar (Var.recursiveArguments con_term)
  
  split :: ZEquation -> [ZEquation]
  split eq
    | not (var `elem` Var.freeZVars eq) = [eq]
    | otherwise = map update rec_vars
    where
    update rec_var = replaceWithin (Term.Var var) (Term.Var rec_var) eq 
    
    
fill :: (MonadState ZenoState m, MonadPlus m) =>
  (ZTerm -> ZTerm, ZType) -> ZTerm -> m ZTerm
fill (context, fill_type) desired_value = do
  fix_var <- Var.invent fun_type Var.Bound
  let rewrite_term = Term.unflattenApp (map Term.Var (fix_var : free_vars))
      rewrite = Logic.Equal desired_value (context rewrite_term)
  state <- get  
  let (mby_term, new_state, ()) = 
        (\rws -> runRWS rws [rewrite] state)  
        $ runMaybeT 
        $ inventor
        $ Var.makeUniversal free_vars 
        $ desired_value
  case mby_term of
    Nothing -> mzero
    Just filler -> do
      put new_state
      return
        $ (\t -> Term.unflattenApp (t : map Term.Var free_vars))  
        $ Term.Fix fix_var
        $ Term.unflattenLam free_vars
        $ Var.makeBound free_vars
        $ filler
  where
  free_vars = Set.toList $ Var.freeZVars desired_value
  arg_types = map typeOf free_vars
  fun_type = Type.unflatten $ arg_types ++ [fill_type]
  
  inventor :: ZTerm -> Fill ZTerm
  inventor term 
    | Just gap <- Var.withinContext term context = return gap
    | isNothing mby_cpair = mzero
    | not (Term.isVar cterm) = mzero
    | otherwise = do
        cons <- Var.caseSplit $ Type.fromVar $ typeOf cvar
        alts <- mapM inventCon cons
        cse_name <- Name.invent
        return $ Term.Cse cse_name Nothing cterm alts
    where
    mby_cpair = criticalPair term
    Just (cterm, cpath) = mby_cpair
    Term.Var cvar = cterm
    
    inventCon :: ZTerm -> Fill ZAlt
    inventCon con_term = 
      splitRewrites cvar con_term $ do
        rewritten <- applyRewrites reduced_term
        invented <- inventor rewritten
        return $ Term.Alt con vars invented
      where
      all_sources = Set.insert cpath (Var.allSources cvar)
      con_term' = Var.addSources all_sources con_term
      reduced_term = normalise $ replaceWithin cterm con_term' term
      (con:vars) = map Term.fromVar (Term.flattenApp con_term)
    
