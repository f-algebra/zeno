module Zeno.Engine.Simplifier (
  run
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Utils ( replace )
import Zeno.Traversing
import Zeno.Unification
import Zeno.Core ( ZenoState )
import Zeno.Var ( ZTerm, ZVar, ZAlt, ZEquation, ZClause ) 
import Zeno.Type ( typeOf )
import Zeno.Term ( TermTraversable (..) )
import Zeno.Name ( MonadUnique )
import Zeno.Context ( Context )
import Zeno.Show
import Zeno.Engine.Deforester ( Deforestable, DeforestT, Induct )

import qualified Zeno.Context as Context
import qualified Zeno.Facts as Facts
import qualified Zeno.Evaluation as Eval
import qualified Zeno.Name as Name
import qualified Zeno.Var as Var
import qualified Zeno.Core as Zeno
import qualified Zeno.Term as Term
import qualified Zeno.Logic as Logic
import qualified Zeno.Type as Type
import qualified Zeno.Engine.Deforester as Deforest
import qualified Zeno.Engine.Checker as Checker

import qualified Data.Set as Set
import qualified Data.Map as Map

run :: (MonadUnique m, Facts.Reader m) => ZTerm -> m (Maybe ZTerm)
run term = do
  liftM (map (Term.unflattenLam vars))
    $ runIdentityT
    $ Deforest.deforest startingGoal
  where
  (vars, term') = Term.flattenLam term 
  startingGoal = SimplifyGoal term' term' (Context.new id (typeOf term'))

type SimplifyT m = IdentityT m

data SimplifyGoal
  = SimplifyGoal    { simplifyMe :: !ZTerm
                    , replaceWithMe :: !ZTerm
                    , goalContext :: !Context }

instance Eq SimplifyGoal where
  (==) = (==) `on` simplifyMe

instance Ord SimplifyGoal where
  compare = compare `on` simplifyMe

instance TermTraversable SimplifyGoal ZVar where
  mapTermsM f goal = do
    simp' <- f (simplifyMe goal)
    repl' <- f (replaceWithMe goal)
    return $ goal { simplifyMe = simp', replaceWithMe = repl' }

  mapTerms f goal =
    goal { simplifyMe = f (simplifyMe goal)
         , replaceWithMe = f (replaceWithMe goal) }

  -- | While replaceWithMe can be modified it should not be accessed
  termList = 
    return . simplifyMe

modifyGoal :: Monad m => (SimplifyGoal -> SimplifyGoal) 
  -> DeforestT SimplifyGoal m a -> DeforestT SimplifyGoal m a
modifyGoal f = local $ \e -> e { Deforest.goal = f (Deforest.goal e) }

setReplaceWith :: Monad m => ZTerm 
  -> DeforestT SimplifyGoal m a -> DeforestT SimplifyGoal m a
setReplaceWith term = modifyGoal $ \g -> g { replaceWithMe = term }

setContext :: Monad m => Context
  -> DeforestT SimplifyGoal m a -> DeforestT SimplifyGoal m a
setContext cxt = modifyGoal $ \g -> g { goalContext = cxt }

recursedOnVar :: ZVar -> ZTerm -> Bool
recursedOnVar var = anyWithin splitUpon
  where
  splitUpon (Term.Cse _ (Term.Var x) _) = x == var
  splitUpon _ = False

instance (MonadUnique m, Facts.Reader m) 
      => Deforestable SimplifyGoal (SimplifyT m) where
      
  simplify = run
      
  start = do
    goal <- asks (simplifyMe . Deforest.goal)
    cxt <- Checker.guessContext goal
    let free_vars = (Set.toList . Var.freeZVars) goal
        var_types = map typeOf free_vars
        fun_type = Type.unflatten (var_types ++ [Context.argType cxt])
    new_fun <- Var.declare ("[" ++ show goal ++ "]") fun_type Var.Universal
    let new_term = Context.function cxt
                 $ Term.unflattenApp 
                 $ map Term.Var (new_fun : free_vars) 
    inner_term <- setReplaceWith new_term 
                $ setContext cxt
                $ Deforest.continue
    let (used_vars, unused_vars) = partition (flip recursedOnVar inner_term) free_vars
        removeUnusedVarCalls = concatEndos 
                             $ map (removeUnusedVarCall new_fun) unused_vars
        new_fix = Term.Fix new_fun
                $ Term.unflattenLam used_vars 
                $ removeUnusedVarCalls inner_term
    new_goal <- 
      Term.reannotate
      $ Context.function cxt
      $ Term.unflattenApp (new_fix : map Term.Var used_vars)
    Deforest.failedIf (new_goal `alphaEq` goal)
    return new_goal
    where
    removeUnusedVarCall :: ZVar -> ZVar -> ZTerm -> ZTerm
    removeUnusedVarCall fix_var unused_var = mapWithin remove
      where
      remove (Term.App fun_term (Term.Var arg_var))
        | arg_var == unused_var
        , head (Term.flattenApp fun_term) == Term.Var fix_var
        = fun_term
      remove other = other

  finish =
    asks (simplifyMe . Deforest.goal)
        
  induct result = do
    inds <- Deforest.usableInducts
    let applyRewrites = appEndo . concatMap (Endo . applyInduct) $ inds
    result' <- Eval.normalise $ applyRewrites result
    cxt <- asks (goalContext . Deforest.goal)
    case Context.within result' cxt of
      Nothing -> Deforest.failed
      Just inner_term -> return inner_term
    where
    applyInduct :: Induct SimplifyGoal -> ZTerm -> ZTerm
    applyInduct (Deforest.inductGoal -> goal) = mapWithin apply
      where
      apply term 
        | term `alphaEq` simplifyMe goal = replaceWithMe goal
        | otherwise = term
      
      
instance Show SimplifyGoal where
  show (SimplifyGoal simp repl cxt) = 
    "<" ++ show cxt ++ ">[" ++ show simp ++ " => " ++ show repl ++ "]"

