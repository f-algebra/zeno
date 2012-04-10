module Zeno.Engine.Deforester (
  Deforestable (..), Induct (..), DeforestT,
  deforest, goal, inducts, facts, continue,
  usableInducts, traceEnv
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Traversing
import Zeno.Unification
import Zeno.Name ( Name )
import Zeno.Unique ( MonadUnique, Unique )
import Zeno.Var ( ZTerm, ZClause, ZDataType, ZType, ZAlt,
                  ZVar, ZTermSubstitution, ZEquation )
import Zeno.Type ( typeOf )
import Zeno.Term ( TermTraversable (..) )
import Zeno.Utils ( orderedSupersetOf )
import Zeno.Evaluation ( evaluate )

import qualified Zeno.DataType as DataType
import qualified Zeno.Type as Type
import qualified Zeno.Logic as Logic
import qualified Zeno.Term as Term
import qualified Zeno.Var as Var
import qualified Zeno.Unique as Unique
import qualified Zeno.Core as Zeno
import qualified Data.Map as Map
import qualified Data.Set as Set  

-- type DeforestT d m = ReaderT (Env d m) m
type CriticalPath = [Name]

class ( Ord d, Show d
      , MonadUnique m
      , WithinTraversable ZTerm d
      , TermTraversable d ZVar ) => Deforestable d m where
  start :: DeforestT d m ZTerm
  generalise :: (ZTerm, ZVar) -> DeforestT d m ZTerm
  finish :: DeforestT d m ZTerm

deforest :: (MonadUnique m, Deforestable d m) => d -> m ZTerm
deforest goal = do
  unis <- Unique.getStream
  (res, unis') <- (runDeforestT startUnfolding) env unis
  Unique.putStream unis'
  return res
  where
  env = Env { goal = goal
            , inducts = mempty
            , facts = mempty
            , usedPaths = mempty
            , nextStep = nextStepErr }
            
  nextStepErr = error "Tried to access Env.nextStep before it was defined"
  absurdErr = error $ "Absurdity at top level deforestation of " ++ show goal
  
data Induct d 
  = Induct    { inductGoal :: !d
              , inductVars :: !(Map ZVar ZVar) }
  
data Env d m
  = Env       { goal :: !d
              , inducts :: ![Induct d]
              , facts :: ![ZEquation]
              , usedPaths :: !(Set CriticalPath)
              , nextStep :: DeforestT d m ZTerm }
              
instance TermTraversable d ZVar => TermTraversable (Env d m) ZVar where
  mapTermsM f env = do
    goal' <- mapTermsM f (goal env)
    inducts' <- mapM (mapTermsM f) (inducts env)
    facts' <- mapM (mapTermsM f) (facts env)
    return 
      $ env { goal = goal'
            , inducts = inducts'
            , facts = facts' }

  mapTerms f env =
    env { goal = mapTerms f (goal env)
        , inducts = map (mapTerms f) (inducts env)
        , facts = map (mapTerms f) (facts env) }
        
  termList env = 
    termList (goal env)
    ++ concatMap termList (inducts env)
    ++ concatMap termList (facts env)
              
instance TermTraversable d ZVar => TermTraversable (Induct d) ZVar where
  mapTermsM f (Induct goal vars) =
    flip Induct vars `liftM` mapTermsM f goal
  mapTerms f (Induct goal vars) = 
    Induct (mapTerms f goal) vars
  termList = termList . inductGoal
  
traceEnv :: Deforestable d m => String -> DeforestT d m a -> DeforestT d m a
traceEnv msg cont = do
  d <- asks goal
  inds <- usableInducts
  trace "" 
    $ trace msg
    $ trace (show d)
    $ trace "with"
    $ trace (intercalate "\n" $ map show inds)
    $ cont
              
normaliseEnv :: Deforestable d m => DeforestT d m a -> DeforestT d m a
normaliseEnv = local $ \e -> evaluate (facts e) e
  
setGoal :: Deforestable d m => d -> DeforestT d m a -> DeforestT d m a
setGoal d = local $ \e -> e { goal = d } 
  
addFact :: Deforestable d m => ZEquation -> DeforestT d m a -> DeforestT d m a
addFact fact = local $ \e -> e { facts = fact:(facts e) }

addPath :: Deforestable d m => 
  CriticalPath -> DeforestT d m a -> DeforestT d m a
addPath path = local $ \e -> e { usedPaths = Set.insert path (usedPaths e) }

addInducts :: Deforestable d m => 
  [Induct d] -> DeforestT d m a -> DeforestT d m a
addInducts inds = local $ \e -> e { inducts = (inducts e) ++ inds }

setNextStep :: Deforestable d m => 
  DeforestT d m ZTerm -> DeforestT d m a -> DeforestT d m a
setNextStep step = local $ \e -> e { nextStep = step }

usableInducts :: Deforestable d m => DeforestT d m [Induct d]
usableInducts = asks (filter (not . Map.null . inductVars) . inducts)

continue :: Deforestable d m => DeforestT d m ZTerm
continue = join (asks nextStep)

startUnfolding :: Deforestable d m => DeforestT d m ZTerm
startUnfolding = setNextStep (setInduct doCriticalStep) 
               $ start
  where
  setInduct = local $ \e -> e { inducts = [Induct (goal e) mempty] }

data CriticalStep
  = SplitStep ZTerm
  | InductStep ZVar CriticalPath
  | Generalise ZTerm

doCriticalStep :: forall d m . Deforestable d m => DeforestT d m ZTerm
doCriticalStep = do
  terms <- asks (termList . goal)
  mby_step <- firstM (runMaybeT . criticalStep) terms
  case mby_step of
    Just step -> doStep step
    Nothing -> setNextStep startUnfolding finish
  where
  doStep :: CriticalStep -> DeforestT d m ZTerm
  doStep (SplitStep term) = do
    cons <- Var.caseSplit dtype
    branches <- mapM doSplit cons
    return $ Term.Cse Term.SplitCase term branches
    where
    dtype = Type.fromVar (typeOf term)
    
    doSplit :: ZTerm -> DeforestT d m ZAlt
    doSplit con_term = id
      $ addFact (Logic.Equal term con_term) 
      $ normaliseEnv
      $ Term.Alt con vars `liftM` doCriticalStep
      where
      (con:vars) = map Term.fromVar (Term.flattenApp con_term)
      
  doStep (Generalise gen_term) = do
    new_var <- Var.generalise gen_term
    let do_gen = substitute $ Map.singleton gen_term (Term.Var new_var)
        un_gen = substitute $ Map.singleton (Term.Var new_var) gen_term
    fmap un_gen
      $ setNextStep (setNextStep startUnfolding finish) 
      $ local do_gen
      $ generalise (gen_term, new_var)
        
  doStep (InductStep var path) = do
    cons <- Var.caseSplit dtype
    branches <- addPath path 
      $ mapM doInduct cons
    return $ Term.Cse Term.SplitCase (Term.Var var) branches
    where
    dtype = Type.fromVar (typeOf var)
    
    doInduct :: ZTerm -> DeforestT d m ZAlt
    doInduct con_term = do
      new_inds <- concatMapM generateInducts rec_vars
      addInducts new_inds 
        $ local (substitute ind_sub)
        $ normaliseEnv
        $ Term.Alt con vars `liftM` doCriticalStep
      where
      (con:vars) = map Term.fromVar (Term.flattenApp con_term)
      rec_vars = filter ((== (typeOf var)) . typeOf) vars
      ind_sub = Map.singleton (Term.Var var) con_term
        
      generateInducts :: ZVar -> DeforestT d m [Induct d]
      generateInducts rec_var = asks (map genInd . inducts)
        where
        rec_sub = Map.singleton (Term.Var var) (Term.Var rec_var)
        
        genInd :: Induct d -> Induct d
        genInd (Induct goal vars) = 
          Induct (substitute rec_sub goal) (Map.insert var rec_var vars)
    
criticalStep :: Deforestable d m => ZTerm -> MaybeT (DeforestT d m) CriticalStep
criticalStep orig_term@(Term.flattenApp -> 
    fix_term@(Term.Fix fix_var fix_rhs) : args) = do
  bgfacts <- asks facts
  paths <- asks usedPaths
  case evaled bgfacts of
    Term.Cse (Term.FoldCase name _) _ _ 
      | Set.member [name] paths -> 
        return (Generalise orig_term)
    term ->
      criticalStep term
  where
  unrolled_fix = replaceWithin (Term.Var fix_var) fix_term fix_rhs
  evaled facts = evaluate facts $ Term.unflattenApp (unrolled_fix : args)
criticalStep (Term.Cse Term.SplitCase cse_term _) =
  return (SplitStep cse_term)
criticalStep (Term.Cse (Term.FoldCase name _) cse_term _) =
  local removeName
    $ liftM addToPath
    $ criticalStep cse_term
  where
  removeName :: Env d m -> Env d m
  removeName env = env { usedPaths = Set.map remove (usedPaths env) }
    where
    remove (n:ns) | n == name = ns
    remove ns = ns
    
  addToPath :: CriticalStep -> CriticalStep
  addToPath (InductStep term path) = InductStep term (name:path)
  addToPath other = other
  
criticalStep (Term.Var var)
  | not (Var.isConstructor var)
  , Type.isVar (typeOf var) =
      return (InductStep var mempty)

criticalStep other = mzero


newtype DeforestT d m a 
  = DeforestT { runDeforestT :: Env d m -> [Unique] -> m (a, [Unique]) }
  
instance Monad m => Functor (DeforestT d m) where
  fmap f (DeforestT g) = 
    DeforestT $ \env unis -> liftM (first f) (g env unis)
  
instance Monad m => Monad (DeforestT d m) where
  return x = DeforestT $ \_ unis -> return (x, unis)
  DeforestT f >>= g = DeforestT $ \env unis -> do
    (x, unis') <- f env unis
    runDeforestT (g x) env unis'
    
instance Monad m => MonadReader (Env d m) (DeforestT d m) where
  ask = DeforestT $ \env unis -> return (env, unis)
  local f (DeforestT g) = DeforestT $ \env unis -> g (f env) unis
  
instance Monad m => MonadUnique (DeforestT d m) where
  getStream = DeforestT $ \_ unis -> return (unis, unis)
  putStream unis = DeforestT $ \_ _ -> return ((), unis)

instance Show d => Show (Induct d) where
  show (Induct ind vars) = show ind
