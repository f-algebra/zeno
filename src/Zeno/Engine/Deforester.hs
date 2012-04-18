module Zeno.Engine.Deforester (
  Deforestable (..), Induct (..), DeforestT,
  deforest, goal, inducts, continue,
  setGoal, absurd, failed, failedIf,
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

import qualified Zeno.Evaluation as Eval
import qualified Zeno.Facts as Facts
import qualified Zeno.DataType as DataType
import qualified Zeno.Type as Type
import qualified Zeno.Logic as Logic
import qualified Zeno.Term as Term
import qualified Zeno.Var as Var
import qualified Zeno.Unique as Unique
import qualified Zeno.Core as Zeno
import qualified Data.Map as Map
import qualified Data.Set as Set  

type CriticalPath = [Name]

class ( Ord d, Show d
      , MonadUnique m
      , Facts.Reader m
      , WithinTraversable ZTerm d
      , TermTraversable d ZVar )
      => Deforestable d m | m -> d where
      
  start :: DeforestT d m ZTerm
  induct :: ZTerm -> DeforestT d m ZTerm
  finish :: DeforestT d m ZTerm
  simplify :: ZTerm -> m (Maybe ZTerm)

deforest :: Deforestable d m => d -> m (Maybe ZTerm)
deforest goal = do
  unis <- Unique.getStream
  out_res <- runDeforestT startUnfolding env unis
  case out_res of 
    Absurdity -> absurdErr
    Failure -> return Nothing
    Success (res, unis') -> do
      Unique.putStream unis'
      return (Just res)
  where
  env = Env { goal = goal
            , inducts = mempty
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
              , usedPaths :: !(Set CriticalPath)
              , nextStep :: DeforestT d m ZTerm }
              
instance TermTraversable d ZVar => TermTraversable (Env d m) ZVar where
  mapTermsM f env = do
    goal' <- mapTermsM f (goal env)
    inducts' <- mapM (mapTermsM f) (inducts env)
    return 
      $ env { goal = goal'
            , inducts = inducts' }

  mapTerms f env =
    env { goal = mapTerms f (goal env)
        , inducts = map (mapTerms f) (inducts env) }
        
  termList env = 
    termList (goal env)
    ++ concatMap termList (inducts env)
              
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
normaliseEnv cont = do
  env <- ask
  env' <- Eval.normalise env
  facts <- Facts.ask
  facts' <- Eval.normalise facts
  local (const env') 
    $ Facts.local (const facts') 
    $ cont
  
setGoal :: Deforestable d m => d -> DeforestT d m a -> DeforestT d m a
setGoal d = local $ \e -> e { goal = d } 

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

data CriticalStep
  = SplitStep ZTerm
  | InductStep ZVar CriticalPath
  | Generalise ZTerm

takeCriticalStep :: Deforestable d m => DeforestT d m (Maybe CriticalStep)
takeCriticalStep = do
  terms <- asks (termList . goal)
  firstM (runMaybeT . criticalStep) terms

startUnfolding :: Deforestable d m => DeforestT d m ZTerm
startUnfolding = do
  has_crit <- isJust <$> takeCriticalStep
  if not has_crit
  then finish
  else setNextStep (setInduct doCriticalStep) start
  where
  setInduct = local $ \e -> e { inducts = [Induct (goal e) mempty] }

doCriticalStep :: forall d m . Deforestable d m => DeforestT d m ZTerm
doCriticalStep = do
  mby_step <- takeCriticalStep
  case mby_step of
    Just step -> doStep step
    Nothing -> finish >>= induct
  where
  mapIgnoringAbsurdities :: 
    (a -> DeforestT d m ZAlt) -> [a] -> DeforestT d m [ZAlt]
  mapIgnoringAbsurdities _ [] = return []
  mapIgnoringAbsurdities f (x:xs) = do
   mby_alt <- catchAbsurdity (f x)
   rest <- mapIgnoringAbsurdities f xs
   return $ case mby_alt of
     Nothing -> rest
     Just alt -> alt:rest
  
  doStep :: CriticalStep -> DeforestT d m ZTerm
  doStep (SplitStep term) = do
    cons <- Var.caseSplit dtype
    branches <- mapIgnoringAbsurdities doSplit cons
    return $ Term.Cse Term.SplitCase term branches
    where
    dtype = Type.fromVar (typeOf term)
    
    doSplit :: ZTerm -> DeforestT d m ZAlt
    doSplit con_term = id
      $ Facts.add (Logic.Equal term con_term) 
      $ normaliseEnv
      $ Term.Alt con vars `liftM` doCriticalStep
      where
      (con:vars) = map Term.fromVar (Term.flattenApp con_term)
      
  doStep (Generalise gen_term) = do
    new_var <- Var.generalise gen_term
    let do_gen = substitute $ Map.singleton gen_term (Term.Var new_var)
        un_gen = substitute $ Map.singleton (Term.Var new_var) gen_term
    mby_result <-
      catchFailure
      $ fmap un_gen
      $ local do_gen
      $ startUnfolding
    case mby_result of
      Nothing -> finish >>= induct
      Just result -> induct result
      
  doStep (InductStep var path) = do
    cons <- Var.caseSplit dtype
    branches <- addPath path 
      $ mapIgnoringAbsurdities doInduct cons
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
    
criticalStep :: forall d m . Deforestable d m 
  => ZTerm -> MaybeT (DeforestT d m) CriticalStep
criticalStep term
  | Term.isFixTerm term = do
    paths <- asks usedPaths
    Just unrolled <- Eval.unrollFix term
    case unrolled of
      Term.Cse (Term.FoldCase name _) _ _ 
        | Set.member [name] paths -> 
          return (Generalise term)
      unrolled ->
        criticalStep unrolled
criticalStep cse_term@(Term.Cse Term.SplitCase cse_of _)
  | Term.isFixTerm cse_of = do
    mby_simpler <- lift . lift $ simplify cse_of 
    case mby_simpler of
      Just cse_of' 
        | Var.isConstructorTerm cse_of' -> do
          normal <- Eval.normalise
                  $ cse_term { Term.caseOfTerm = cse_of' }
          criticalStep normal
      otherwise ->
        return (SplitStep cse_of)
  | otherwise = 
    return (SplitStep cse_term)
criticalStep (Term.Cse (Term.FoldCase name _) cse_of _) =
  local removeName
    $ liftM addToPath
    $ criticalStep cse_of
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

data Outcome a
  = Success a
  | Failure
  | Absurdity
  deriving ( Functor )

newtype DeforestT d m a = DeforestT { 
    runDeforestT :: Env d m -> [Unique] -> m (Outcome (a, [Unique])) }
    
absurd :: Monad m => DeforestT d m a
absurd = DeforestT $ \_ _ -> return Absurdity

failed :: Monad m => DeforestT d m a
failed = DeforestT $ \_ _ -> return Failure

failedIf :: Monad m => Bool -> DeforestT d m ()
failedIf True = failed
failedIf False = return ()

catchAbsurdity :: Monad m => DeforestT d m a -> DeforestT d m (Maybe a)
catchAbsurdity (DeforestT f) = DeforestT $ \env unis -> do
  out_x <- f env unis
  case out_x of
    Absurdity -> return $ Success $ (Nothing, unis)
    other -> return $ map (first Just) other
  
catchFailure :: Monad m => DeforestT d m a -> DeforestT d m (Maybe a)
catchFailure (DeforestT f) = DeforestT $ \env unis -> do
  out_x <- f env unis
  case out_x of
    Failure -> return $ Success $ (Nothing, unis)
    other -> return $ map (first Just) other
    
instance Monad m => Functor (DeforestT d m) where
  fmap f (DeforestT g) = 
    DeforestT $ \env unis -> liftM (map (first f)) (g env unis)
  
instance Monad m => Monad (DeforestT d m) where
  return x = DeforestT $ \_ unis -> return $ Success $ (x, unis)
  DeforestT f >>= g = DeforestT $ \env unis -> do
    out_x <- f env unis
    case out_x of
      Success (x, unis') -> runDeforestT (g x) env unis'
      Failure -> return Failure
      Absurdity -> return Absurdity
    
instance Monad m => MonadReader (Env d m) (DeforestT d m) where
  ask = DeforestT $ \env unis -> return $ Success $ (env, unis)
  local f (DeforestT g) = DeforestT $ \env unis -> g (f env) unis
  
instance Monad m => MonadUnique (DeforestT d m) where
  getStream = DeforestT $ \_ unis -> return $ Success $ (unis, unis)
  putStream unis = DeforestT $ \_ _ -> return $ Success $ ((), unis)
  
instance Deforestable d m => Facts.Reader (DeforestT d m) where
  ask = lift Facts.ask
  local f df = DeforestT $ \env unis ->
    Facts.local f $ runDeforestT df env unis
  
instance MonadTrans (DeforestT d) where
  lift m = DeforestT $ \env unis -> do
    x <- m
    return $ Success $ (x, unis)

instance Show d => Show (Induct d) where
  show (Induct ind vars) = show ind
