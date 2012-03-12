module Zeno.Engine.Deforester (
  Deforestable (..)
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Traversing
import Zeno.Unification
import Zeno.Name ( Name )
import Zeno.Var ( ZTerm, ZClause, ZDataType, ZType, 
                  ZVar, ZTermSubstitution, ZEquation )
import Zeno.Type ( typeOf )
import Zeno.Term ( TermTraversable (..) )
import Zeno.Utils ( orderedSupersetOf )
import Zeno.Evaluation ( evaluate )

import qualified Zeno.DataType as DataType
import qualified Zeno.Type as Type
import qualified Zeno.Term as Term
import qualified Zeno.Var as Var
import qualified Zeno.Core as Zeno
import qualified Data.Map as Map
import qualified Data.Set as Set

type DeforestT d m = ReaderT (Env d m) m
type CriticalPath = [Name]

class ( WithinTraversable ZTerm d
      , TermTraversable d ZVar
      , Monad m ) => Deforestable d m | d -> m where
      
  start :: DeforestT d m ZTerm
  apply :: DeforestT d m ZTerm
  generalise :: (ZTerm, ZVar) -> DeforestT d m ZTerm
  
  
data Env d m
  = Env       { goal :: !d 
              , original :: !d
              , inducts :: ![d]
              , facts :: ![ZEquation]
              , usedPaths :: !(Set CriticalPath)
              , nextStep :: !(DeforestT d m ZTerm) }
              
changeGoal :: Deforestable d m => d -> DeforestT d m a -> DeforestT d m a
changeGoal d = local $ \e -> e { goal = d } 
  
deforest :: Deforestable d m => d -> m ZTerm
deforest goal = runReaderT start env
  where
  env = Env { goal = goal
            , original = goal
            , inducts = mempty
            , facts = mempty
            , usedPaths = mempty
            , nextStep = afterStart }
            
  
afterStart :: Deforestable d m => DeforestT d m ZTerm
afterStart = do
  term <- asks (head . termList . goal)
  undefined
  
data CriticalStep
  = Split ZTerm
  | Induct ZTerm CriticalPath
  | Generalise ZTerm
    
criticalStep :: Deforestable d m => ZTerm -> DeforestT d m CriticalStep
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
  return (Split cse_term)
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
  addToPath (Induct term path) = Induct term (name:path)
  addToPath other = other
  
criticalStep term = 
  return (Induct term mempty)

