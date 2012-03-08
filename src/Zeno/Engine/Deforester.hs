module Zeno.Engine.Deforester (
  Deforestable (..)
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Traversing
import Zeno.Unification
import Zeno.Var ( ZTerm, ZClause, ZDataType, ZType, 
                  ZVar, ZTermSubstitution, ZEquation,
                  CriticalPath, CriticalPair )
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

type Deforest d m = ReaderT (Env d m) m

class ( WithinTraversable ZTerm d
      , TermTraversable d ZVar
      , Monad m ) => Deforestable d m | d -> m where
      
  start :: Deforest d m ZTerm
  apply :: Deforest d m ZTerm
  generalise :: (ZTerm, ZVar) -> Deforest d m ZTerm
  
  
data Env d m
  = Env       { goal :: !d 
              , original :: !d
              , inducts :: ![d]
              , facts :: ![ZEquation]
              , usedPaths :: !(Set CriticalPath)
              , nextStep :: !(Deforest d m ZTerm) }
              
changeGoal :: Deforestable d m => d -> Deforest d m a -> Deforest d m a
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
            
  
afterStart :: Deforestable d m => Deforest d m ZTerm
afterStart = do
  term <- asks (head . termList . goal)
  undefined
  
    
criticalPair :: forall d m .
  Deforestable d m => ZTerm -> Deforest d m (Either CriticalPair ZTerm)
criticalPair term = do
  paths <- asks usedPaths
  cpair@(cterm, cpath) <- lift $ runWriterT (critical term)
  guard (all (not . flip orderedSupersetOf cpath) paths)
  return cpair
  where
  critical :: [ZEquation] -> Set CriticalPath -> ZTerm -> Writer CriticalPath ZTerm
  critical (Term.flattenApp -> fix_term@(Term.Fix fix_var fix_rhs) : args) = do
    bgfacts <- asks facts
    critical $ evaluate bgfacts 
             $ Term.unflattenApp (unrolled_fix : args)
    where
    unrolled_fix = replaceWithin (Term.Var fix_var) fix_term fix_rhs
  critical (Term.Cse Term.SplitCase cse_term _) =
    return cse_term
  critical (Term.Cse (Term.FoldCase name _) cse_term _) = do
    tell [name]
    critical cse_term
  critical term = 
    return term

