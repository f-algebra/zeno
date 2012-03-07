module Zeno.Engine.Deforester (
  Deforestable (..)
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Traversing
import Zeno.Unification
import Zeno.Var ( ZTerm, ZClause, ZDataType, ZType, 
                  ZVar, ZTermSubstitution, ZEquation )
import Zeno.Type ( typeOf )
import Zeno.Term ( TermTraversable (..) )

import qualified Zeno.DataType as DataType
import qualified Zeno.Type as Type
import qualified Zeno.Term as Term
import qualified Zeno.Var as Var
import qualified Zeno.Core as Zeno
import qualified Zeno.Evaluation as Eval
import qualified Data.Map as Map

type Deforest d =
  Deforestable d => ReaderT (Env d) (Effects d) ZTerm

class ( WithinTraversable ZTerm d
      , TermTraversable d ZVar
      , Monad (Effects d) ) => Deforestable d where
  type Effects d :: * -> *
  
  start :: Deforest d
  apply :: Deforest d
  generalise :: (ZTerm, ZVar) -> Deforest d
  
data Env d
  = Env       { goal :: d 
              , original :: d
              , inducts :: [d]
              , facts :: [ZEquation]
              , nextStep :: Deforest d }
              
changeGoal :: d -> Deforest d -> Deforest d
changeGoal d = local $ \e -> e { deforestee = d } 
  
deforest :: forall d . Deforestable d => d -> Effects d ZTerm
deforest goal = runReaderT start env
  where
  env = Env { goal = goal
            , original = goal
            , inducts = [] 
            , facts = []
            , nextStep = afterStart }
  
afterStart :: Deforest d
afterStart = do
  term <- asks (head . termList . goal)
  
  
  
  where
  terms = termList d
    
    
criticalPair :: ZTerm -> Maybe CriticalPair
criticalPair = runWriter . critical
  where
  critical :: ZTerm -> Writer CriticalPath ZTerm
  critical (Term.flattenApp -> fix_term@(Term.Fix fix_var fix_rhs) : args) =
    critical unrolled
    where
    unrolled_fix = replaceWithin (Term.Var fix_var) fix_term fix_rhs
    unrolled = normalise $ Term.unflattenApp (unrolled_fix : args)
  critical (Term.Cse Term.SplitCase cse_term _) =
    return cse_term
  critical (Term.Cse (Term.FoldCase name _) cse_term _) = do
    tell [name]
    critical cse_term
  critical term = 
    return term
