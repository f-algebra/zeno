module Zeno.Engine.Deforester (
  Deforestable (..)
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Traversing
import Zeno.Unification
import Zeno.Var ( ZTerm, ZClause, ZDataType, ZType, 
                  ZVar, ZTermSubstitution )
import Zeno.Type ( typeOf )
import Zeno.Term ( TermTraversable (..) )

import qualified Zeno.DataType as DataType
import qualified Zeno.Type as Type
import qualified Zeno.Term as Term
import qualified Zeno.Var as Var
import qualified Zeno.Core as Zeno
import qualified Zeno.Evaluation as Eval
import qualified Data.Map as Map

class ( WithinTraversable ZTerm d
      , TermTraversable d ZVar
      , Monad (Effects d) ) => Deforestable d where
  type Effects d :: * -> *
  
  start :: d -> (d -> Effects d ZTerm) -> Effects d ZTerm
  apply :: [d] -> d -> (d -> Effects d ZTerm) -> Effects d ZTerm
  generalise :: ZTerm -> ZVar -> d -> (d -> Effects d ZTerm) -> Effects d ZTerm
  
  
deforest :: forall d . Deforestable d => d -> Effects d ZTerm
deforest = flip start afterStart
  where
  afterStart :: d -> Effects d ZTerm
  afterStart d = do
    undefined
    where
    terms = termList d
    {-
    
criticalPair :: ZTerm -> Maybe CriticalPair
criticalPair 
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
    -}
