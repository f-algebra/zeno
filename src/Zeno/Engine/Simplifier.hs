module Zeno.Engine.Simplifier (
  run
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Core ( Zeno )
import Zeno.Var ( ZVar, ZTerm, ZClause, ZDataType )

import qualified Zeno.Core as Zeno
import qualified Zeno.Evaluation as Eval
import qualified Zeno.Term as Term
import qualified Data.Set as Set

type Simp = ReaderT (Set ZVar) (WriterT Any Zeno)

addFixed :: ZVar -> Simp a -> Simp a
addFixed = local . Set.insert 

run :: ZTerm -> Zeno (Maybe ZTerm)
run term = do
  (term', any) <- runWriterT writer
  return $ if getAny any 
    then Just term'
    else Nothing
  where
  writer = runReaderT (simplify term) mempty
  
simplify :: ZTerm -> Simp ZTerm
simplify old@(Term.Fix var rhs) = do
  is_fixed <- asks (Set.member var)
  if is_fixed
    then return old
    else do
      let rhs' = replace (Term.Var var) old rhs
      addFixed var 
        $ simplify 
        $ Eval.normalise rhs'
simplify outer_cse@(Term.Cse outer_name outer_fixes outer_term outer_alts)
  | Term.isCse (Term.caseOf outer_cse) = do
    
  where
  pushIn :: ZAlt -> ZTerm
  pushIn (Term.Alt con vars term) = 
    outer_cse { caseOfTerm = term } 
  
