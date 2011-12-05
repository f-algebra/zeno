module Zeno.Engine.Deforester (
  run
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Traversing
import Zeno.Core ( Zeno, ZenoState )
import Zeno.Evaluation ( normalise )
import Zeno.Var ( ZTerm, ZVar, ZAlt )
import Zeno.Show
import Zeno.Simplifier ( Simplifier )

import qualified Zeno.Core as Zeno
import qualified Zeno.Term as Term
import qualified Data.Set as Set
import qualified Zeno.Simplifier as Simplifier

run :: ZTerm -> Simplifier ZTerm
run = deforest

deforest :: ZTerm -> Simplifier ZTerm
deforest (Term.Lam x t) = 
  Term.Lam x <$> deforest t
  
deforest term@(Term.Cse outer_fxs outer_term outer_alts)
  | Term.isCse outer_term = do
      Simplifier.simpler
      deforest new_term
  where
  new_term =
    outer_term { Term.caseOfAlts = map pushIntoAlt (Term.caseOfAlts outer_term),
                 Term.caseOfFixes = outer_fxs }
  
  inner_fxs = Term.caseOfFixes outer_term 
    
  pushIntoAlt :: ZAlt -> ZAlt
  pushIntoAlt alt = alt { Term.altTerm = new_term }
    where
    new_term = Term.Cse inner_fxs (Term.altTerm alt) outer_alts

deforest term@(Term.Cse cse_fixed cse_of cse_alts) = 
  Simplifier.fixed cse_fixed $ do
    cse_of' <- deforest cse_of
    if Term.isCse cse_of'
    then deforest $ term { Term.caseOfTerm = cse_of' } 
    else do
      cse_alts' <- mapM (deforestAlt cse_of') cse_alts
      return $ Term.Cse cse_fixed cse_of' cse_alts'
  where
  deforestAlt :: ZTerm -> ZAlt -> Simplifier ZAlt
  deforestAlt cse_of (Term.Alt con vars term) =
    Term.Alt con vars <$> deforest term'
    where
    alt_match = Term.unflattenApp . map Term.Var $ (con:vars)
    term' = normalise $ replaceWithin cse_of alt_match term

deforest app@(Term.App {}) =
  deforestApp (Term.flattenApp app)
  where
  deforestApp :: [ZTerm] -> Simplifier ZTerm
  deforestApp app@(fun@(Term.Fix fix_var fix_term) : args) = do
    already_unrolled <- Simplifier.isFixed fix_var
    if already_unrolled
    then return (Term.unflattenApp app)
    else do
      Simplifier.simpler 
      deforest normalised
    where
    unrolled_fix = replaceWithin (Term.Var fix_var) fun fix_term
    normalised = normalise $ Term.unflattenApp (unrolled_fix : args)
    free_vars = concatMap freeVars args
    
  deforestApp app = 
    return (Term.unflattenApp app)
    
deforest other = 
  return other
  
