-- |This module contains functions which will normalise a 'ZTerm'.
module Zeno.Evaluation (
  normalise, evaluate
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Core

import qualified Zeno.Clause as Clause
import qualified Zeno.Var as Var
import qualified Zeno.Term as Term

import qualified Data.Set as Set
import qualified Data.Map as Map

type Eval a = Reader ([ZEquation], Set ZVar) a

normalise :: ZTerm -> ZTerm
normalise = evaluate []

evaluate :: [ZEquation] -> ZTerm -> ZTerm
evaluate facts = flip runReader (facts, mempty) . eval

addFixedVars :: Set ZVar -> Eval a -> Eval a
addFixedVars = local . second . Set.union

eval :: ZTerm -> Eval ZTerm
eval (Term.Var x) = return (Term.Var x)
eval (Term.Lam x t) = Term.Lam x <$> eval t
eval (Term.Fix f t) = Term.Fix f <$> eval t
eval (Term.Cse cse_name cse_fixed cse_of cse_alts) =
  addFixedVars cse_fixed $ do
    cse_of' <- applyFacts =<< eval cse_of
    cse_alts' <- mapM evalAlt cse_alts
    if not (Var.isConstructorTerm cse_of')
      then return $ Term.Cse cse_name cse_fixed cse_of' cse_alts'
      else eval $ matchAlt cse_of' cse_alts'
  where
  applyFacts :: ZTerm -> Eval ZTerm
  applyFacts term = do
    facts <- asks fst
    case find ((== term) . Clause.eqLeft) facts of
      Nothing -> return term
      Just eq -> return (Clause.eqRight eq)
  
  evalAlt alt = do
    term' <- eval (Term.altTerm alt)
    return $ alt { Term.altTerm = term' }
  
  matchAlt term alts =
    substitute sub . Term.altTerm $ match
    where
    (Term.Var con : term_args) = Term.flattenApp term
    Just match = find ((== con) . Term.altCon) alts
    bound_vars = map Term.Var . Term.altVars $ match
    sub = Map.fromList $ bound_vars `zip` term_args 

eval other = do
  flattened <- mapM eval (Term.flattenApp other)
  evalApp flattened
  where
  evalApp :: [ZTerm] -> Eval ZTerm
  evalApp (Term.Lam lam_var lam_rhs : arg : rest) = do
    lam_rhs' <- eval $ replaceWithin (Term.Var lam_var) arg lam_rhs
    evalApp (lam_rhs' : rest)
  evalApp app@(fix_t@(Term.Fix fix_var fix_rhs) : args) = do
    already_unrolled <- asks snd
    let did_nothing = return (Term.unflattenApp app)
    if fix_var `Set.member` already_unrolled
      then did_nothing
      else do
        unrolled <- eval $ replaceWithin (Term.Var fix_var) fix_t fix_rhs
        unrolled' <- evalApp (unrolled : args)
        if Term.isNormal unrolled'
          then return unrolled'
          else did_nothing
  evalApp app = 
    return (Term.unflattenApp app)
