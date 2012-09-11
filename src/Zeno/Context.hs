-- | Single hole 'ZTerm' contexts
module Zeno.Context (
  Context, contextTerm,
  new, fill, fillType, matches, 
  compose, identity, null, 
  innermost, unrollInnermost
) where

import Prelude ()
import Zeno.Prelude hiding ( null )
import Zeno.Traversing
import Zeno.Unification
import Zeno.Term ( TermTraversable )
import Zeno.Var ( ZTerm, ZVar, ZType ) 
import Zeno.Type ( typeOf )

import qualified Zeno.Evaluation as Eval
import qualified Zeno.Substitution as Substitution
import qualified Zeno.Var as Var
import qualified Zeno.Term as Term

import qualified Control.Failure as Fail
import qualified Data.Set as Set
import qualified Data.Map as Map

data Context
  = Context   { contextTerm :: !ZTerm,
                fillType :: !ZType }
                
gap :: ZVar
gap = Var.magic "_"

gapTerm :: ZTerm
gapTerm = Term.Var gap
     
valid :: Context -> Bool
valid (Context term typ) = 
  countOrd gap (toList term) <= 1

new :: (ZTerm -> ZTerm) -> ZType -> Context
new func typ =
  assert (valid cxt) cxt
  where
  cxt = Context (func gapTerm) typ
  
identity :: ZType -> Context
identity = new id

compose :: Context -> Context -> Context
compose left right = 
  Context (fill left (contextTerm right)) (fillType right)
  
null :: Context -> Bool
null (Context term _) = term == gapTerm 

fill :: Context -> ZTerm -> ZTerm
fill cxt filler = 
  replaceWithin gapTerm filler (contextTerm cxt) 
  
-- | Checks whether a term matches a given context and if so,
-- returns the value that is inside the hole,
-- e.g. f (g x) `matches` f _ = g x.
-- If the context does not have a hole it returns 'empty'.
-- Fails if the term does not match the context.
matches :: (MonadUnique m, MonadFailure m) => Context -> ZTerm -> m ZTerm
matches (Context cxt_term _) match_term = do
  uni_map <- unifier cxt_term match_term
  case Substitution.toList uni_map of
    [] -> return empty
    [(key, context_gap)] 
      | key == gap -> return context_gap
    _ -> 
      Fail.here
        
-- | Splits a term into an innermost function call
-- and an outermost context.
innermost :: ZTerm -> (Context, ZTerm)
innermost term
  | (left_args, fix_arg:right_args) <- break Var.isFunctionCall args =
      let (inner_cxt, inner_term) = innermost fix_arg
          cxt_func t = Term.unflattenApp 
            $ func : (left_args ++ (t : right_args))
          outer_cxt = new cxt_func (typeOf fix_arg)
      in (compose outer_cxt inner_cxt, inner_term)
  | otherwise = 
      (identity (typeOf term), term)
  where
  (func:args) = Term.flattenApp term
  
-- | Unrolls the innermost function in a term and floats the now unrolled
-- pattern matches to the top level.
unrollInnermost :: MonadUnique m => ZTerm -> m ZTerm
unrollInnermost term = do
  unrolled <- Eval.unrollFix inner_term
  Term.mapCaseBranchesM pushContextIn unrolled
  where
  (outer_cxt, inner_term) = innermost term
  pushContextIn = Eval.normalise . fill outer_cxt

  
instance TermTraversable Context ZVar where
  mapTermsM f (Context term typ) = 
    return Context `ap` f term `ap` return typ
  mapTerms f (Context term typ) =
    Context (f term) typ
  termList = 
    pure . contextTerm
        
instance Show Context where
  show context = show term 
    where
    term = fill context (Term.Var (Var.relabel lbl empty)) 
    lbl = "{" ++ show (fillType context) ++ "}"

