module Zeno.Context (
  Context,
  new, function, argType, within
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Traversing
import Zeno.Unification
import Zeno.Var ( ZTerm, ZVar, ZType ) 

import qualified Zeno.Var as Var
import qualified Zeno.Term as Term
import qualified Data.Set as Set
import qualified Data.Map as Map

data Context
  = Context   { function :: !(ZTerm -> ZTerm),
                argType :: !ZType }
                
new :: (ZTerm -> ZTerm) -> ZType -> Context
new = Context
                
within :: ZTerm -> Context -> Maybe ZTerm
within term (Context cxt_func _) = 
  case unifier (cxt_func empty) term of
    NoUnifier -> Nothing
    Unifier sub ->
      case Map.toList sub of
        [] -> Just empty
        [(key, context_gap)] 
          | key == empty -> Just context_gap
        _ -> Nothing
        
instance Show Context where
  show (Context func typ) = show term 
    where
    term = func (Term.Var (Var.relabel lbl empty)) 
    lbl = "{" ++ show typ ++ "}"

