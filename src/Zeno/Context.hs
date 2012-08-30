-- | Single hole 'Term' contexts
module Zeno.Context (
  Context, contextTerm,
  new, fill, fillType, matches, 
  compose, identity, null
) where

import Prelude ()
import Zeno.Prelude hiding ( null )
import Zeno.Traversing
import Zeno.Unification
import Zeno.Term ( TermTraversable )
import Zeno.Var ( ZTerm, ZVar, ZType ) 

import qualified Zeno.Var as Var
import qualified Zeno.Term as Term
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
-- returns the value that is inside the hole.
-- e.g. f (g x) `matches` f _ = g x
-- If the context is does not have a hole this returns 'Just Err'
matches :: Context -> ZTerm -> Maybe ZTerm
matches (Context cxt_term _) match_term = 
  case unifier cxt_term match_term of
    NoUnifier -> Nothing
    Unifier sub ->
      case Map.toList sub of
        [] -> Just empty
        [(key, context_gap)] 
          | key == gap -> Just context_gap
        _ -> Nothing

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

