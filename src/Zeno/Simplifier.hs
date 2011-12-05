module Zeno.Simplifier (
  Simplifier, 
  run, traverse,
  fixed, simpler, isFixed
) where

import Prelude ()
import Zeno.Prelude hiding ( traverse )
import Zeno.Core ( Zeno, ZenoState )
import Zeno.Var ( ZVar, ZTerm )
import Zeno.Evaluation ( normalise )
import Zeno.Traversing
import Zeno.Show

import qualified Zeno.Term as Term
import qualified Data.Set as Set

type Simplifier = RWS (Set ZVar) Any ZenoState

traverse :: (ZTerm -> Simplifier ZTerm) -> ZTerm -> Simplifier ZTerm 
traverse simplify = visit . normalise
  where
  travel visited = do
    (term, any) <- listen visited
    if getAny any
    then visit (normalise term)
    else do
      (term', any) <- listen (simplify term)
      if getAny any
      then visit (normalise term')
      else return term'
 
  visit cse@(Term.Cse cse_fxs cse_of cse_alts) = 
    fixed cse_fxs 
      $ travel $ Term.Cse cse_fxs <$> visit cse_of <*> mapM visitAlt cse_alts
    where
    visitAlt (Term.Alt con vars term) =
      Term.Alt con vars <$> visit term
  visit (Term.Lam var term) =
    travel $ Term.Lam var <$> visit term
  visit (Term.App fun arg) =
    travel $ Term.App <$> visit fun <*> visit arg
  visit other = 
    travel $ return other   
  

run :: MonadState ZenoState m => Simplifier a -> m (Maybe a)
run simp = do
  state <- get
  let (term', state', any) = runRWS simp mempty state
  put state'
  if getAny any
  then return (Just term')
  else return Nothing
  
fixed :: [ZVar] -> Simplifier a -> Simplifier a
fixed = local . Set.union . Set.fromList

simpler :: Simplifier ()
simpler = tell (Any True)

isFixed :: ZVar -> Simplifier Bool
isFixed var = asks (Set.member var)
