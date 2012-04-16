-- |This module 'reduce's something to the conjunction of simpler, 
-- logically equivalent things, or falsity.
--
-- It implements simple /iff/ transformations, some of which are given below. 
-- These should be read from left to right, and uses lists to represent conjunction;
-- so [] is logical truth.
--
-- Extensionality: 
-- @
--    f = g <==> [f x = g x]  (for some new x)
-- @
--
-- Alpha Equality
-- @
--    t1 = t2 <==> t1 =_\alpha t2
-- @
--
-- Constructor factoring:
-- @
--    K x1 y1 = K x2 y2 <==> [x1 = x2, y1 = y2]
--    where K is a constructor
-- @
--
-- Constructor inequality:
-- @
--    K ... = J ... <==> _|_
--    where K and J are both constructors
-- @
--
-- Antecedent contradiction
-- @
--    (_|_ ==> A) <==> []
-- @

module Zeno.Reduction (
  reduce, Reduction (..)
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Var ( ZEquation, ZClause )
import Zeno.Evaluation ( normalise )
import Zeno.Unification ( alphaEq )
import Zeno.Term ( TermTraversable (..) )

import qualified Zeno.Term as Term
import qualified Zeno.Var as Var
import qualified Zeno.Logic as Logic

-- |'Reduction a' is the logical reduction of a value of type 'a'.
-- It is either false ('ReducedToFalse') 
-- or the conjunction of a new set of values ('ReducedTo').
-- Truth is represented by 'ReducedTo []', as this is an empty conjunction.
data Reduction a = ReducedToFalse | ReducedTo [a]
  deriving ( Eq, Functor, Foldable, Traversable )
  
-- |The Reducible class specifies types which can be logically 'reduce'd.
-- This reduction will always be iff.
class Reducible a where
  reduce :: a -> Reduction a
  
-- |This is the 'Monoid' of reductions under logical conjunction. 
-- Think of 'mappend' as '(&&)' and 'mempty' as 'True'.
instance Monoid (Reduction a) where
  mempty = ReducedTo []
  
  mappend _ ReducedToFalse = ReducedToFalse
  mappend ReducedToFalse _ = ReducedToFalse
  mappend (ReducedTo xs) (ReducedTo ys) = ReducedTo (xs ++ ys)
  
instance Reducible ZClause where
  reduce (Logic.Clause antes consq) =
    case concatMap reduce antes of
      ReducedToFalse -> ReducedToFalse
      ReducedTo new_antes -> 
        case reduce consq of
          ReducedToFalse 
            | null new_antes -> ReducedToFalse
            | otherwise -> ReducedTo [Logic.Clause new_antes consq]
          ReducedTo new_consqs ->
            ReducedTo $ map (Logic.Clause new_antes) new_consqs
            
  
instance Reducible ZEquation where
  reduce (Logic.Equal t1 t2) 
    | Term.Var fun_var1 <- fun1
    , Term.Var fun_var2 <- fun2
    , Var.isConstructor fun_var1
    , Var.isConstructor fun_var2 =
      if fun_var1 == fun_var2
      then ReducedTo $ zipWith Logic.Equal args1 args2
      else ReducedToFalse
    where
    fun1 : args1 = Term.flattenApp t1
    fun2 : args2 = Term.flattenApp t2
  
  reduce (Logic.Equal t1@(Term.Lam x1 _) t2@(Term.Lam x2 _)) =
    ReducedTo 
      $ pure 
      $ (runIdentity . normalise) 
      $ Logic.Equal (Term.App t1 arg) (Term.App t2 arg)
    where 
    arg = Term.Var x1
    
  reduce (Logic.Equal t1 t2)
    | t1 `alphaEq` t2 = mempty
    
  reduce other = 
    ReducedTo [other]

  
