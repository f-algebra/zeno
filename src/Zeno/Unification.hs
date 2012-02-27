module Zeno.Unification (
  Unifiable (..), Unification (..), 
  applyUnification, mergeUnifiers, 
  allUnifiers, unifyMany, alphaEq
) where 

import Prelude ()
import Zeno.Prelude
import Zeno.Utils
import Zeno.Traversing

import qualified Data.Map as Map

-- |The result of trying to unify two values where
-- 'NoUnifier' indicates that unification was impossible,
-- This is just 'Maybe (Substitution n a)'.
data Unification n a 
  = Unifier !(Substitution n a)
  | NoUnifier

-- |Values which can be unified
class Ord a => Unifiable a where
  type UniVar a
  type UniTerm a
  
  unifier :: a -> a -> Unification (UniVar a) (UniTerm a)
  applyUnifier :: Substitution (UniVar a) (UniTerm a) -> a -> a
  
  unify :: a -> a -> Maybe a
  unify x y = 
    case unifier x y of
      NoUnifier -> Nothing
      Unifier sub -> 
        let x' = applyUnifier sub x
            y' = applyUnifier sub y
        in assert (x' == y') (Just x')
  
-- |Appending two unifiers will create a unifier that will
-- perform both unifications, if such a unifier is still valid.
instance (Ord a, Eq b) => Monoid (Unification a b) where
  mempty = Unifier mempty

  mappend NoUnifier _ = NoUnifier
  mappend _ NoUnifier = NoUnifier
  mappend (Unifier left) (Unifier right)
    | and (Map.elems inter) = Unifier (Map.union left right)
    | otherwise = NoUnifier
    where inter = Map.intersectionWith (==) left right
 
-- |This is like 'catMaybes'
mergeUnifiers :: [Unification a b] -> [Substitution a b]
mergeUnifiers = foldl' addUni []
  where addUni subs NoUnifier = subs
        addUni subs (Unifier sub) = sub : subs
        
applyUnification :: (WithinTraversable a f, Ord a) =>
  Unification a a -> f -> f
applyUnification NoUnifier = id
applyUnification (Unifier sub) = substitute sub

allUnifiers :: (Unifiable a, WithinTraversable a f, Eq a) => 
  a -> f -> [Substitution (UniVar a) (UniTerm a)]
allUnifiers from = mergeUnifiers . foldWithin (return . unifier from)

alphaEq :: Unifiable a => a -> a -> Bool
alphaEq x y = 
  case unifier x y of
    Unifier sub -> Map.null sub
    _ -> False

unifyMany :: Unifiable a => [a] -> Maybe a
unifyMany [] = Nothing
unifyMany xs = foldl1 unifold (map Just xs)
  where 
  unifold mx my = do
    x <- mx; y <- my
    unify x y
