-- | The 'Unifiable' class and
-- 'alphaEq', alpha-equality defined using unification.
-- Does not need qualified import.
module Zeno.Unification (
  Unifiable (..), alphaEq
) where 

import Prelude ()
import Zeno.Prelude
import Zeno.Traversing

import qualified Control.Unique as Unique
import qualified Control.Failure as Fail
import qualified Zeno.Substitution as Substitution

-- | Values which can be unified.
class Ord a => Unifiable a where
  type UniVar a
  type UniTerm a
  
  -- | Returns a mapping which can be applied the first argument
  -- to return something alpha-equal to the second.
  -- Fails if the two values cannot be unified.
  unifier :: MonadFailure m => 
    a -> a -> m (Substitution.Map (UniVar a) (UniTerm a))

alphaEq :: (Show (Substitution.Map (UniVar a) (UniTerm a)), Unifiable a) => 
  a -> a -> Bool
alphaEq x y =
  isJust mby_map
  && Substitution.null (fromJust mby_map)
  where
  mby_map = runIdentity 
    $ Fail.toMaybe (unifier x y)

