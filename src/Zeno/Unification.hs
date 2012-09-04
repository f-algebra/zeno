-- | The 'Unifiable' class and
-- 'alphaEq', alpha-equality defined using unification.
-- Does not need qualified import.
module Zeno.Unification (
  Unifiable (..), alphaEq
) where 

import Prelude ()
import Zeno.Prelude
import Zeno.Traversing
import Zeno.Unique ( MonadUnique )

import qualified Zeno.Unique as Unique
import qualified Zeno.Substitution as Substitution

-- | Values which can be unified.
class Ord a => Unifiable a where
  type UniVar a
  type UniTerm a
  
  -- | Returns a mapping which can be applied the first argument
  -- to return something alpha-equal to the second. 
  -- 'MonadUnique' is needed as unification usually requires 
  -- substitution, which requires fresh variables.
  -- 'MonadPlus' allows us to return 'mzero' if unification fails.
  unifier :: (MonadUnique m, MonadPlus m) => 
    a -> a -> m (Substitution.Map (UniVar a) (UniTerm a))

alphaEq :: (MonadUnique m, Unifiable a) => a -> a -> m Bool
alphaEq x y = Unique.readonly $ do
  mby_map <- runMaybeT (unifier x y)
  return 
    $ isJust mby_map
    && Substitution.null (fromJust mby_map)

