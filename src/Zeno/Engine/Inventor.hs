module Zeno.Engine.Inventor (
  run
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Traversing
import Zeno.Core ( Zeno, ZenoState )
import Zeno.Evaluation ( normalise )
import Zeno.Var ( ZTerm, ZVar, ZAlt )
import Zeno.Show

import qualified Zeno.Core as Zeno
import qualified Zeno.Term as Term
import qualified Data.Set as Set

run :: (MonadState ZenoState m, MonadPlus m) => 
  [ZTerm] -> ZTerm -> m ZTerm
run args result = mzero
