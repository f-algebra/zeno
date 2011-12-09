module Zeno.Engine.Checker (
  run
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Core ( Zeno )
import Zeno.Var ( ZTerm, ZClause, ZDataType )

import qualified Zeno.Core as Zeno
import qualified Zeno.Evaluation as Eval

type ZCounterExample = ZTermSubstitution

depth :: Int
depth = 3

run :: (MonadState ZenoState m, MonadPlus m) => ZClause -> m ZCounterExample
run cls = do
  


check :: 
