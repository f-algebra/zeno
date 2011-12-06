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

run :: ZClause -> Zeno (Maybe ZCounterExample)
run = undefined
