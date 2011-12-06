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

run :: [ZTerm] -> ZTerm -> Zeno (Maybe ZTerm)
run args result = return Nothing
