module Zeno.Engine.Checker (
  run, ZCounterExample
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Core ( Zeno )
import Zeno.Var ( ZTerm, ZClause, ZDataType )
import Zeno.Evaluation ( normalise )
import Zeno.Reduction ( reduce )

import qualified Zeno.DataType as DataType
import qualified Zeno.Type as Type
import qualified Zeno.Core as Zeno

type ZCounterExample = ZTermSubstitution
type Check = State ZenoState

depth :: Int
depth = 3

run :: (MonadState ZenoState m, MonadPlus m) => ZClause -> m ZCounterExample
run cls = do
  undefined
  



check :: Int -> [ZVar] -> [ZVar] -> Check (Maybe ZCounterExample)
check depth [] next_round = 
  check (depth - 1) next_round []
check depth (split_var : this_round) next_round 
  | Type.isVar var_type = check depth this_round next_round
  | otherwise = do
    undefined
  where
  var_type = typeOf split_var
  cons = DataType.constructors (Type.fromVar var_type)
