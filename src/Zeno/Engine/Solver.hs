module Zeno.Engine.Solver (
  
) where


run :: (MonadState ZenoState m, MonadPlus m) => ZClause -> m ZProof


type Solve = 
