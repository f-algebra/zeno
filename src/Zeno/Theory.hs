module Zeno.Theory (
  ZTheory (..), addDataType, addDefinition
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Var

import qualified Data.Map as Map

data ZTheory
  = ZTheory     { theoryDefinitions :: !(Map String ZExpr),
                  theoryDataTypes :: !(Map String ZDataType),
                  theoryCounter :: !Id,
                  theoryFlags :: !ZenoFlags }
                  
instance IdCounter Theory where
  newId pgm = 
    let id = nextId (theoryCounter pgm)
    in (id, pgm { theoryCounter = id })
    
  largestId = theoryCounter
  
emptyTheory :: ZenoFlags -> ZTheory
emptyTheory flags
  = Theory     { theoryDefinitions = mempty,
                 theoryDataTypes = mempty,
                 theoryCounter = mempty,
                 theoryFlags = flags }
                           
addDataType :: MonadState ZTheory m => ZDataType -> m ()
addDataType dtype = modify $ \z -> z 
  { theoryDataTypes = 
      Map.insert (dataTypeName dtype) dtype (theoryDataTypes z) }
      
addDefinition :: MonadState ZTheory m => String -> ZExpr -> m ()
addDefinition name expr = modify $ \z -> z
  { theoryDefinitions = Map.insert name expr (theoryDefinitions z) }
      
