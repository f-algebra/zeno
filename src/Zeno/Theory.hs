module Zeno.Theory (
  ZTheory (..), emptyTheory,
  addDataType, addDefinition,
  lookupDefinition, lookupDataType
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Id
import Zeno.Var
import Zeno.Flags
import Zeno.DataType

import qualified Data.Map as Map

data ZTheory
  = ZTheory     { theoryDefinitions :: !(Map String ZTerm),
                  theoryDataTypes :: !(Map String ZDataType),
                  theoryCounter :: !Id }
                  
instance IdCounter ZTheory where
  newId pgm = 
    let id = nextId (theoryCounter pgm)
    in (id, pgm { theoryCounter = id })
    
  largestId = theoryCounter
  
emptyTheory :: ZTheory
emptyTheory
  = ZTheory    { theoryDefinitions = mempty,
                 theoryDataTypes = mempty,
                 theoryCounter = mempty }
                           
addDataType :: MonadState ZTheory m => ZDataType -> m ()
addDataType dtype = modify $ \z -> z 
  { theoryDataTypes = 
      Map.insert (dataTypeName dtype) dtype (theoryDataTypes z) }
      
addDefinition :: MonadState ZTheory m => String -> ZTerm -> m ()
addDefinition name expr = modify $ \z -> z
  { theoryDefinitions = Map.insert name expr (theoryDefinitions z) }
  
lookupDefinition :: MonadState ZTheory m => String -> m (Maybe ZTerm)
lookupDefinition name = gets (Map.lookup name . theoryDefinitions)

lookupDataType :: MonadState ZTheory m => String -> m (Maybe ZDataType)
lookupDataType name = gets (Map.lookup name . theoryDataTypes)
