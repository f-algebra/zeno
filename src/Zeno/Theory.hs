module Zeno.Theory (
  ZTheory (..), empty,
  addDataType, addDefinition, addConjecture,
  lookupDefinition, lookupDataType
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Utils
import Zeno.Name ( UniqueGen (..), Unique )
import Zeno.Var ( ZVar, ZDataType, ZTerm, ZClause )

import qualified Zeno.DataType as DataType
import qualified Data.Map as Map

type ZProof = ()
type ZCounterExample = ()

data ZTheory
  = Theory      { definitions :: !(Map String ZTerm),
                  dataTypes :: !(Map String ZDataType),
                  conjectures :: !(Map String ZClause),
                  lemmata :: !(Map String (ZClause, ZProof)),
                  falsehoods :: !(Map String (ZClause, ZCounterExample)),
                  uniqueGen :: !Unique }

instance UniqueGen ZTheory where
  takeUnique thy = 
    let (new_uni, new_gen) = takeUnique (uniqueGen thy)
    in (new_uni, thy { uniqueGen = new_gen })
  
empty :: ZTheory
empty = Theory  { definitions = mempty,
                  dataTypes = mempty,
                  conjectures = mempty,
                  lemmata = mempty,
                  falsehoods = mempty,
                  uniqueGen = mempty }
                  
addDataType :: MonadState ZTheory m => ZDataType -> m ()
addDataType dtype = modify $ \z -> z 
  { dataTypes = Map.insert (show . DataType.name $ dtype) dtype (dataTypes z) }
      
addDefinition :: MonadState ZTheory m => String -> ZTerm -> m ()
addDefinition name expr = modify $ \z -> z
  { definitions = Map.insert name expr (definitions z) }
  
addConjecture :: MonadState ZTheory m => String -> ZClause -> m ()
addConjecture name cls = modify $ \z -> z
  { conjectures = Map.insert name cls (conjectures z) }
  
lookupDefinition :: MonadState ZTheory m => String -> m (Maybe ZTerm)
lookupDefinition name = gets (Map.lookup name . definitions)

lookupDataType :: MonadState ZTheory m => String -> m (Maybe ZDataType)
lookupDataType name = gets (Map.lookup name . dataTypes)
