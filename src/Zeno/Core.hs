module Zeno.Core (
  Zeno, ZenoState (..), ZenoTheory (..),
  ZProofStep, ZCounterExample,
  initialState, emptyTheory,
  defineType, defineTerm, addConjecture,
  lookupTerm, lookupType
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Var ( ZTerm, ZClause, ZDataType,
                  ZTermSubstitution )
import Zeno.Parsing.Lisp ( Lisp )
import Zeno.Name ( Unique, UniqueGen (..) )
import Zeno.ReaderWriter

import qualified Zeno.DataType as DataType
import qualified Data.Map as Map

type Zeno = State ZenoState
type StringMap = Map String

type ZProofStep = String
type ZProof = String
type ZCounterExample = ZTermSubstitution

data ZenoState
  = ZenoState         { uniqueGen :: !Unique,
                        theory :: !ZenoTheory }

data ZenoTheory 
  = ZenoTheory        { definitions :: !(StringMap ZTerm),
                        dataTypes :: !(StringMap ZDataType),
                        conjectures :: !(StringMap ZClause),
                        theorems :: !(StringMap (ZClause, ZProof)) }

data DiscoveredLemma 
  = DiscoveredLemma   { discoveredProperty :: !ZClause,
                        discoveredProof :: !ZProof,
                        discoveredReason :: !String }
                        

instance UniqueGen ZenoState where
  takeUnique zeno = 
    let (new_uni, new_gen) = takeUnique (uniqueGen zeno)
    in (new_uni, zeno { uniqueGen = new_gen })
    
emptyTheory :: ZenoTheory 
emptyTheory
  = ZenoTheory  { definitions = mempty,
                  dataTypes = mempty,
                  conjectures = mempty,
                  theorems = mempty }
   
initialState :: ZenoState
initialState 
  = ZenoState   { uniqueGen = mempty,
                  theory = emptyTheory }
                  
modifyTheory :: MonadState ZenoState m => (ZenoTheory -> ZenoTheory) -> m ()
modifyTheory f = modify $ \zs -> zs { theory = f (theory zs) }

defineType :: MonadState ZenoState m => ZDataType -> m ()
defineType dtype = modifyTheory $ \z -> z 
  { dataTypes = Map.insert (show . DataType.name $ dtype) dtype (dataTypes z) }
      
defineTerm :: MonadState ZenoState m => String -> ZTerm -> m ()
defineTerm name expr = modifyTheory $ \z -> z
  { definitions = Map.insert name expr (definitions z) }
  
addConjecture :: MonadState ZenoState m => String -> ZClause -> m ()
addConjecture name cls = modifyTheory $ \z -> z
  { conjectures = Map.insert name cls (conjectures z) }
  
lookupTerm :: MonadState ZenoState m => String -> m (Maybe ZTerm)
lookupTerm name = gets (Map.lookup name . definitions . theory)

lookupType :: MonadState ZenoState m => String -> m (Maybe ZDataType)
lookupType name = gets (Map.lookup name . dataTypes . theory)
