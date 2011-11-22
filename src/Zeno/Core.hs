module Zeno.Core (
  Zeno, ATP, ZenoState (..), ZenoTheory (..), 
  ZenoEngine (..), DiscoveredLemma (..),
  Prover, Simplifier, Disprover, Inventor, Generaliser,
  ZProofStep, ZCounterExample,
  initialState, emptyTheory, emptyEngine,
  addDataType, addDefinition, addConjecture,
  lookupDefinition, lookupDataType
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

type ATP = ReaderWriter ZenoState [DiscoveredLemma]
type Zeno = State ZenoState
type StringMap = Map String

type ZProofStep = String
type ZProof = String
type ZCounterExample = ZTermSubstitution

type Prover = ZClause -> ATP (Maybe (ZProofStep, [ZClause]))
type Simplifier = [ZClause] -> ZTerm -> ATP (Maybe ZTerm)
type Disprover = ZClause -> ATP (Maybe ZCounterExample)
type Inventor = [ZClause] -> ZTerm -> ZTerm -> ATP (Maybe ZTerm)
type Generaliser = ZClause -> ATP (Maybe ZClause)

data ZenoState
  = ZenoState         { uniqueGen :: !Unique,
                        theory :: !ZenoTheory,
                        engine :: !ZenoEngine }

data ZenoTheory 
  = ZenoTheory        { definitions :: !(StringMap ZTerm),
                        dataTypes :: !(StringMap ZDataType),
                        conjectures :: !(StringMap ZClause),
                        theorems :: !(StringMap (ZClause, ZProof)) }
                        
data ZenoEngine 
  = ZenoEngine        { provers :: !(StringMap Prover),
                        simplifiers :: !(StringMap Simplifier),
                        disprovers :: !(StringMap Disprover),
                        inventors :: !(StringMap Inventor),
                        generalisers :: !(StringMap Generaliser) }

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
                  
emptyEngine :: ZenoEngine
emptyEngine 
  = ZenoEngine  { provers = mempty,
                  simplifiers = mempty,
                  disprovers = mempty,
                  inventors = mempty,
                  generalisers = mempty }
   
initialState :: ZenoState
initialState 
  = ZenoState   { uniqueGen = mempty,
                  theory = emptyTheory,
                  engine = emptyEngine }
                  
modifyTheory :: MonadState ZenoState m => (ZenoTheory -> ZenoTheory) -> m ()
modifyTheory f = modify $ \zs -> zs { theory = f (theory zs) }

modifyEngine :: MonadState ZenoState m => (ZenoEngine -> ZenoEngine) -> m ()
modifyEngine f = modify $ \zs -> zs { engine = f (engine zs) } 

addDataType :: MonadState ZenoState m => ZDataType -> m ()
addDataType dtype = modifyTheory $ \z -> z 
  { dataTypes = Map.insert (show . DataType.name $ dtype) dtype (dataTypes z) }
      
addDefinition :: MonadState ZenoState m => String -> ZTerm -> m ()
addDefinition name expr = modifyTheory $ \z -> z
  { definitions = Map.insert name expr (definitions z) }
  
addConjecture :: MonadState ZenoState m => String -> ZClause -> m ()
addConjecture name cls = modifyTheory $ \z -> z
  { conjectures = Map.insert name cls (conjectures z) }
  
lookupDefinition :: MonadState ZenoState m => String -> m (Maybe ZTerm)
lookupDefinition name = gets (Map.lookup name . definitions . theory)

lookupDataType :: MonadState ZenoState m => String -> m (Maybe ZDataType)
lookupDataType name = gets (Map.lookup name . dataTypes . theory)
