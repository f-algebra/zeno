module Zeno.Core (
  Zeno, ZenoState (..), ZenoTheory (..),
  ZProofStep, ZCounterExample,
  initialState, emptyTheory,
  defineType, defineTerm, defineProp,
  lookupTerm, lookupType,
  println, flush
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

type StringMap = Map String

type ZProofStep = String
type ZProof = String
type ZCounterExample = ZTermSubstitution

data ZenoState
  = ZenoState         { uniqueGen :: !Unique,
                        theory :: !ZenoTheory,
                        output :: ![String] }

data ZenoTheory 
  = ZenoTheory        { terms :: !(StringMap ZTerm),
                        types :: !(StringMap ZDataType),
                        props :: !(StringMap ZClause),
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
  = ZenoTheory  { terms = mempty,
                  types = mempty,
                  props = mempty,
                  theorems = mempty }
   
initialState :: ZenoState
initialState 
  = ZenoState   { uniqueGen = mempty,
                  theory = emptyTheory,
                  output = mempty }
                  
modifyTheory :: MonadState ZenoState m => (ZenoTheory -> ZenoTheory) -> m ()
modifyTheory f = modify $ \zs -> zs { theory = f (theory zs) }

defineType :: MonadState ZenoState m => ZDataType -> m ()
defineType dtype = modifyTheory $ \z -> z 
  { types = Map.insert (show . DataType.name $ dtype) dtype (types z) }
      
defineTerm :: MonadState ZenoState m => String -> ZTerm -> m ()
defineTerm name expr = modifyTheory $ \z -> z
  { terms = Map.insert name expr (terms z) }
  
defineProp :: MonadState ZenoState m => String -> ZClause -> m ()
defineProp name cls = modifyTheory $ \z -> z
  { props = Map.insert name cls (props z) }
  
lookupTerm :: MonadState ZenoState m => String -> m (Maybe ZTerm)
lookupTerm name = gets (Map.lookup name . terms . theory)

lookupType :: MonadState ZenoState m => String -> m (Maybe ZDataType)
lookupType name = gets (Map.lookup name . types . theory)

println :: MonadState ZenoState m => String -> m ()
println text = modify $ \z -> z { output = output z ++ [text] }

flush :: MonadState ZenoState m => m [String]
flush = do
  out <- gets output
  modify $ \z -> z { output = mempty }
  return out
  
  
newtype Zeno a
  = Zeno              { runZeno :: ZenoState -> Either String (a, ZenoState) }

?? OR ?? 

type Zeno = ErrorT (State ZenoState)

?? OR MAKE A CPS VERSION OF THE FIRST ONE ??
http://www.haskell.org/haskellwiki/Performance/Monads

