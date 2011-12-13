module Zeno.Core (
  Zeno, ZenoState (..), ZenoTheory (..),
  ZProof,
  defineType, defineTerm, defineProp,
  lookupTerm, lookupType, lookupProp,
  print, flush
) where

import Prelude ()
import Zeno.Prelude hiding ( print )
import Zeno.Var ( ZTerm, ZClause, ZDataType,
                  ZTermSubstitution, ZVar )
import Zeno.Name ( Unique, UniqueGen (..) )

import qualified Zeno.Name as Name
import qualified Zeno.Var as Var
import qualified Zeno.DataType as DataType
import qualified Data.Map as Map

type StringMap = Map String
type ZProof = ()

type Zeno = State ZenoState

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
   
instance Empty ZenoTheory where
  empty = ZenoTheory  { terms = mempty,
                        types = mempty,
                        props = mempty,
                        theorems = mempty }
   
instance Empty ZenoState where
  empty = ZenoState   { uniqueGen = mempty,
                        theory = empty,
                        output = mempty }

modifyTheory :: MonadState ZenoState m => (ZenoTheory -> ZenoTheory) -> m ()
modifyTheory f = modify $ \zs -> zs { theory = f (theory zs) }

defineType :: MonadState ZenoState m => ZDataType -> m ()
defineType dtype = modifyTheory $ \z -> z 
  { types = Map.insert (Name.label . DataType.name $ dtype) dtype (types z) }
      
defineTerm :: MonadState ZenoState m => String -> ZTerm -> m ()
defineTerm name expr = modifyTheory $ \z -> z
  { terms = Map.insert name expr (terms z) }
  
defineProp :: MonadState ZenoState m => String -> ZClause -> m ()
defineProp name cls = modifyTheory $ \z -> z
  { props = Map.insert name cls (props z) }
  
lookupTerm :: (Functor m, MonadState ZenoState m) => String -> m (Maybe ZTerm)
lookupTerm name = do
  maybe_term <- gets (Map.lookup name . terms . theory)
  case maybe_term of
    Nothing -> return Nothing
    Just term -> Just <$> Var.distinguishFixes term

lookupType :: MonadState ZenoState m => String -> m (Maybe ZDataType)
lookupType name = gets (Map.lookup name . types . theory)

lookupProp :: MonadState ZenoState m => String -> m (Maybe ZClause)
lookupProp name = gets (Map.lookup name . props . theory)

print :: MonadState ZenoState m => String -> m ()
print text = modify $ \z -> z { output = output z ++ [text] }

flush :: MonadState ZenoState m => m [String]
flush = do
  out <- gets output
  modify $ \z -> z { output = mempty }
  return out

