{-# LANGUAGE UndecidableInstances #-}
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
import Zeno.Unique ( Unique, MonadUnique )
import Zeno.Show

import qualified Zeno.Name as Name
import qualified Zeno.Var as Var
import qualified Zeno.Term as Term
import qualified Zeno.DataType as DataType
import qualified Data.Map as Map
import qualified Zeno.Unique as Unique

type StringMap = Map String
type ZProof = ()

type Zeno = State ZenoState

data ZenoState
  = ZenoState         { uniques :: ![Unique],
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
                        
instance MonadUnique (State ZenoState) where
  getStream = unwrapFunctor Unique.getStream
  putStream = unwrapFunctor . Unique.putStream    
  
instance MonadState ZenoState m => MonadUnique (FunctorWrapper m) where
  getStream = wrapFunctor $ 
    gets uniques
    
  putStream str = wrapFunctor $
    modify $ \s -> s { uniques = str }
  
instance Empty ZenoTheory where
  empty = ZenoTheory  { terms = mempty,
                        types = mempty,
                        props = mempty,
                        theorems = mempty }
   
instance Empty ZenoState where
  empty = ZenoState   { uniques = Unique.stream,
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
  
lookupTerm :: MonadState ZenoState m => String -> m (Maybe ZTerm)
lookupTerm name = do
  maybe_term <- gets (Map.lookup name . terms . theory)
  case maybe_term of
    Nothing -> return Nothing
    Just term -> Just `liftM` unwrapFunctor (Var.distinguishFixes term)

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

instance Show ZenoTheory where
  show thy = types_s ++ terms_s ++ conjs_s
    where
    types_s = concatMap showType 
      $ Map.elems 
      $ types thy 
      
    terms_s = concatMap showTerm
      $ filter (not . Term.isVar . snd) 
      $ Map.toList 
      $ terms thy 
      
    conjs_s = concatMap showProp
      $ Map.toList
      $ props thy
      
    showProp (name, cls) = 
      "\nprop " ++ name ++ " = " ++ show cls ++ "\n"
    
    showTerm (name, def) = 
      "\nlet " ++ name ++ " = " ++ show def ++ "\n"
    
    showType dtype =
      "\ntype " ++ show (DataType.name dtype) ++ " where" 
      ++ concatMap (("\n  " ++) . showTyped) (DataType.constructors dtype) ++ "\n"
  
