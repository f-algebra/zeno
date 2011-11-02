module Zeno.Parsing.Z (
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Utils
import Zeno.Core

import Zeno.Parsing.Lisp ( Lisp )
import qualified Zeno.Parsing.Lisp as Lisp

import qualified Data.Set as Set
import qualified Data.Map as Map

parse :: String -> State ZTheory ()
parse text = runReaderT (parseTheory (Lisp.parse text)) mempty 

type Parser = ReaderT (Map String TypeVar) (State ZTheory)

newTypeVars :: String -> Parser


newTypeVar :: MonadState ParserState m => String -> m TypeVar
newTypeVar name = do
  new_id <- newIdS
  let tvar = TypeVar new_id (Just name)
  addTypeVar tvar
  return tvar
  where
  addTypeVar :: TypeVar -> m ()
  addTypeVar tvar = modify $ \s -> 
    s { parsing_tvars = Map.insert name tvar (parsing_tvars s) }
    where Just name = typevarName tvar
    
lookupTypeVar :: MonadState ParserState m => String -> m (Maybe TypeVar)
lookupTypeVar name = gets (Map.lookup name . parsing_tvars) 

knownTypeVars :: MonadState ParserState m => m (Set TypeVar)
knownTypeVars = gets (Set.fromList . Map.elems . parsing_tvars)

fl_theory :: FromLisp ()
fl_theory (LL decls) = mapM_ fl_toplevel decls

fl_toplevel :: FromLisp ()
fl_toplevel (LL ((LN "defdata"):type_def:l_cons)) = localDefs $ do
  new_id <- newIdS
  new_tvars <- mapM newTypeVar type_args 
  rec let new_dtype = DataType new_id type_name cons
      let res_type = AppType new_dtype (map VarType new_tvars)
      addDataType new_dtype
      cons <- mapM (fl_con res_type) l_cons
  return ()
  where
  (type_name, type_args) = 
    case type_def of
      LN name        -> (name, [])
      LL (name:args) -> (fromLN name, map fromLN args)
      
  fl_con :: EInnerType -> FromLisp EVar
  fl_con res_type l_con = do
    args <- mapM fl_innerType l_args
    
    let arg_vars = concatMap typeVars args
        poly_vars = typeVars res_type
        valid_vars = Set.null (arg_vars `Set.difference` poly_vars)
    when (not valid_vars) 
      $ fail "Undeclared polymorphic variables in datatype definition."
      
    con_id <- newIdS
    let con_itype = foldFunType (args ++ [res_type])
        con_type = Type poly_vars con_itype
        new_con = EVar con_id (Just con_name) con_type ConstructorVar
    addTerm con_name (Var new_con)
    return new_con    
    where
    (con_name, l_args) = case l_con of
      LN con_name -> (con_name, [])
      LL ((LN con_name):l_args) -> (con_name, l_args)
      
   
fl_type :: FromLisp EType 
fl_type l_inner_type = do
  inner_type <- fl_innerType l_inner_type
  known_vars <- knownTypeVars
  let free_vars = typeVars inner_type `Set.difference` known_vars
  return (Type free_vars inner_type)

  
fl_innerType :: FromLisp EInnerType
fl_innerType (LN name) = do
  dt_lookup <- lookupDataType name
  tv_lookup <- lookupTypeVar name
  case dt_lookup of
    Just dtype -> return (AppType dtype [])
    Nothing -> case tv_lookup of
      Just tvar -> return (VarType tvar)
      Nothing -> VarType <$> newTypeVar name
  
fl_innerType (LL ((LN "->"):l_types)) = do
  types <- mapM fl_innerType l_types
  return (foldFunType types)
  
fl_innerType (LL ((LN dtype_name):l_args)) = do
  types <- mapM fl_innerType l_args
  Just dtype <- lookupDataType dtype_name
  return (AppType dtype types)
  
fl_term :: FromLisp (ETerm, EType)
fl_term (LN name) = do
  term <- fromJust <$> lookupTerm name
  return (term, typeOf term)
fl_term (LL [(LN "lam"), 
             (LL [LN var_name, l_var_type]), l_term]) = localDefs $ do
  var_itype <- fl_innerType l_var_type
  
  
  
  
