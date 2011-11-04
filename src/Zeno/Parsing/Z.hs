module Zeno.Parsing.Z (
  parse
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Utils
import Zeno.Core

import Zeno.Parsing.Lisp ( Lisp (..) )

import qualified Zeno.Parsing.Lisp as Lisp
import qualified Data.Set as Set
import qualified Data.Map as Map

type Parser = State ZTheory
type TermParser = ReaderT (Map String ZTerm) Parser

parse :: String -> Parser ()
parse text = parseTheory (Lisp.parse text)

runTermParser :: TermParser a -> Parser a
runTermParser = flip runReaderT mempty

lookupType :: String -> Parser ZType
lookupType name = do
  dt_lookup <- lookupDataType name
  case dt_lookup of
    Just dtype -> return (VarType dtype)
    Nothing -> error $ "Type not found: " ++ name

lookupDef :: String -> TermParser ZTerm
lookupDef name = do
  mby_local <- asks (Map.lookup name)
  case mby_local of
    Just def -> return def
    Nothing -> do
      mby_global <- lookupDefinition name
      case mby_global of
        Just def -> return def
        Nothing -> error $ "Variable not found: " ++ name
        
localDefinition :: String -> ZTerm -> TermParser a -> TermParser a
localDefinition name term = local (Map.insert name term)

transformZLisp :: Lisp -> Lisp
transformZLisp (LL (LN "lam":rest)) 
  | length rest > 2 = foldr foldLam term typed_vars
  where
  (typed_vars, term) = takeLast rest
  foldLam typed_var term = LL [LN "lam", typed_var, term]
transformZLisp (LL (LN "defun":typed_name@(LL [LN name, type_l]):rest)) = 
  LL [LN "def", LN name, fixed]
  where
  var_types = case type_l of
    LN _ -> []
    LL types -> butlast types
    
  (vars, term) = takeLast rest 
  typed_vars = zipWith (\var typ -> LL [var, typ]) vars var_types
  lambdas = transformZLisp (LL ((LN "lam":typed_vars) ++ [term]))
  fixed = LL [LN "fix", typed_name, lambdas]
transformZLisp lisp = lisp

parseTheory :: Lisp -> Parser ()
parseTheory (LL decls) = 
  mapM_ parseTopLevel . map (mapWithin transformZLisp) $ decls

parseTopLevel :: Lisp -> Parser ()
parseTopLevel (LL [LN "def", LN name, def]) = do
  term <- runTermParser (parseTerm def)
  addDefinition name term
parseTopLevel (LL ((LN "type"):(LN type_name):l_cons)) = do
  new_id <- newIdS
  rec let new_dtype = DataType new_id type_name cons
      addDataType new_dtype
      cons <- mapM (parseCon (VarType new_dtype)) l_cons
  return ()
  where
  parseCon :: ZType -> Lisp -> Parser ZVar
  parseCon res_type l_con = do
    con_id <- newIdS
    args <- mapM parseType l_args
    let con_type = unflattenFunType (args ++ [res_type])
        new_con = ZVar con_id (Just con_name) con_type ConstructorVar
    addDefinition con_name (Var new_con)
    return new_con    
    where
    (con_name, l_args) = case l_con of
      LN con_name -> (con_name, [])
      LL ((LN con_name):l_args) -> (con_name, l_args)
parseTopLevel other = 
  error $ "Top level statement not recognized: " ++ show other
      
parseType :: Lisp -> Parser ZType
parseType (LN name) = lookupType name
parseType (LL l_types) = do
  types <- mapM parseType l_types
  return (unflattenFunType types)
  
parseTerm :: Lisp -> TermParser ZTerm
parseTerm (LN name) = lookupDef name
parseTerm (LL [LN "let", LL [LN name, l_term], l_in]) = do
  term <- parseTerm l_term
  localDefinition name term (parseTerm l_in)
parseTerm (LL [LN "fix", LL [LN name, l_type], l_term]) = do 
  fix_type <- lift (parseType l_type)
  var_id <- newIdS
  let fix_var = ZVar var_id (Just name) fix_type FixedVar
  term <- localDefinition name (Var fix_var) (parseTerm l_term)
  return (Fix fix_var term)
parseTerm (LL [LN "lam", LL [LN name, l_type], l_term]) = do
  var_type <- lift (parseType l_type)
  var_id <- newIdS
  let lam_var = ZVar var_id (Just name) var_type (UniversalVar mempty)
  term <- localDefinition name (Var lam_var) (parseTerm l_term)
  return (Lam lam_var term)
parseTerm (LL (LN "case":l_term:l_alts)) = do
  cse_id <- newIdS
  term <- parseTerm l_term
  alts <- mapM parseAlt l_alts
  return (Cse cse_id term alts)
  where
  parseAlt (LL [l_pattern, l_term]) = do
    Var con_var <- lookupDef con_name
    let (var_types, con_type) = varType con_var |> flattenFunType |> takeLast 
    vars <- zipWithM makeVar var_names var_types
    term <- foldr localVar (parseTerm l_term) (var_names `zip` vars)
    return (Alt con_var vars term)
    where
    (con_name, var_names) = case l_pattern of
      LN name -> (name, [])
      LL (LN name:names_l) -> (name, map Lisp.fromLN names_l)
    
    makeVar :: String -> ZType -> TermParser ZVar
    makeVar var_name var_type = do
      var_id <- newIdS
      return $ ZVar var_id (Just var_name) var_type (UniversalVar mempty)
      
    localVar :: (String, ZVar) -> TermParser a -> TermParser a
    localVar (name, var) = localDefinition name (Var var)
  
parseTerm (LL l_terms) = 
  unflattenApp <$> mapM parseTerm l_terms
  
