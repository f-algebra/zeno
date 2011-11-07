module Zeno.Parsing.Z (
  parse
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Utils
import Zeno.Core
import Text.Parsec ( ParsecT, runParserT )

import qualified Text.Parsec as Ps
import qualified Zeno.Name as Name
import qualified Zeno.Var as Var
import qualified Zeno.DataType as DataType
import qualified Zeno.Clause as Clause
import qualified Zeno.Term as Term
import qualified Zeno.Type as Type
import qualified Zeno.Theory as Thy

import qualified Data.Set as Set
import qualified Data.Map as Map

type Parser = ParsecT String () (ReaderT (Map String ZTerm) (State ZTheory)) 

parse :: String -> State ZTheory ()
parse = fmap handleError 
      . flip runReaderT mempty 
      . runParserT parseTheory () "zthy"
  where
  handleError (Right ()) = ()
  handleError (Left err) = error (show err)

parseTheory :: Parser ()
parseTheory = void $ many parseTypeDef
      
parseTypeDef :: Parser ()
parseTypeDef = do 
  Ps.string "type"
  label <- parseName
  Ps.char '='
  name <- lift (Name.declare label)
  cons <- parseCon (Type.Var new_dtype) `Ps.sepBy1` Ps.char '|'
  
  let pre_dtype = DataType.DataType name []
  
  rec let new_dtype = DataType.DataType name cons
      
      
  return ()
  where
  knotTie :: 
  
  parseCon :: Parser (String, [Type String])
  parseCon = do
    name <- parseName
    args <- many parseType
    return (name, args)
  {-
  parseCon :: ZType -> Parser ZVar
  parseCon result_type = do
    name_s <- parseName
    arg_types <- many parseType
    let con_type = Type.unflatten (arg_types ++ [result_type])
    con_name <- lift $ Name.declare name_s
    new_con <- lift $ Var.declare con_name con_type Var.Constructor
    Thy.addDefinition con_name (Type.Var new_con)
    return new_con
    -}
parseType :: Parser (Type String)
parseType = parseTypeFun <|> (Type.Var <$> parseName)
  where
  parseTypeFun = do
    types <- parseType `Ps.sepBy1` Ps.string "->"
    return (Type.unflatten types)

lookupDataType :: String -> Parser ZDataType
lookupDataType name = do
  dt_lookup <- lift $ Thy.lookupDataType name
  case dt_lookup of
    Just dtype -> return dtype
    Nothing -> Ps.parserFail $ "Type not found: " ++ name

parseName :: Parser String
parseName = some (Ps.alphaNum <|> Ps.oneOf chars)
  where                                      
  chars = "-<>.,?#+*&%$£^:;~/\\!=@[]'"

{-

lookupDef :: String -> TermParser ZTerm
lookupDef name = do
  mby_local <- asks (Map.lookup name)
  case mby_local of
    Just def -> return def
    Nothing -> do
      mby_global <- Thy.lookupDefinition name
      case mby_global of
        Just def -> return def
        Nothing -> error $ "Variable not found: " ++ name

runTermParser :: TermParser a -> Parser a
runTermParser = flip runReaderT mempty
        
localDefinition :: String -> ZTerm -> TermParser a -> TermParser a
localDefinition name term = local (Map.insert name term)

transformZLisp :: Lisp -> Lisp
transformZLisp (LL (LN "lam":rest)) 
  | length rest > 2 = foldr foldLam term typed_vars
  where
  (typed_vars, term) = takeLast rest
  foldLam typed_var term = LL [LN "lam", typed_var, term]
transformZLisp (LL (LN "fun":typed_name@(LL [LN name, type_l]):rest)) = 
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
  Thy.addDefinition name term
parseTopLevel (LL ((LN "type"):(LN type_name):l_cons)) = do
  name <- Name.declare type_name
  rec let new_dtype = DataType.DataType name cons
      Thy.addDataType new_dtype
      cons <- mapM (parseCon (Type.Var new_dtype)) l_cons
  return ()
  where
  parseCon :: ZType -> Lisp -> Parser ZVar
  parseCon res_type l_con = do
    args <- mapM parseType l_args
    let con_type = Type.unflatten (args ++ [res_type])
    new_con <- Var.declare con_name con_type Var.Constructor
    Thy.addDefinition con_name (Term.Var new_con)
    return new_con    
    where
    (con_name, l_args) = case l_con of
      LN con_name -> (con_name, [])
      LL ((LN con_name):l_args) -> (con_name, l_args)
parseTopLevel (LL (LN "conject":LN name:l_cls)) = do
  cls <- parseClause l_cls
  Thy.addConjecture name cls
parseTopLevel other = 
  error $ "Top level statement not recognized: " ++ show other
  
parseClause :: Lisp -> Parser ZClause
parseClause (LL (LN "all":rest)) = do
  where
  (vars_l, 
  
parseType :: Lisp -> Parser ZType
parseType (LN name) = lookupType name
parseType (LL l_types) = do
  types <- mapM parseType l_types
  return (Type.unflatten types)
  
parseTerm :: Lisp -> TermParser ZTerm
parseTerm (LN name) = lookupDef name
parseTerm (LL [LN "let", LL [LN name, l_term], l_in]) = do
  term <- parseTerm l_term
  localDefinition name term (parseTerm l_in)
parseTerm (LL [LN "fix", LL [LN name, l_type], l_term]) = do 
  fix_type <- lift (parseType l_type)
  fix_var <- Var.declare name fix_type Var.Fixed
  term <- localDefinition name (Term.Var fix_var) (parseTerm l_term)
  return (Term.Fix fix_var term)
parseTerm (LL [LN "lam", LL [LN name, l_type], l_term]) = do
  var_type <- lift (parseType l_type)
  lam_var <- Var.declare name var_type (Var.Universal mempty)
  term <- localDefinition name (Term.Var lam_var) (parseTerm l_term)
  return (Term.Lam lam_var term)
parseTerm (LL (LN "case":LN label:l_term:l_alts)) = do
  name <- Name.declare label
  term <- parseTerm l_term
  alts <- mapM parseAlt l_alts
  return (Term.Cse name term alts)
  where
  parseAlt (LL [l_pattern, l_term]) = do
    Term.Var con_var <- lookupDef con_name
    let (var_types, con_type) = takeLast . Type.flatten $ typeOf con_var
    vars <- zipWithM makeVar var_names var_types
    term <- foldr localVar (parseTerm l_term) (var_names `zip` vars)
    return (Term.Alt con_var vars term)
    where
    (con_name, var_names) = case l_pattern of
      LN name -> (name, [])
      LL (LN name:names_l) -> (name, map Lisp.fromLN names_l)
    
    makeVar :: String -> ZType -> TermParser ZVar
    makeVar var_name var_type =
      Var.declare var_name var_type (Var.Universal mempty)
      
    localVar :: (String, ZVar) -> TermParser a -> TermParser a
    localVar (name, var) = localDefinition name (Term.Var var)
  
parseTerm (LL l_terms) = 
  Term.unflattenApp <$> mapM parseTerm l_terms
  -}
