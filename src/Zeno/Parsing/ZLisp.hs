module Zeno.Parsing.ZLisp (
  parse
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Utils
import Zeno.Traversing
import Zeno.Core ( Zeno )
import Zeno.Var ( ZVar, ZTerm, ZType, ZClause, ZEquation )
import Zeno.Type ( typeOf )
import Zeno.Parsing.Lisp ( Lisp (..) )

import qualified Zeno.Evaluation as Eval
import qualified Zeno.Core as Zeno
import qualified Zeno.Name as Name
import qualified Zeno.Var as Var
import qualified Zeno.DataType as DataType
import qualified Zeno.Clause as Clause
import qualified Zeno.Term as Term
import qualified Zeno.Type as Type
import qualified Zeno.Parsing.Lisp as Lisp

import qualified Data.Set as Set
import qualified Data.Map as Map

type TermParser = ReaderT (Map String ZTerm, Set ZVar) Zeno

parse :: String -> Zeno ()
parse text = parseTheory (Lisp.toLisp text)

runTermParser :: TermParser a -> Zeno a
runTermParser = flip runReaderT mempty

lookupType :: String -> Zeno ZType
lookupType name = do
  dt_lookup <- Zeno.lookupDataType name
  case dt_lookup of
    Just dtype -> return (Type.Var dtype)
    Nothing -> error $ "Type not found: " ++ name

lookupDef :: String -> TermParser ZTerm
lookupDef name = do
  mby_local <- asks (Map.lookup name . fst)
  case mby_local of
    Just def -> return def
    Nothing -> do
      mby_global <- Zeno.lookupDefinition name
      case mby_global of
        Just def -> return def
        Nothing -> error $ "Variable not found: " ++ name
        
localDefinition :: String -> ZTerm -> TermParser a -> TermParser a
localDefinition name term = local (first (Map.insert name term))

withinFix :: ZVar -> TermParser a -> TermParser a
withinFix fix_var = local (second (Set.insert fix_var))

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

parseTheory :: Lisp -> Zeno ()
parseTheory (LL decls) = 
  mapM_ parseTopLevel . map (mapWithin transformZLisp) $ decls

parseTopLevel :: Lisp -> Zeno ()
parseTopLevel (LL [LN "def", LN name, def]) = do
  term <- runTermParser (Eval.evaluate <$> parseTerm def)
  Zeno.addDefinition name term
parseTopLevel (LL ((LN "type"):(LN type_name):l_cons)) = do
  name <- Name.declare type_name
  rec let new_dtype = DataType.DataType name cons
      Zeno.addDataType new_dtype
      cons <- mapM (parseCon (Type.Var new_dtype)) l_cons
  return ()
  where
  parseCon :: ZType -> Lisp -> Zeno ZVar
  parseCon res_type l_con = do
    args <- mapM parseType l_args
    let con_type = Type.unflatten (args ++ [res_type])
    new_con <- Var.declare con_name con_type Var.Constructor
    Zeno.addDefinition con_name (Term.Var new_con)
    return new_con    
    where
    (con_name, l_args) = case l_con of
      LN con_name -> (con_name, [])
      LL ((LN con_name):l_args) -> (con_name, l_args)
parseTopLevel (LL [LN "prop", LN name, cls_l]) = do
  cls <- runTermParser (parseClause cls_l)
  Zeno.addConjecture name cls
parseTopLevel other = 
  error $ "Top level statement not recognized: " ++ show other
  
  
parseClause :: Lisp -> TermParser ZClause
parseClause (LL vars_then_body) = do
  cls_body <- localVars 
            $ mapM parseEquation 
            $ Lisp.flatten cls_body_l
  let (cls_antes, cls_consq) = takeLast cls_body
  return (Clause.Clause cls_antes cls_consq)
  where
  (typed_vars, cls_body_l) = takeLast vars_then_body
  
  localVars :: TermParser a -> TermParser a
  localVars = appEndo $ concatMap (Endo . localTypedVar) typed_vars
  
  localTypedVar :: Lisp -> TermParser a -> TermParser a
  localTypedVar (LL [LN name, type_l]) action = do
    var_type <- lift (parseType type_l)
    var <- Var.declare name var_type Var.Bound
    localDefinition name (Term.Var var) action
  
  parseEquation :: Lisp -> TermParser ZEquation
  parseEquation (LL [LN "=", left_term_l, right_term_l]) = do
    left_term <- parseTerm left_term_l
    right_term <- parseTerm right_term_l
    return (Clause.Equal left_term right_term)
  parseEquation other = 
    error $ "Equation expected in place of: " ++ show other

parseType :: Lisp -> Zeno ZType
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
  fix_var <- Var.declare name fix_type Var.Bound
  term <- localDefinition name (Term.Var fix_var) 
        $ withinFix fix_var 
        $ parseTerm l_term
  return (Term.Fix fix_var term)
parseTerm (LL [LN "lam", LL [LN name, l_type], l_term]) = do
  var_type <- lift (parseType l_type)
  lam_var <- Var.declare name var_type Var.Bound
  term <- localDefinition name (Term.Var lam_var) (parseTerm l_term)
  return (Term.Lam lam_var term)
parseTerm (LL (LN "case":LN label:l_term:l_alts)) = do
  name <- Name.declare label
  term <- parseTerm l_term
  alts <- mapM parseAlt l_alts
  fixes <- asks snd
  return (Term.Cse name fixes term alts)
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
      Var.declare var_name var_type Var.Bound
      
    localVar :: (String, ZVar) -> TermParser a -> TermParser a
    localVar (name, var) = localDefinition name (Term.Var var)
parseTerm (LL l_terms) = 
  Term.unflattenApp <$> mapM parseTerm l_terms
  
