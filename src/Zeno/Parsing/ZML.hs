module Zeno.Parsing.ZML (
  readTypeDef, readBinding, readProp, 
  readLine, readTerm
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Utils
import Zeno.Traversing
import Zeno.Core ( Zeno )
import Zeno.Var ( ZVar, ZTerm, ZType, ZAlt, ZClause, ZEquation )
import Zeno.Type ( typeOf )
import Zeno.Parsing.ZMLRaw ( RVar (..), RTypeDef (..), RProp (..),
                             RTerm, RAlt, RType, REquation, RClause )

import qualified Zeno.Core as Zeno
import qualified Zeno.Name as Name
import qualified Zeno.Var as Var
import qualified Zeno.DataType as DataType
import qualified Zeno.Clause as Clause
import qualified Zeno.Term as Term
import qualified Zeno.Type as Type
import qualified Zeno.Parsing.ZMLRaw as Raw
import qualified Data.Map as Map
import qualified Data.Set as Set

type Parser = ReaderT (Map String ZTerm, Set ZVar) Zeno

readLine :: String -> (Maybe (String, String), String)
readLine str = 
  case span (/= ';') potential_line of
    ([], rest) -> (Nothing, rest)
    (line, rest) -> (Just (span Raw.isNameChar line), rest)
  where
  potential_line = dropWhile (not . Raw.isNameChar) str

readTypeDef :: String -> Zeno ()
readTypeDef = runParser . parseRTypeDef . Raw.parseTypeDef

readBinding :: String -> Zeno ()
readBinding text = runParser $ do
  zterm <- parseRTerm rterm
  Zeno.defineTerm name zterm
  where
  (name, rterm) = Raw.parseBinding text
  
readProp :: String -> Zeno ()
readProp text = runParser $ do
  zvars <- mapM parseTypedRVar rvars
  zcls <- localVars (var_names `zip` zvars) 
        $ parseRClause rcls
  Zeno.defineProp name zcls
  where
  RProp name rvars rcls = Raw.parseProp text
  var_names = map Raw.varName rvars
  
readTerm :: String -> Zeno ZTerm
readTerm = runParser . parseRTerm . Raw.parseTerm

runParser :: Parser a -> Zeno a
runParser = flip runReaderT (mempty, mempty)

parseRTypeDef :: RTypeDef -> Parser ()
parseRTypeDef (RTypeDef type_name type_cons) = do
  name <- Name.declare type_name
  rec let new_dtype = DataType.DataType name cons
      Zeno.defineType new_dtype
      cons <- mapM (parseCon (Type.Var new_dtype)) type_cons
  return ()
  where
  parseCon :: ZType -> (String, RType) -> Parser ZVar
  parseCon result_type (con_name, rtype) = do
    con_type <- parseRType rtype
    new_con <- Var.declare con_name con_type Var.Constructor
    Zeno.defineTerm con_name (Term.Var new_con)
    return new_con
    
parseRClause :: RClause -> Parser ZClause
parseRClause (Clause.Clause antes consq) =
  Clause.Clause <$> mapM parseREquation antes <*> parseREquation consq

parseREquation :: REquation -> Parser ZEquation
parseREquation (Clause.Equal rleft rright) = 
  Clause.Equal <$> parseRTerm rleft <*> parseRTerm rright
  
parseRType :: RType -> Parser ZType
parseRType (Type.Var rtvar) = 
  lookupType rtvar
parseRType (Type.Fun arg res) = 
  Type.Fun <$> parseRType arg <*> parseRType res
  
parseRTerm :: RTerm -> Parser ZTerm
parseRTerm (Term.Var var) = 
  lookupTerm (varName var)
parseRTerm (Term.App t1 t2) = 
  Term.App <$> parseRTerm t1 <*> parseRTerm t2
parseRTerm (Term.Lam typed_var rhs) = do
  new_var <- parseTypedRVar typed_var
  zhs <- localTerm (Raw.varName typed_var) (Term.Var new_var) 
       $ parseRTerm rhs
  return (Term.Lam new_var zhs)
parseRTerm (Term.Fix typed_var rhs) = do
  new_var <- parseTypedRVar typed_var
  zhs <- localTerm (Raw.varName typed_var) (Term.Var new_var) 
       $ withinFix new_var
       $ parseRTerm rhs
  return (Term.Fix new_var zhs)
parseRTerm (Term.Cse _ _ rterm ralts) = do
  name <- Name.invent
  fixes <- asks snd
  zterm <- parseRTerm rterm
  zalts <- mapM parseRAlt ralts
  return (Term.Cse name fixes zterm zalts)
  where
  parseRAlt :: RAlt -> Parser ZAlt
  parseRAlt (Term.Alt rcon rargs rterm) = do
    Just (Term.Var con_var) <- Zeno.lookupTerm (Raw.varName rcon)
    let arg_types = (butlast . Type.flatten . Type.typeOf) con_var
        arg_names = map varName rargs
    arg_vars <- zipWithM boundVar arg_names arg_types
    let named_vars = arg_names `zip` arg_vars
    zterm <- localVars named_vars (parseRTerm rterm) 
    return (Term.Alt con_var arg_vars zterm)
    where
    boundVar name typ = Var.declare name typ Var.Bound

parseTypedRVar :: RVar -> Parser ZVar
parseTypedRVar (RVar name (Just rtype)) = do
  ztype <- parseRType rtype
  Var.declare name ztype Var.Bound
    
localVars :: [(String, ZVar)] -> Parser a -> Parser a
localVars = appEndo . concatMap (Endo . uncurry localTerm . second Term.Var)
  
lookupType :: String -> Parser ZType
lookupType name = do
  dt_lookup <- Zeno.lookupType name
  case dt_lookup of
    Just dtype -> return (Type.Var dtype)
    Nothing -> error $ "Type not found: " ++ name
    
lookupTerm :: String -> Parser ZTerm
lookupTerm name = do
  mby_local <- asks (Map.lookup name . fst)
  case mby_local of
    Just def -> return def
    Nothing -> do
      mby_global <- Zeno.lookupTerm name
      case mby_global of
        Just def -> return def
        Nothing -> error $ "Variable not found: " ++ name
        
localTerm :: String -> ZTerm -> Parser a -> Parser a
localTerm name term = local (first (Map.insert name term))

withinFix :: ZVar -> Parser a -> Parser a
withinFix fix_var = local (second (Set.insert fix_var))

