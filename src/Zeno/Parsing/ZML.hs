module Zeno.Parsing.ZML (
  readTypeDef, readLine
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Utils
import Zeno.Traversing
import Zeno.Core ( Zeno )
import Zeno.Var ( ZVar, ZTerm, ZType, ZClause, ZEquation )
import Zeno.Type ( typeOf )

import qualified Zeno.Core as Zeno
import qualified Zeno.Name as Name
import qualified Zeno.Var as Var
import qualified Zeno.DataType as DataType
import qualified Zeno.Clause as Clause
import qualified Zeno.Term as Term
import qualified Zeno.Type as Type
import qualified Zeno.Parsing.ZMLRaw as Raw

readLine :: String -> (Maybe (String, String), String)
readLine str = 
  case span (/= ';') potential_line of
    ([], rest) -> (Nothing, rest)
    (line, rest) -> (Just (span Raw.isNameChar line), rest)
  where
  potential_line = dropWhile (not . Raw.isNameChar) str

readTypeDef :: String -> Zeno ()
readTypeDef = parseRTypeDef . Raw.parseTypeDef

parseRTypeDef :: Raw.RTypeDef -> Zeno ()
parseRTypeDef (Raw.RTypeDef type_name type_cons) = do
  name <- Name.declare type_name
  rec let new_dtype = DataType.DataType name cons
      Zeno.defineType new_dtype
      cons <- mapM (parseCon (Type.Var new_dtype)) type_cons
  return ()
  where
  parseCon :: ZType -> (String, Raw.RType) -> Zeno ZVar
  parseCon result_type (con_name, rtype) = do
    con_type <- parseRType rtype
    new_con <- Var.declare con_name con_type Var.Constructor
    Zeno.defineTerm con_name (Term.Var new_con)
    return new_con
  
parseRType :: Raw.RType -> Zeno ZType
parseRType (Type.Var rtvar) = 
  lookupType rtvar
parseRType (Type.Fun arg res) = 
  Type.Fun <$> parseRType arg <*> parseRType res

lookupType :: String -> Zeno ZType
lookupType name = do
  dt_lookup <- Zeno.lookupDataType name
  case dt_lookup of
    Just dtype -> return (Type.Var dtype)
    Nothing -> error $ "Type not found: " ++ name
