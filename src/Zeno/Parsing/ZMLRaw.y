{
module Zeno.Parsing.ZMLRaw ( 
  RTypeDef (..), RVar (..), RProp (..),
  RType, RTerm, RAlt, RClause, REquation,
  isNameChar, 
  parseTypeDef, parseBinding, 
  parseProp, parseTerm
) where
import Prelude ()
import Zeno.Prelude
import Zeno.Show

import qualified Zeno.Clause as Clause
import qualified Zeno.Term as Term
import qualified Zeno.Type as Type
import qualified Zeno.Name as Name
}

%name typeDef TypeDef
%name binding Binding
%name prop Prop
%name term Term

%tokentype { Token }

%token
  name        { TokenName $$ }
  '|'         { TokenBar }
  ':'         { TokenType }
  '->'        { TokenArr }
  '('         { TokenOP }
  ')'         { TokenCP }
  '='         { TokenEq }
  'case'      { TokenCase }
  'of'        { TokenOf }
  'fun'       { TokenLambda }
  'fix'       { TokenFix }
  'let'       { TokenLet }
  'rec'       { TokenRec }
  'in'        { TokenIn }
  
%right '->'
    
%%
  
Type :: { RType }
  : name                              { Type.Var $1 }
  | '(' Type ')'                      { $2 }
  | Type '->' Type                    { Type.Fun $1 $3 }
  
Types :: { [RType] } 
  : Type                              { [$1] }
  | Types Type                        { $1 ++ [$2] }
  
TypeDef :: { RTypeDef }
  : name '=' TypeDefCons              { RTypeDef $1
                                          (map (mkTypeDefCon $1) $3) }
  
TypeDefCon :: { (String, [RType]) }
  : name                              { ($1, []) }
  | name Types                        { ($1, $2) }
  
  
TypeDefCons :: { [(String, [RType])] }
  : TypeDefCon                        { [$1] }
  | TypeDefCons '|' TypeDefCon        { $1 ++ [$3] }
  
Pattern :: { [RVar] }
  : UntypedVar                        { [$1] }
  | Pattern UntypedVar                { $1 ++ [$2] }
  
Matches :: { [RAlt] }
  : Match                             { [$1] }
  | Matches '|' Match                 { $1 ++ [$3] }
  
Match :: { RAlt }
  : Pattern '->' Term                 { Term.Alt (head $1) (tail $1) $3 }
  
TypedVar :: { RVar }
  : '(' name ':' Type ')'             { RVar $2 (Just $4) }
  
TypedVars :: { [RVar] }
  :                                   { [] }
  | TypedVars TypedVar                { $1 ++ [$2] }
  
UntypedVar :: { RVar }
  : name                              { RVar $1 Nothing }
  
Binding :: { (String, RTerm) } 
  : name '=' Term                     { ($1, $3) }
  | 'rec' TypedVar '=' Term           { (varName $2, Term.Fix $2 $4) }
  
Term :: { RTerm }
  : UntypedVar                        { Term.Var $1 }
  | Term UntypedVar                   { Term.App $1 (Term.Var $2) }
  | Term '(' Term ')'                 { Term.App $1 $3 }
  | '(' Term ')'                      { $2 }
  | 'fun' TypedVar '->' Term          { Term.Lam $2 $4 }
  | 'fix' TypedVar 'in' Term          { Term.Fix $2 $4 }
  | 'case' Term 'of' Matches          { Term.Cse Name.empty mempty $2 $4 }
  
Prop :: { RProp }
  : name TypedVars '=' Clause         { RProp $1 $2 $4 } 
  
Clause :: { RClause } 
  : Clause '->' Equation              { Clause.Clause (Clause.flatten $1) $3 }
  | Equation                          { Clause.Clause [] $1 }
  
Equation :: { REquation }
  : Term '=' Term                     { Clause.Equal $1 $3 }
  
{
happyError :: [Token] -> a
happyError tokens = error $ "Parse error\n" ++ (show tokens)

type RType = Type.Type String
type RTerm = Term.Term RVar
type RAlt = Term.Alt RVar
type RClause = Clause.Clause RVar
type REquation = Clause.Equation RVar

data RProp
  = RProp     { propName :: String,
                propVars :: [RVar],
                propClause :: RClause }

data RVar 
  = RVar      { varName :: String,
                varType :: Maybe RType }
  deriving ( Eq, Ord, Show )

data RTypeDef 
  = RTypeDef  { typeName :: String,
                typeCons :: [(String, RType)] }
  
mkTypeDefCon :: String -> (String, [RType]) -> (String, RType)
mkTypeDefCon result_type (name, arg_types) = 
  (name, Type.unflatten (arg_types ++ [Type.Var result_type]))

data Token
  = TokenBar
  | TokenName String
  | TokenType
  | TokenArr
  | TokenOP
  | TokenCP
  | TokenEq
  | TokenCase
  | TokenOf
  | TokenLambda
  | TokenFix
  | TokenLet
  | TokenRec
  | TokenIn
  deriving Show
  
isNameChar :: Char -> Bool
isNameChar c = isAlphaNum c || c `elem` "'_"
  
lexer :: String -> [Token]
lexer [] = []
lexer (' ':cs) = lexer cs
lexer ('\n':cs) = lexer cs
lexer ('-':'>':cs) = TokenArr : lexer cs
lexer (':':cs) = TokenType : lexer cs
lexer ('(':cs) = TokenOP : lexer cs
lexer (')':cs) = TokenCP : lexer cs
lexer ('=':cs) = TokenEq : lexer cs
lexer ('|':cs) = TokenBar : lexer cs
lexer (c:cs) 
  | isSpace c = lexer cs
  | isNameChar c = lexVar (c : cs)
  where
  lexVar cs =
    case span isNameChar cs of
      ("fun", rest) -> TokenLambda : lexer rest
      ("fix", rest) -> TokenFix : lexer rest
      ("case", rest) -> TokenCase : lexer rest
      ("of", rest) -> TokenOf : lexer rest
      ("let", rest) -> TokenLet : lexer rest
      ("rec", rest) -> TokenRec : lexer rest
      ("in", rest) -> TokenIn : lexer rest
      (name, rest) -> TokenName name : lexer rest
lexer cs = error $ "Unrecognized symbol " ++ take 1 cs

parseTypeDef :: String -> RTypeDef
parseTypeDef = typeDef . lexer

parseBinding :: String -> (String, RTerm)
parseBinding = binding . lexer

parseProp :: String -> RProp
parseProp = prop . lexer

parseTerm :: String -> RTerm
parseTerm = term . lexer

instance Show RTypeDef where
  show (RTypeDef name cons) = 
    "type " ++ name ++ " where" ++ cons_s
    where
    cons_s = concatMap (mappend "\n  " . showCon) cons
    showCon (name, typ) = name ++ " : " ++ show typ
}
