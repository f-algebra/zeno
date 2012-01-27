{
module Zeno.Parsing.ZMLRaw ( 
  RTypeDef (..), RVar (..), RProp,
  RType, RTerm, RAlt, RClause, REquation,
  isNameChar, 
  parseTypeDef, parseBinding, 
  parseProp, parseTerm, parseClause
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Show

import qualified Zeno.Logic as Logic
import qualified Zeno.Term as Term
import qualified Zeno.Type as Type
import qualified Zeno.Name as Name

}

%name typeDef TypeDef
%name binding Binding
%name prop Prop
%name term Term
%name clause Clause

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
  'all'       { TokenAll }
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
  | '|' Match                         { [$2] }
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
  | 'fun' TypedVars '->' Term         { Term.unflattenLam $2 $4 }
  | 'fix' TypedVar 'in' Term          { Term.Fix $2 $4 }
  | 'case' Term 'of' Matches          { Term.Cse undefined $2 $4 }
  
Prop :: { RProp }
  : name '=' Clause                   { ($1, $3) } 
  
InnerClause :: { Logic.Clause RVar } 
  : InnerClause '->' Equation         { Logic.Clause (Logic.flatten $1) $3 }
  | Equation                          { Logic.Clause [] $1 }
  
Clause :: { RClause }
  : InnerClause                       { ([], $1) }
  | 'all' TypedVars '->' InnerClause  { ($2, $4) }
  
Equation :: { REquation }
  : Term '=' Term                     { Logic.Equal $1 $3 }
  
{
happyError :: [Token] -> a
happyError tokens = error $ "Parse error\n" ++ (show tokens)

type RType = Type.Type String
type RTerm = Term.Term RVar
type RAlt = Term.Alt RVar
type RClause = ([RVar], Logic.Clause RVar)
type REquation = Logic.Equation RVar
type RProp = (String, RClause)
data RVar
  = RVar      { varName :: String, 
                varType :: Maybe RType }
                
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
  | TokenAll
  | TokenFix
  | TokenLet
  | TokenRec
  | TokenIn
  deriving Show
  
isNameChar :: Char -> Bool
isNameChar c = isAlphaNum c || c `elem` "'_"
  
lexer :: String -> [Token]
lexer [] = []
lexer ('/':'*':cs) = lexer (commentEnd cs)
  where
  commentEnd [] = []
  commentEnd ('*':'/':cs) = cs
  commentEnd (c:cs) = commentEnd cs
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
      ("all", rest) -> TokenAll : lexer rest
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

parseClause :: String -> RClause
parseClause = clause . lexer

instance Show RTypeDef where
  show (RTypeDef name cons) = 
    "type " ++ name ++ " where" ++ cons_s
    where
    cons_s = concatMap (mappend "\n  " . showCon) cons
    showCon (name, typ) = name ++ " : " ++ show typ
}
