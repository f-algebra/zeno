{
module Zeno.Parsing.ZMLRaw ( 
  RTypeDef (..), RVar (..), 
  RType, RTerm, RAlt,
  isNameChar, parseTypeDef, parseBinding
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
%tokentype { Token }

%token
  var         { TokenVar $$ }
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
  : var                               { Type.Var $1 }
  | Type '->' Type                    { Type.Fun $1 $3 }
  | '(' Type ')'                      { $2 }
  
Types :: { [RType] }
  : Types Type                        { $1 ++ [$2] }
  | Type                              { [$1] }
  
TypeDef :: { RTypeDef }
  : var '=' TypeDefCons               { RTypeDef $1
                                          (map (mkTypeDefCon $1) $3) }
  
TypeDefCon :: { (String, [RType]) }
  : var Types                         { ($1, $2) }
  | var                               { ($1, []) }
  
TypeDefCons :: { [(String, [RType])] }
  : TypeDefCons '|' TypeDefCon        { $1 ++ [$3] }
  | TypeDefCon                        { [$1] }
  
Pattern :: { [RVar] }
  : Pattern UntypedVar                { $1 ++ [$2] }
  | UntypedVar                        { [$1] }
  
Matches :: { [RAlt] }
  : Matches '|' Match                 { $1 ++ [$3] }
  | Match                             { [$1] }
  
Match :: { RAlt }
  : Pattern '->' Term                 { Term.Alt (head $1) (tail $1) $3 }
  
TypedVar :: { RVar }
  : '(' var ':' Type ')'              { RVar $2 (Just $4) }
  
UntypedVar :: { RVar }
  : var                               { RVar $1 Nothing }
  
Binding :: { (String, RTerm) } 
  : var '=' Term                      { ($1, $3) }
  | 'rec' TypedVar '=' Term           { (varName $2, Term.Fix $2 $4) }
  
Term :: { RTerm }
  : 'fun' TypedVar '->' Term          { Term.Lam $2 $4 }
  | 'fix' TypedVar 'in' Term          { Term.Fix $2 $4 }
  | 'case' Term 'of' Matches          { Term.Cse Name.empty mempty $2 $4 }
  | UntypedVar Term                   { Term.App (Term.Var $1) $2 }
  | '(' Term ')' Term                 { Term.App $2 $4 }
  | '(' Term ')'                      { $2 }
  | UntypedVar                        { Term.Var $1 }
  
{
happyError :: [Token] -> a
happyError tokens = error $ "Parse error\n" ++ (show tokens)

type RType = Type.Type String
type RTerm = Term.Term RVar
type RAlt = Term.Alt RVar

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
  | TokenVar String
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
isNameChar c = isAlpha c || c `elem` "'_"
  
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
      (var, rest) -> TokenVar var : lexer rest
lexer cs = error $ "Unrecognized symbol " ++ take 1 cs

parseTypeDef :: String -> RTypeDef
parseTypeDef = typeDef . lexer

parseBinding :: String -> (String, RTerm)
parseBinding = binding . lexer

instance Show RTypeDef where
  show (RTypeDef name cons) = 
    "type " ++ name ++ " where" ++ cons_s
    where
    cons_s = concatMap (mappend "\n  " . showCon) cons
    showCon (name, typ) = name ++ " : " ++ show typ
}
