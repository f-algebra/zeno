{
module Zeno.Parsing.ZMLRaw ( 
  RTypeDef (..), RType, RTerm,
  isNameChar, parseTypeDef
) where

import Prelude ()
import Zeno.Prelude
import Zeno.Show

import qualified Zeno.Clause as Clause
import qualified Zeno.Term as Term
import qualified Zeno.Type as Type
}

%name typeDef TypeDef
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
  '=>'        { TokenLeadsTo }
  'fn'        { TokenLambda }
  
  
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
  
Pattern :: { [String] }
  : Pattern var                       { $1 ++ [$2] }
  | var                               { [$1] }
  
Matches :: { [([String], RTerm)] }
  : Matches '|' Match                 { $1 ++ [$3] }
  | Match                             { [$1] }
  
Match :: { ([String], RTerm) }
  : Pattern '=>' Term
  
Term :: { RTerm }
  : '(' Term ')'                      { $2 }
  | 
  
{
happyError :: [Token] -> a
happyError tokens = error $ "Parse error\n" ++ (show tokens)

type RType = Type.Type String
type RTerm = Term.Term String

data RTypeDef 
  = RTypeDef String [(String, RType)]
  
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
  deriving Show
  
isNameChar :: Char -> Bool
isNameChar c = isAlpha c || c `elem` "'"
  
lexer :: String -> [Token]
lexer [] = []
lexer (' ':cs) = lexer cs
lexer ('\n':cs) = lexer cs
lexer ('-':'>':cs) = TokenArr : lexer cs
lexer (':':cs) = TokenType : lexer cs
lexer ('(':cs) = TokenOP : lexer cs
lexer (')':cs) = TokenCP : lexer cs
lexer ('=':'>':cs) = TokenLeadsTo : lexer cs
lexer ('=':cs) = TokenEq : lexer cs
lexer ('|':cs) = TokenBar : lexer cs
lexer ('f':'n':cs) = TokenLambda : lexer cs
lexer (c:cs) 
  | isSpace c = lexer cs
  | isNameChar c = lexVar (c : cs)
  where
  lexVar cs =
    case span isNameChar cs of
      (var, rest) -> TokenVar var : lexer rest
lexer cs = error $ "Unrecognized symbol " ++ take 1 cs

parseTypeDef :: String -> RTypeDef
parseTypeDef = typeDef . lexer

instance Show RTypeDef where
  show (RTypeDef name cons) = 
    "type " ++ name ++ " where" ++ cons_s
    where
    cons_s = concatMap (mappend "\n  " . showCon) cons
    showCon (name, typ) = name ++ " : " ++ show typ
}
