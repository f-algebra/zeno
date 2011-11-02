module Zeno.Parsing.Lisp (
  Lisp (..), parse,
) where

import Prelude ()
import Zeno.Prelude hiding ( many, (<|>) )

import Text.Parsec hiding ( State, parse )

data Lisp
  = LN String
  | LL [Lisp]

instance Show Lisp where
  show (LN str) = str
  show (LL ls) = "(" ++ intercalate " " (map show ls) ++ ")"
  
parse :: String -> Lisp
parse text = case runP lispParser () "zeno" text of
  Right lisp -> lisp
  Left err -> error (show err)
  
nameChars :: Parsec String () Char
nameChars = foldr (\c p -> char c <|> p) alphaNum chars
  where chars = ['-', '<', '>', '.', ',', '?', 
                 '#', '+', '*', '&', '|', '%', 
                 '$', '£', '"', '\'', '^', ':', ';']
       
lispParser :: Parsec String () Lisp
lispParser = LL <$> many (spaces *> lispList <* spaces)
  where
  lispName = LN <$> many1 nameChars
  lispList = LL <$> (char '(' *> many1 listInner <* char ')')
  listInner = spaces *> (lispName <|> lispList) <* spaces
  
fromLN :: Lisp -> String
fromLN (LN name) = name
