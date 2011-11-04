module Zeno.Parsing.Lisp (
  Lisp (..), parse, fromLN
) where

import Prelude ()
import Zeno.Prelude hiding ( many, (<|>) )
import Zeno.Traversing

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
nameChars = alphaNum <|> oneOf chars
  where chars = "-<>.,?#+*&|%$£^:;~/\\!=@[]'"
  
lispParser :: Parsec String () Lisp
lispParser = LL <$> many (spaces *> lispList <* spaces)
  where
  lispName = LN <$> many1 nameChars
  lispList = LL <$> (char '(' *> many1 listInner <* char ')')
  listInner = spaces *> (lispName <|> lispList) <* spaces
  
fromLN :: Lisp -> String
fromLN (LN name) = name

instance WithinTraversable Lisp Lisp where
  mapWithinM f (LL ls) = 
    f =<< LL `liftM` mapM (mapWithinM f) ls
  mapWithinM f ls = f ls
