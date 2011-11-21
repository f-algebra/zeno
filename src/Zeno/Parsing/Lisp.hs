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
  
lispWord :: Parsec String () String
lispWord = many1 $ satisfy (not . flip elem "(.) \n")
  
lispParser :: Parsec String () Lisp
lispParser = LL <$> many (spaces *> lispList <* spaces)
  where
  lispName = LN <$> lispWord
  lispList = do
    char '(' 
    inner <- many1 listInner
    end <- try dottedEnd <|> pure []
    char ')'
    let all_inner | null end = inner
                  | otherwise = inner ++ [LL end]
    return (LL all_inner)
    where
    dottedEnd = char '.' *> many1 listInner
    listInner = spaces *> (lispName <|> lispList) <* spaces
  
fromLN :: Lisp -> String
fromLN (LN name) = name

instance WithinTraversable Lisp Lisp where
  mapWithinM f (LL ls) = 
    f =<< LL `liftM` mapM (mapWithinM f) ls
  mapWithinM f ls = f ls
