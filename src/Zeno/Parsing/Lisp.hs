module Zeno.Parsing.Lisp (
  Lisp (..), LispParserT,
  toLisp, fromLisp,
  stepInside, anyName, name, anyLisp,
  fromLN, isLN, flatten
) where

import Prelude ()
import Zeno.Prelude hiding ( many, (<|>), next )
import Zeno.Traversing

import Text.Parsec hiding ( State, parse, satisfy )
import qualified Text.Parsec as Parsec

data Lisp
  = LN String
  | LL [Lisp]
  deriving ( Eq, Ord )

type LispParserT u m = ParsecT Lisp u m
  
toLisp :: String -> Lisp
toLisp text = case runP lispParser () "zeno" text of
  Right lisp -> lisp
  Left err -> error (show err)
  
fromLisp :: Monad m => LispParserT u m a -> u -> Lisp -> m (Either ParseError a)
fromLisp parser initial = runParserT parser initial "lisp"
  
stepInside :: Monad m => LispParserT u m a -> LispParserT u m a
stepInside run_parser = do
  lisp_stream <- getInput
  case (runIdentity . uncons) lisp_stream of
    Just (lisp, rest) -> do
      setInput lisp
      val <- run_parser
      lisp_leftover <- getInput
      case lisp_leftover of
        LL [] -> do
          setInput rest
          return val
        leftover -> parserFail $ 
          "Parsing: " ++ show lisp ++ " finished with left-over: " ++ show leftover 
    Nothing -> 
      parserFail "Cannot step inside EOS"
  
name :: Stream s m Lisp => String -> ParsecT s u m String
name n = fromLN <$> satisfy (== (LN n))
     
anyName :: Stream s m Lisp => ParsecT s u m String
anyName = fromLN <$> satisfy isLN

anyLisp :: Stream s m Lisp => ParsecT s u m Lisp
anyLisp = satisfy (const True)

satisfy :: Stream s m Lisp => (Lisp -> Bool) -> ParsecT s u m Lisp
satisfy p = tokenPrim show 
                      (\pos _ _ -> pos) 
                      (\l -> if p l then Just l else Nothing)  
  
  
lispParser :: Parsec String () Lisp
lispParser = LL <$> many (spaces *> lispList <* spaces)
  where
  lispWord :: Parsec String () String
  lispWord = many1 $ Parsec.satisfy (not . flip elem "(.) \n")
  
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

isLN :: Lisp -> Bool
isLN (LN {}) = True
isLN _ = False

flatten :: Lisp -> [Lisp]
flatten (LL lisp) = lisp
flatten lisp = [lisp]

instance Monad m => Stream Lisp m Lisp where
  uncons (LL []) = return Nothing
  uncons (LL (l:ls)) = return (Just (l, (LL ls)))
  uncons ln = return (Just (ln, (LL [])))

instance WithinTraversable Lisp Lisp where
  mapWithinM f (LL ls) = 
    f =<< LL `liftM` mapM (mapWithinM f) ls
  mapWithinM f ls = f ls
  
instance Show Lisp where
  show (LN str) = str
  show (LL ls) = "(" ++ intercalate " " (map show ls) ++ ")"
