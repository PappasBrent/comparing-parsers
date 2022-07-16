{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
import Control.Applicative (Alternative (empty, many, some, (<|>)))
import Data.Char (isDigit, isSpace)
import System.Exit (exitSuccess)
import System.IO (isEOF)

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

instance Functor Parser where
  fmap f p = Parser $ \s ->
    case runParser p s of
      Nothing -> Nothing
      Just (c, str) -> Just (f c, str)

instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)
  patob <*> pa = Parser $ \s -> case runParser patob s of
    Nothing -> Nothing
    Just (f, s') -> case runParser pa s' of
      Nothing -> Nothing
      Just (a, s'') -> Just (f a, s'')

instance Alternative Parser where
  empty = Parser $ const Nothing
  p1 <|> p2 = Parser $ \s -> case runParser p1 s of
    Nothing -> case runParser p2 s of
      Nothing -> Nothing
      Just (a2, s') -> Just (a2, s')
    Just (a1, s') -> Just (a1, s')

instance Monad Parser where
  pa >>= atopb = Parser $ \s -> case runParser pa s of
    Nothing -> Nothing
    Just (a, s') -> runParser (atopb a) s'

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = p >>= rst
  where
    rst a = ((op <*> pure a <*> p) >>= rst) <|> return a

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainr1` op = p >>= rst
  where
    rst a = ((op <*> p <*> pure a) >>= rst) <|> return a

eof :: Parser [a]
eof = Parser $ \s -> case s of
  [] -> Just ([], [])
  c : str -> Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = Parser $ \s -> case s of
  [] -> Nothing
  c : str -> if pred c then Just (c, str) else Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

digit :: Parser Char
digit = satisfy isDigit

integer :: Parser Integer
integer = read <$> some digit

spaces :: Parser String
spaces = many $ satisfy isSpace

between :: Char -> Char -> Parser a -> Parser a
between b e p = char b *> p <* char e

parens :: Parser a -> Parser a
parens = between '(' ')'

addsub, muldiv :: Parser (Integer -> Integer -> Integer)
addsub = spaces *> (((+) <$ char '+') <|> ((-) <$ char '-'))
muldiv = spaces *> (((*) <$ char '*') <|> (quot <$ char '/'))

unop :: Parser Integer
unop = product <$> many (spaces *> (char '-' >> pure (-1)))

expr :: Parser Integer
expr = spaces *> ((*) <$> unop <*> factor) `chainl1` muldiv `chainl1` addsub

factor :: Parser Integer
factor = spaces *> (parens expr <|> integer)

interpret :: Parser Integer
interpret = expr <* spaces <* eof

main :: IO ()
main = do
  end <- isEOF
  if end
    then exitSuccess
    else do
      line <- getLine
      case runParser interpret line of
        Nothing -> putStrLn "Bad parse" >> main
        Just (n, rst) -> print n >> main
