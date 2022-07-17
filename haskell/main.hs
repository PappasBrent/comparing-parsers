{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
import Control.Applicative (Alternative (empty, many, some, (<|>)))
import Data.Char (isDigit, isSpace)
import Data.Functor (($>))
import System.Exit (exitSuccess)
import System.IO (isEOF)

newtype Parser a b = Parser {runParser :: [a] -> Maybe (b, [a])}

instance Functor (Parser a) where
  fmap f p = Parser $ \s ->
    case runParser p s of
      Nothing -> Nothing
      Just (c, str) -> Just (f c, str)

instance Applicative (Parser a) where
  pure a = Parser $ \s -> Just (a, s)
  patob <*> pa = Parser $ \s -> case runParser patob s of
    Nothing -> Nothing
    Just (f, s') -> case runParser pa s' of
      Nothing -> Nothing
      Just (a, s'') -> Just (f a, s'')

instance Alternative (Parser a) where
  empty = Parser $ const Nothing
  p1 <|> p2 = Parser $ \s -> case runParser p1 s of
    Nothing -> case runParser p2 s of
      Nothing -> Nothing
      Just (a2, s') -> Just (a2, s')
    Just (a1, s') -> Just (a1, s')

instance Monad (Parser a) where
  pa >>= atopb = Parser $ \s -> case runParser pa s of
    Nothing -> Nothing
    Just (a, s') -> runParser (atopb a) s'

chainl :: Parser a b -> Parser a (b -> b -> b) -> Parser a b
p `chainl` op = p >>= rst
  where
    rst a = ((op <*> pure a <*> p) >>= rst) <|> return a

eof :: Parser a ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  c : str -> Nothing

satisfy :: Eq a => (a -> Bool) -> Parser a a
satisfy pred = Parser $ \s -> case s of
  [] -> Nothing
  x : xs -> if pred x then Just (x, xs) else Nothing

char :: Char -> Parser Char Char
char c = satisfy (== c)

digit :: Parser Char Char
digit = satisfy isDigit

integer :: Parser Char Integer
integer = read <$> some digit

spaces :: Parser Char [Char]
spaces = many $ satisfy isSpace

between :: Eq a => Parser a a -> Parser a a -> Parser a b -> Parser a b
between b e p = b *> p <* e

parens :: Parser Char b -> Parser Char b
parens = between (spaces *> char '(') (spaces *> char ')')

addsub, muldiv :: Parser Char (Integer -> Integer -> Integer)
addsub = spaces *> (((+) <$ char '+') <|> ((-) <$ char '-'))
muldiv = spaces *> (((*) <$ char '*') <|> (quot <$ char '/'))

neg :: Parser Char (Integer -> Integer)
neg = spaces *> fmap (*) (product <$> many (spaces *> (char '-' $> (-1))))

parenint :: Parser Char Integer
parenint = spaces *> (parens expr <|> integer) <* spaces

expr :: Parser Char Integer
expr = (neg <*> parenint) `chainl` muldiv `chainl` addsub

arith :: Parser Char Integer
arith = spaces *> expr <* spaces <* eof

main :: IO ()
main = do
  end <- isEOF
  if end
    then exitSuccess
    else do
      line <- getLine
      case runParser arith line of
        Nothing -> putStrLn "Bad parse" >> main
        Just (n, rst) -> print n >> main
