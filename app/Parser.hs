module Parser (

    parse,

    Number,
    AST (Assign, Function, Operator, Value, Constant),
    Op (Add, Mult, Pow),
    ParseError

    ) where

import qualified Text.Parsec as P
import Text.Parsec ((<?>))
import qualified Text.Parsec.Char as P
import Control.Applicative ((*>), (<*), some, many, (<|>))
import Data.Char

parse :: String -> Either ParseError AST
parse s = parserRun statement s

data AST =
  Assign String AST |
  Function String AST |
  Operator Op AST AST |
  Value Number |
  Constant String deriving (Show)

data Op = Add | Mult | Pow deriving (Show, Eq)

type Number = Double

-- FORMAL GRAMMAR
-- 
-- statement = (assignment | expr) eof
-- assignment = identifier = expr
-- expr = term (+ expr | - expr | ε)
-- term = factor (* term | / term | ε)
-- factor = unit (^ factor | ε)
-- unit = parenthesizedExpression | number | funcOrConst
-- parenthesizedExpression = "(" expr ")"
-- funcOrConst = identifier (parenthesizedExpression | ε)

statement :: Parser AST
statement = do
  result <- (P.try assignment) <|> expr
  eof
  return result

assignment :: Parser AST
assignment = do
  name <- token identifier
  char '='
  a <- token expr
  return $ Assign name a
  

expr :: Parser AST
expr = do
  a <- token term
  do
    sym <- char '+' <|> char '-'
    b <- token expr
    let b' = if sym == '+' then b else Function "neg" b
    return $ inverseAssociativityRTL $ Operator Add a b'
    <|> return a
    <?> "expression"

term :: Parser AST
term = do
  a <- token factor
  do
    sym <- char '*' <|> char '/'
    b <- token expr
    let b' = if sym == '*' then b else Function "inv" b
    return $ inverseAssociativityRTL $ Operator Mult a b'
    <|> return a
    <?> "term"

factor :: Parser AST
factor = do
  a <- token unit
  do
    char '^'
    b <- token factor
    return $ Operator Pow a b
    <|> return a
    <?> "factor"


unit :: Parser AST
unit = parenthesizedExpression
    <|> do
      a <- token number
      return $ Value a
    <|> funcOrConst

parenthesizedExpression :: Parser AST
parenthesizedExpression = do
  char '('
  a <- token expr
  char ')'
  return a
  <?> "parenthesized expression"

funcOrConst :: Parser AST
funcOrConst = do
  name <- token identifier
  do
    a <- token parenthesizedExpression
    return $ Function name a
    <|> do
      return $ Constant name

-- BASIC PARSER COMBINATORS

type Parser t = P.Parsec String () t

type ParseError = String

number :: Parser Number
number = fmap read $ some $ P.satisfy isDigit

identifier :: Parser String
identifier = some $ P.satisfy isLower

token :: Parser a -> Parser a
token p = spaces *> p <* spaces

spaces :: Parser ()
spaces = P.spaces

char :: Char -> Parser Char
char c = P.satisfy (==c)

eof :: Parser ()
eof = P.eof

parserRun :: Parser a -> String -> Either ParseError a
parserRun p s = case P.parse p "" s of
  Left err -> Left $ unlines ["ParseError:", show err]
  Right result -> Right result

-- UTILS

inverseAssociativityRTL :: AST -> AST
inverseAssociativityRTL full@(Operator op l (Operator op2 l' r))
  | op == op2 = Operator op (Operator op l l') r
  | otherwise = full
inverseAssociativityRTL full = full

