module Main where

import qualified Text.Parsec as P
import qualified Text.Parsec.Char as P
import Data.Char
import Control.Applicative
import Control.Monad
import System.IO
import Data.Map.Strict

type Number = Double

type Parser t = P.Parsec String () t

parse :: Parser t -> String -> Either P.ParseError t
parse p s = P.parse p "REPL" s

spaces :: Parser ()
spaces = P.spaces

char :: Char -> Parser Char
char c = P.satisfy (==c)

token p = spaces *> p <* spaces

num :: Parser Number
num = fmap read $ some $ P.satisfy isDigit

number :: Parser Number
number = token num

identifier :: Parser String
identifier = some $ P.satisfy isLower

data AST =
  Assign String AST |
  Function String AST |
  Operator (Number -> Number -> Number) AST AST |
  Value Number |
  Constant String

eval :: AST -> Context -> Either String Number
eval (Value n) _ = Right n
eval (Constant name) ctx = case Data.Map.Strict.lookup name ctx of
    Nothing -> Left "Constant not found"
    Just value -> Right value

eval (Assign _ v) ctx = eval v ctx
eval (Operator op a b) ctx = (pure op) <*> (eval a ctx) <*> (eval b ctx)
eval (Function "cos" x) ctx = cos <$> (eval x ctx)
eval (Function "sin" x) ctx = sin <$> (eval x ctx)
eval (Function "tan" x) ctx = tan <$> (eval x ctx)
eval (Function "exp" x) ctx = exp <$> (eval x ctx)
eval (Function name _) _ = Left $ "Function '" ++ name ++ "' does not exist"

-- statement = (expr | assignment) eof
-- expr = term | term + expr | term - expr
-- term = factor | factor * term | factor / term
-- factor = "(" expr ")" | number | funcOrConst
-- funcOrConst = identifier (ε | "(" expr ")")

statement :: Parser AST
statement = do
  a <- (P.try assignment) <|> expr
  P.eof
  return a

assignment :: Parser AST
assignment = token $ do
  name <- token identifier
  char '='
  a <- token expr
  return $ Assign name a

expr :: Parser AST
expr = token $ do
  a <- token term
  do
    char '+'
    b <- token expr
    return $ Operator (+) a b
    <|> do
      char '-'
      b <- token expr
      return $ Operator (-) a b
    <|> return a

term :: Parser AST
term = token $ do
  a <- token factor
  do
    char '*'
    b <- token term
    return $ Operator (*) a b
    <|> do
      char '/'
      b <- token term
      return $ Operator (/) a b
    <|> return a

factor :: Parser AST
factor = token $ (Value <$> number) <|> funcOrConst <|> do
  char '('
  a <- token expr
  char ')'
  return a

funcOrConst :: Parser AST
funcOrConst = token $ do
  name <- identifier
  (do
    char '('
    a <- token expr
    char ')'
    return $ Function name a
    ) <|> (return $ Constant name)

type Context = Map String Number

ctx :: Context
ctx = fromList [("pi", pi), ("e", exp(1))]

parsed = parse expr "1+pi"

prompt :: IO String
prompt = do
  putStr "> "
  hFlush stdout
  line <- getLine
  if line == "" then prompt else return line

main :: IO ()
main = do
  putStrLn "hcalc 1.0"
  forever $ do
    line <- prompt
    let res = parse statement line
    case res of
      Left err -> print err
      Right ast -> case eval ast ctx of
        Left err -> putStrLn err
        Right res -> print res
    return ()

