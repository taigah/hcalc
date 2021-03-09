module Main where

import qualified Text.Parsec as P
import qualified Text.Parsec.Char as P
import Data.Char
import Control.Applicative
import Control.Monad
import System.IO

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

number = token num

identifier :: Parser String
identifier = some $ P.satisfy isLower

applyFn :: String -> Number -> Maybe Number
applyFn "cos" x = Just $ cos x
applyFn "sin" x = Just $ sin x
applyFn "exp" x = Just $ exp x
applyFn _ _ = Nothing 

-- expr = func | term | term + expr | term - expr
-- func = identifier ( expr )
-- term = factor | factor * term | factor / term
-- factor = (expr) | number

expr :: Parser Number
expr = token $ func <|> do
  a <- token term
  do
    char '+'
    b <- token expr
    return $ a + b
    <|> do
      char '-'
      b <- token expr
      return $ a - b
    <|> return a

func :: Parser Number
func = token $ do
  fname <- token identifier
  char '('
  a <- token expr
  char ')'
  case applyFn fname a of
    Nothing -> P.parserFail "Unrecognized function name"
    Just result -> return result

term :: Parser Number
term = token $ do
  a <- token factor
  do
    char '*'
    b <- token term
    return $ a * b
    <|> do
      char '/'
      b <- token term
      return $ a / b
    <|> return a

factor :: Parser Number
factor = token $ number <|> do
  char '('
  a <- token expr
  char ')'
  return a

eval :: String -> Either P.ParseError Number
eval s = parse (expr <* P.eof) s

main :: IO ()
main = do
  putStrLn "hcalc 1.0"
  forever $ do
    putStr "> "
    hFlush stdout
    line <- getLine 
    let res = eval line
    case res of
      Left err -> print err
      Right value -> print value 
    return ()

