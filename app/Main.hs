module Main where

import Eval
import Parser
import System.IO

main :: IO ()
main = f defaultCtx
  where
    f :: Context -> IO ()
    f ctx = do
      expr <- prompt

      newCtx <- case parse expr of
        Left err -> do
          putStrLn err
          return ctx
        Right ast -> do
          let (newCtx, res) = runEval ast ctx
          case res of
            Left err -> putStrLn err
            Right value -> print value
          return newCtx

      f newCtx

prompt :: IO String
prompt = do
  putStr "> "
  hFlush stdout
  s <- getLine
  case s of
    "" -> prompt
    s -> return s

