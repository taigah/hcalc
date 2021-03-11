module Main where

import Eval
import Parser
import System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings (loop defaultCtx)
  where
    loop :: Context -> InputT IO ()
    loop ctx = do
      expr <- prompt

      newCtx <- case parse expr of
        Left err -> do
          outputStrLn err
          return ctx
        Right ast -> do
          let (newCtx, res) = runEval ast ctx
          case res of
            Left err -> outputStrLn err
            Right value -> outputStrLn $ show value
          return newCtx

      loop newCtx

prompt :: InputT IO String
prompt = do
  s <- getInputLine "> "
  case s of
    Nothing -> prompt
    Just "" -> prompt
    Just s -> return s

