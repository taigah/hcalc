module Eval (

    Context,
    defaultCtx,

    eval,
    runEval,
    
    EvalError

  ) where

import Control.Monad.State
import Data.Map
import Parser
import Prelude hiding (lookup)
import Data.Tuple (swap)

type Context = Map String Number

-- Monad State Context
type Evaluation a = State Context a

type EvalError = String

eval :: AST -> Evaluation (Either EvalError Number)
eval (Assign name a) = do
  x <- eval a
  case x of
    Left err -> return $ Left err
    Right value -> do
      modify (insert name value)
      return $ Right value
eval (Function name a) = do
  x <- eval a
  case x of
    Left err -> return $ Left err
    Right value -> return $ Right $ applyFn name value
eval (Operator op l r) = do
  x <- eval l
  y <- eval r
  return $ do
    a <- x
    b <- y
    return $ applyOp op a b 
eval (Value x) = do
  return $ do
    return x
eval (Constant name) = do
  s <- get
  return $ case lookup name s of
    Nothing -> Left $ makeEvalError ("could not find constant '" ++ name ++ "'")
    Just value -> Right value

rememberAns :: Either EvalError Number -> Evaluation (Either EvalError Number)
rememberAns res = do
  case res of
    Left err -> return $ Left err
    Right ans -> do
      modify (insert "ans" ans)
      return $ Right ans
      

defaultCtx :: Context
defaultCtx = fromList [("pi", pi), ("e", exp 1)]

runEval :: AST -> Context -> (Context, Either EvalError Number)
runEval ast ctx = swap $ runState (eval ast >>= rememberAns) ctx

-- UTILS

applyOp :: Op -> Number -> Number -> Number
applyOp Add = (+)
applyOp Sub = (-)
applyOp Mult = (*)
applyOp Div = (/)
applyOp Pow = (**)

applyFn :: String -> Number -> Number
applyFn "cos" = cos
applyFn "sin" = sin
applyFn "tan" = tan
applyFn "exp" = exp

makeEvalError :: String -> EvalError
makeEvalError s = unlines [ "EvalError:", s ]

