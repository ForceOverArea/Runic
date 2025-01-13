{-# LANGUAGE Safe #-}
module Compiler.Evaluator
    ( compileExprWithCtx
    , evaluateExpr
    , evaluateExprWithCtx
    , runCompiledWithCtx
    ) where

import safe Data.Map (empty)
import safe Compiler.Evaluator.Internal (tokenizeExpr, Context, RnNum, Token)
import safe Compiler.Evaluator.Postfix (pfeval)
import safe Compiler.Evaluator.Shunting (rpnify)

-- | Compiles an expression to reverse polish notation, allowing an
--   expression to have its context-defined values tweaked before 
--   simplifying the expression to a numeric value.
compileExprWithCtx :: String -> Context -> Either String [Token]
compileExprWithCtx expr ctx = do
    compiled <- maybe errMsg Right (tokenizeExpr ctx expr)
    rpnify ctx compiled
    where
        errMsg = Left "failed to tokenize expression"

-- | Evaluates a compiled expression to a single numeric value with a 
--   given context map. This should execute faster than using 
--   evaluateExprWithCtx.
runCompiledWithCtx :: [Token] -> Context -> Either String RnNum
runCompiledWithCtx = flip pfeval

-- | Evaluates an expression to a single numeric value with a given 
--   context map. 
evaluateExprWithCtx :: String -> Context -> Either String RnNum
evaluateExprWithCtx expr ctx = do
    tokens <- compileExprWithCtx expr ctx
    rpnify ctx tokens >>= pfeval ctx

-- | Evaluates a given expression to a single numeric value with no 
--   context, simply allowing 
evaluateExpr :: String -> Either String RnNum
evaluateExpr expr = evaluateExprWithCtx expr empty
