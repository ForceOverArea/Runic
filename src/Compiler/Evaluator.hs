{-# LANGUAGE Safe #-}
module Compiler.Evaluator
    ( compileExprWithCtx
    , evaluateExpr
    , evaluateExprWithCtx
    , runCompiledWithCtx
    , SyNum
    ) where

import Data.Map ( empty )
import Data.Text ( Text )
import Compiler.Evaluator.Internal ( tokenizeExpr, Context, SyNum, Token )
import Compiler.Evaluator.Postfix ( pfeval )
import Compiler.Evaluator.Shunting ( rpnify )

{-|
    Compiles an expression to reverse polish notation, allowing an
    expression to have its context-defined values tweaked before 
    simplifying the expression to a numeric value.
-}
compileExprWithCtx :: Text -> Context -> Either String [Token]
compileExprWithCtx expr ctx = do
    compiled <- maybe errMsg Right (tokenizeExpr ctx expr)
    rpnify ctx compiled
    where
        errMsg = Left "failed to tokenize expression"

{-|
    Evaluates a compiled expression to a single numeric value with a 
    given context map. This should execute faster than using 
    evaluateExprWithCtx.
-}
runCompiledWithCtx :: [Token] -> Context -> Either String SyNum
runCompiledWithCtx = flip pfeval

{-|
    Evaluates an expression to a single numeric value with a given 
    context map. 
-}
evaluateExprWithCtx :: Text -> Context -> Either String SyNum
evaluateExprWithCtx expr ctx = do
    tokens <- compileExprWithCtx expr ctx
    rpnify ctx tokens >>= pfeval ctx

{-|
    Evaluates a given expression to a single numeric value with no 
    context, simply allowing 
-}
evaluateExpr :: Text -> Either String SyNum
evaluateExpr expr = evaluateExprWithCtx expr empty
