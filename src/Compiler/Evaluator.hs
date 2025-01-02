module Compiler.Evaluator
    ( evaluateExpr
    , evaluateExprWithCtx
    ) where

import Data.Map ( empty )
import Data.Text ( Text )
import Compiler.Evaluator.Internal ( tokenizeExpr, Context, SyNum, Token )
import Compiler.Evaluator.Postfix ( pfeval )
import Compiler.Evaluator.Shunting ( rpnify )

compileExprWithCtx :: Text -> Maybe [Token]
compileExprWithCtx = tokenizeExpr

{-|
    Evaluates an expression with a given context map. 
    FIXME: ew?
-}
evaluateExprWithCtx :: Text -> Context -> Either String SyNum
evaluateExprWithCtx expr ctx = do
    tokens <- maybe errMsg Right (tokenizeExpr ctx expr)
    rpnify ctx tokens >>= pfeval ctx
    where
        errMsg = Left "failed to tokenize expression"

{-|

-}
evaluateExpr :: Text -> Either String SyNum
evaluateExpr expr = evaluateExprWithCtx expr empty
