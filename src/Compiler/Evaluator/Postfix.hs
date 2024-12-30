module Compiler.Evaluator.Postfix 
    ( pfeval
    ) where

import Compiler.Evaluator.Internal
    ( getCtxItem
    , getOp
    , headStack
    , pushStack
    , returnError
    , runShuntingYd
    , tryPopInputQueue
    , tryPopStack
    , tryPopStackN
    , Context
    , CtxItem(..)
    , ShuntingYd
    , SyNum
    , Token(..)
    )

{-|
    Kicks off a postfix evaluator given a context Map, an input
    token queue and an action shunting yard monad.
-}
pfeval :: Context -> [Token] -> Either String SyNum
pfeval = runShuntingYd procPf

{-|
    The internal implementation of the postfix evaluator.
-}
procPf :: ShuntingYd SyNum
procPf = do
    possTok <- tryPopInputQueue
    case possTok of
        Nothing -> returnVal
        Just tok -> do
            _ <- case tok of
                Num x -> pushStack (Num x)
                Op x -> applyOp x
                CtxVal x -> do
                    item <- getCtxItem x
                    case item of 
                        Function n f -> applyFunction n f
                        Const c -> pushStack (Num c)
                _ -> returnError ""
            procPf

{-|
    Handles reporting errors or fetching the evaluated value from the 
    stack once the input token queue is empty.
-}
returnVal :: ShuntingYd SyNum
returnVal = do
    result <- tryPopStack
    emptyStack <- headStack
    case (result, emptyStack) of
        (Just (Num res), Nothing) -> return res
        (_, _) -> returnError $ "expected only one numeric value \
            \after evaluating compiled infix expression, found: " 
            ++ show emptyStack 
            ++ " and possibly other values left over after evaluation"

{-|
    Applies an operator to the 2 numeric arguments at the top of the 
    operator stack.
-}
applyOp :: Char -> ShuntingYd ()
applyOp op = do
    operands <- tryPopStackN 2
    operation <- getOp op
    pushStack (Num $ head operands `operation` (operands !! 1) )

{-|
    Applies a function to as many arguments as it takes at the top of 
    the operator stack.
-}
applyFunction :: Int -> ([SyNum] -> SyNum) -> ShuntingYd ()
applyFunction n f = do
    args <- tryPopStackN n
    pushStack (Num $ f args)
