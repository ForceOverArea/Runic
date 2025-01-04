{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedStrings #-}
module Compiler.Evaluator.Shunting 
    ( rpnify
    ) where

import Control.Monad ( when )
import Data.Text ( Text )
import Compiler.Evaluator.Internal
    ( getCtxItem
    , getQueue
    , headStack
    , isLeftAssoc
    , popStack
    , precedence
    , pushQueue
    , pushStack
    , returnError
    , runShuntingYd
    , tryGetCtxItem
    , tryPopInputQueue
    , tryPopStack
    , Context
    , CtxItem(..)
    , ShuntingYd
    , Token(..)
    )

{-|
    Kicks off a shunting yard monad given a context Map, an input
    token queue and an action shunting yard monad.
-}
rpnify :: Context -> [Token] -> Either String [Token] 
rpnify = runShuntingYd procSy

{-|
    The internal implementation of the shunting yard.
-}
procSy :: ShuntingYd [Token]
procSy = do
    possTok <- tryPopInputQueue
    case possTok of
        Nothing -> emptyStack
        Just tok -> do
            _ <- case tok of
                Num x -> pushQueue (Num x)
                Op x -> handleOp x
                CtxVal x -> handleCtxVal x
                Comma -> handleComma
                LParen -> pushStack LParen
                RParen -> handleRParen
            procSy

{-|
    Handles emptying the operator stack once the input queue has been 
    exhausted by the shunting yard.
-}
emptyStack :: ShuntingYd [Token]
emptyStack = do
    possTok <- tryPopStack
    case possTok of
        Just LParen -> returnError "Found an unmatched parenthesis \
            \while trying parsing infix expression"
        Just tok -> pushQueue tok >> emptyStack
        Nothing -> getQueue

{-|
    Handles sorting binary operators through the shunting yard 
    correctly.
-}
handleOp :: Char -> ShuntingYd ()
handleOp o1 = do
    possTok <- headStack
    case possTok of 
        Just (Op o2) -> when (comparePrec o2) 
            $ popStack >>= pushQueue >> handleOp o1
        _ -> pushQueue (Op o1)
    where
        comparePrec :: Char -> Bool
        comparePrec o2 = (precedence o2 > precedence o1) 
            || ((precedence o2 == precedence o1) 
            && isLeftAssoc o1)

{-|
    Handles fetching values from context (functions or constants)
    and sorting them into where they should go correctly. This 
    function also implements error handling for values that are not 
    found in the given context.
-}
handleCtxVal :: Text -> ShuntingYd ()
handleCtxVal itemName = do
    possItem <- getCtxItem itemName
    case possItem of
        Const _ -> pushQueue (CtxVal itemName)
        Function _ _ -> pushStack (CtxVal itemName)

{-|
    Handles when a comma is found in the shunting yard algorithm
    by popping tokens (except left parenthesis) into the output
    queue to be processed by a postfix expression evaluator.
-}
handleComma :: ShuntingYd ()
handleComma = do
    possTok <- headStack
    case possTok of 
        Nothing -> return ()
        Just LParen -> return ()
        Just _ -> popStack >>= pushQueue >> handleComma

{-|
    Handles when a right parenthesis is found in the shunting yard
    algorithm by popping values (expcept left parenthesis) into the 
    output queue to be processed by a postfix expression evaluator.
-}
handleRParen :: ShuntingYd ()
handleRParen = do
    possTok <- tryPopStack
    case possTok of 
        Nothing -> returnError "found unclosed parenthesis"
        Just LParen -> handlePossibleFunction
        Just tok -> pushQueue tok >> handleRParen

{-|
    Handles a possible function found preceding a left parenthesis
    by popping it into the output queue for later processing.
-}
handlePossibleFunction :: ShuntingYd ()
handlePossibleFunction = do
    possTok <- headStack
    case possTok of
        Just (CtxVal name) -> do 
            possFunc <- tryGetCtxItem name
            case possFunc of 
                Just (Function _ _) -> popStack >>= pushQueue
                _ -> return ()
        _ -> return ()
