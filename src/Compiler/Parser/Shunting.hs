{-# LANGUAGE OverloadedStrings #-}

module Compiler.Parser.Shunting
    (
    ) where

import Prelude hiding ( words ) 
import Compiler.Parser.Types ( CtxMap )
import Control.Monad ( when )
import Control.Monad.State.Lazy ( evalStateT, get, put, StateT )
import Data.List as L ( uncons )
import Data.Map ( member )
import Data.Maybe ( fromMaybe )
import Data.Text as T ( cons, pack, uncons, unpack, words, Text )
import Text.Read ( readMaybe )

data Token
    = Num Double
    | Op Char
    | CtxVal String
    | Comma
    | LParen
    | RParen
    deriving (Eq, Show)

-- | A type alias for the state needed for a shunting yard
type ShuntingYard a = (CtxMap, [a], [a], [a])

{-|
A type alias for a state monad transformer with a shunting yard state 
included.
-}
type ShYdStateT t = StateT (ShuntingYard t)

inputQueueNotEmpty :: (Eq t, Monad m) => ShYdStateT t m Bool
inputQueueNotEmpty = do
    (_, i, _, _) <- get
    return (i /= [])

-- | Pops a token from the input queue 
popInputQueue :: Monad m => ShYdStateT t m t
popInputQueue = do
    (ctx, i, s, q) <- get
    put (ctx, tail i, s, q)
    return (head i)

tryPopInputQueue :: Monad m => ShYdStateT t m (Maybe t)
tryPopInputQueue = do
    (ctx, i, s, q) <- get
    case L.uncons i of
        Nothing -> return Nothing
        Just (x, xs) -> do
            put (ctx, i, xs, q)
            return (Just x)

stackNotEmpty :: (Eq t, Monad m) => ShYdStateT t m Bool
stackNotEmpty = do
    (_, _, s, _) <- get
    return (s /= [])

-- | Pushes a token onto the stack
pushStack :: Monad m => t -> ShYdStateT t m ()
pushStack tok = do
    (ctx, i, s, q) <- get
    put (ctx, i, tok:s, q)
    return ()

-- | Pops a token from the top of the operator stack 
popStack :: Monad m => ShYdStateT t m t
popStack = do
    (ctx, i, s, q) <- get
    put (ctx, i, tail s, q)
    return (head s)

tryPopStack :: Monad m => ShYdStateT t m (Maybe t)
tryPopStack = do
    (ctx, i, s, q) <- get
    case L.uncons s of
        Nothing -> return Nothing
        Just (x, xs) -> do
            put (ctx, i, xs, q)
            return (Just x)

-- | Pushes a token into the output queue
pushQueue :: Monad m => t -> ShYdStateT t m ()
pushQueue tok = do
    (ctx, i, s, q) <- get
    put (ctx, i, s, tok:q)
    return ()

getQueue :: Monad m => ShYdStateT t m [t]
getQueue = do
    (_, _, _, q) <- get
    return q

getCtxItem :: Text -> Monad m => ShYdStateT t m (Maybe CtxItem)
getCtxItem name = do
    (ctx, _, _, _) <- get
    return lookup name ctx

-- | The list of characters recognized as operators in an expression 
operators :: String
operators = "^*/+-"

{-| 
The list of characters that singlehandedly represent a token in an 
expression.
-}
primitiveTokens :: String
primitiveTokens = operators ++ "(),"

-- | The precedence of each operator that may be parsed
precedence :: Token -> Int
precedence (Op '^') = 4
precedence (Op '*') = 3
precedence (Op '/') = 3
precedence (Op '+') = 2
precedence (Op '-') = 2
precedence _ = 1

-- | Indicates whether the given operator is left associative.
isLeftAssoc :: Token -> Bool
isLeftAssoc (Op '^') = True
isLeftAssoc _ = False

-- | Returns the operator to the given operator token
getOperatorAsFunc :: Char -> (Double -> Double -> Double)
getOperatorAsFunc '^' = (**)
getOperatorAsFunc '*' = (*)
getOperatorAsFunc '/' = (/)
getOperatorAsFunc '+' = (+)
getOperatorAsFunc '-' = (-)
-- FIXME: is this the correct action to take here?
getOperatorAsFunc _ = error "found invalid operator"

punctuate :: Text -> Text
punctuate "" = ""
punctuate tokens =
    if x `elem` primitiveTokens
        then " " <> cons x (" " <> punctuate xs)
        else cons x (punctuate xs)
    where
        (x, xs) = fromMaybe (' ', "") (T.uncons tokens)

-- | 
tokenize :: CtxMap -> String -> Either String Token
tokenize _ "(" = Right LParen
tokenize _ ")" = Right RParen
tokenize _ "," = Right Comma
tokenize _ [] = Left "cannot tokenize an empty string"
tokenize ctx (x:xs)
    | x `elem` operators = Right $ Op x
    | member (pack $ x:xs) ctx = Right (CtxVal $ x:xs)
    | otherwise = case readMaybe (x:xs) of
        Just val -> Right $ Num val
        Nothing -> Left $ "failed to tokenize unknown value: " ++ (x:xs)

tokenizeExpr :: CtxMap -> Text -> Either String [Token]
tokenizeExpr ctx = mapM (tokenize ctx . unpack) . words . punctuate

-- | Converts a token stream from infix notation to postfix notation.
rpnify :: Monad m => CtxMap -> [Token] -> m (Either String [Token])
rpnify ctx tokens = evalStateT process (ctx, tokens, [], [])

process :: Monad m => ShYdStateT Token m (Either String [Token])
process = do
    iToken <- tryPopInputQueue
    case iToken of
        Just (Num x) -> pushQueue (Num x) >> process
        Just (Op x) -> handleOp (Op x) >> process
        Just Comma -> handleComma >> process
        Just LParen -> pushStack LParen >> process
        Just (CtxVal name) -> handleCtxValue name >> process
        Just RParen -> do
            foundRParen <- handleRParen
            if not foundRParen 
                then return (Left "found unclosed parenthesis while parsing expression")
                else process
        Nothing -> do
            sToken <- tryPopStack
            case sToken of
                Just x -> pushQueue x >> process
                Nothing -> Right <$> getQueue

handleOp :: Monad m => Token -> ShYdStateT Token m ()
handleOp o1 = do
    stackPresent <- stackNotEmpty
    when stackPresent $ do
        o2 <- popStack
        if (precedence o2 > precedence o1) || (precedence o2 == precedence o1 && isLeftAssoc o1)
            then do
                pushQueue o2
                handleOp o1
            else
                pushStack o1

handleComma :: Monad m => ShYdStateT Token m ()
handleComma = do
    stackPresent <- stackNotEmpty
    when stackPresent $ do
        o <- popStack
        when (o /= LParen) $ do
            pushQueue o
            handleComma

handleRParen :: Monad m => ShYdStateT Token m Bool
handleRParen = do
    tok <- tryPopStack
    case tok of
        Nothing -> return False
        Just LParen -> case tryPopStack of
            Just (CtxVal name) -> do
                tok2 <- getCtxItem name
                case tok2 of
                    Just Func -> 
            Nothing -> return True
        else do
            t <- popStack
            if t == LParen
                then do
                    stackPresent <- stackNotEmpty
                    when stackPresent $ do
                        t2 <- popStack
                        if t2 
                        
                else do
                    pushQueue t
                    handleRParen
