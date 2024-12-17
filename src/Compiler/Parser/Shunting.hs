{-# LANGUAGE OverloadedStrings #-}

module Compiler.Parser.Shunting
    (
    ) where

import Prelude hiding ( words )
import Compiler.Parser.Types ( CtxMap )
import Control.Monad.State.Lazy ( get, put, StateT )
import Data.Map ( member )
import Data.Maybe ( fromMaybe )
import Data.Text ( cons, pack, uncons, unpack, words, Text )
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
type ShuntingYard a = ([a], [a], [a])

{-|
A type alias for a state monad transformer with a shunting yard state 
included.
-}
type ShYdStateT t = StateT (ShuntingYard t)

-- | Pops a token from the input queue 
popInputQueue :: Monad m => ShYdStateT t m t
popInputQueue = do
    (i, s, q) <- get
    put (tail i, s, q)
    return (head i)

-- | Pushes a token onto the stack
pushStack :: Monad m => t -> ShYdStateT t m ()
pushStack tok = do
    (i, s, q) <- get
    put (i, tok:s, q)
    return ()

-- | Pops a token from the top of the operator stack 
popStack :: Monad m => ShYdStateT t m t
popStack = do
    (i, s, q) <- get
    put (i, tail s, q)
    return (head s)

-- | Pushes a token into the output queue
pushQueue :: Monad m => t -> ShYdStateT t m ()
pushQueue tok = do
    (i, s, q) <- get
    put (i, s, tok:q)
    return ()

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
        (x, xs) = fromMaybe (' ', "") (uncons tokens)

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