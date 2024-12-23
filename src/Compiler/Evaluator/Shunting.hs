{-# LANGUAGE OverloadedStrings #-}

module Compiler.Evaluator.Shunting
    ( primitiveTokens
    , rpnify
    , tokenize
    ) where

import Prelude hiding ( lookup, words ) 
import Compiler.Evaluator.Types ( CtxMap, CtxItem(..) )
import Control.Monad ( when )
import Control.Monad.State.Lazy ( evalStateT, get, put, StateT )
import Data.List as L ( uncons )
import Data.Map ( lookup, member )
import Data.Maybe ( fromMaybe )
import Data.Text as T ( cons, pack, uncons, unpack, words, Text )
import Text.Read ( readMaybe )

data Token
    = Num Double
    | Op Char
    | CtxVal Text
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

tryPopInputQueue :: Monad m => ShYdStateT t m (Maybe t)
tryPopInputQueue = do
    (ctx, i, _, q) <- get
    case L.uncons i of
        Nothing -> return Nothing
        Just (x, xs) -> do
            put (ctx, i, xs, q)
            return (Just x)

-- | Pushes a token onto the stack
pushStack :: Monad m => t -> ShYdStateT t m ()
pushStack tok = do
    (ctx, i, s, q) <- get
    put (ctx, i, tok:s, q)
    return ()

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

getCtxItem :: Monad m => Text -> ShYdStateT t m (Maybe CtxItem)
getCtxItem name = do
    (ctx, _, _, _) <- get
    return (lookup name ctx)

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

-- | Converts a string to a token (TODO: remove String from this file)
tokenize :: CtxMap -> String -> Either String Token
tokenize _ "(" = Right LParen
tokenize _ ")" = Right RParen
tokenize _ "," = Right Comma
tokenize _ [] = Left "cannot tokenize an empty string"
tokenize ctx (x:xs)
    | x `elem` operators = Right $ Op x
    | member (pack $ x:xs) ctx = Right (CtxVal . pack $ x:xs)
    | otherwise = case readMaybe (x:xs) of
        Just val -> Right $ Num val
        Nothing -> Left $ "failed to tokenize unknown value: " ++ (x:xs)

tokenizeExpr :: CtxMap -> Text -> Either String [Token]
tokenizeExpr ctx = mapM (tokenize ctx . unpack) . words . punctuate

-- | Converts a token stream from infix notation to postfix notation.
rpnify :: Monad m => CtxMap -> [Token] -> m (Either String [Token])
rpnify ctx tokens = evalStateT procSy (ctx, tokens, [], [])

{-| 
### Process Shunting Yard
The internal control flow of the shunting yard algorithm.
-}
procSy :: Monad m => ShYdStateT Token m (Either String [Token])
procSy = do
    iToken <- tryPopInputQueue
    case iToken of
        Just (Num x) -> Right <$> pushQueue (Num x) >> procSy
        Just (Op x) -> handleOp (Op x) >> procSy
        Just (CtxVal name) -> handleCtxValue name >> procSy
        Just Comma -> handleComma >> procSy
        Just LParen -> pushStack LParen >> procSy
        Just RParen -> do
            foundRParen <- handleRParen
            if not foundRParen 
                then return (Left "found unclosed parenthesis while parsing expression")
                else procSy
        Nothing -> do
            sToken <- tryPopStack
            case sToken of
                Just x -> pushQueue x >> procSy
                Nothing -> Right <$> getQueue
      
handleOp :: Monad m => Token -> ShYdStateT Token m ()
handleOp o1 = do
    x <- tryPopStack
    case x of 
        Nothing -> return ()
        Just o2 -> if (precedence o2 > precedence o1) 
            || (precedence o2 == precedence o1 && isLeftAssoc o1)
                then do
                    pushQueue o2
                    handleOp o1
                else
                    pushStack o1

handleCtxValue :: Monad m => Text -> ShYdStateT Token m Bool
handleCtxValue name = do
    item <- getCtxItem name
    case item of 
        Nothing -> return False
        Just x -> do 
            case x of
                (CtxFunction {}) -> pushStack (CtxVal name)
                _ -> pushQueue (CtxVal name)
            return True

handleComma :: Monad m => ShYdStateT Token m ()
handleComma = do
    o <- tryPopStack
    case o of
        Nothing -> return ()
        Just LParen -> return ()
        Just x -> do
            pushQueue x
            handleComma
 
handleRParen :: Monad m => ShYdStateT Token m Bool
handleRParen = do
    tok <- tryPopStack
    case tok of
        Nothing -> return False
        Just LParen -> tryPopStack >>= handlePossibleFunction 
        Just x -> do
            pushQueue x
            handleRParen
            
handlePossibleFunction :: Monad m => Maybe Token -> ShYdStateT Token m Bool
handlePossibleFunction maybeTok = 
    case maybeTok of
        Nothing -> return True
        Just (CtxVal name) -> do
            tok <- getCtxItem name
            case tok of
                Just (CtxFunction {}) -> pushQueue (CtxVal name)
                Just _ -> pushStack (CtxVal name)
                Nothing -> return ()
            return True
        Just tok -> do
            pushStack tok
            return True

postfixEval :: Monad m => CtxMap -> [Token] -> m (Either String Double)
postfixEval ctx tokens = evalStateT procPf (ctx, tokens, [], [])

{-| 
### Process PostFix
Evaluates the postfix representation of an expression returned by 
the `rpnify` function.
-}
procPf :: Monad m => ShYdStateT Token m (Either String Double)
procPf = do
    iToken <- tryPopInputQueue
    case iToken of
        Just (Num x) -> pushStack (Num x) >> procPf
        Just (Op x) -> handleOpPf x >> procPf
        Just (CtxVal name) -> handleCtxValuePf name >> process

{-|

-}
handleOpPf :: Monad m => Char -> ShYdStateT Token m ()
handleOpPf x = do
    o1 <- tryPopStack
    o2 <- tryPopStack
    pushStack (getOperatorAsFunc x o1 o2)

{-|

-}
handleCtxValuePf :: Monad m => Text -> ShYdStateT Token m Bool
handleCtxValuePf name = do
    possItem <- getCtxItem name
    case possItem of
        Nothing -> return False
        Just (CtxGuessDmn val guess mn mx) -> 
        Just (CtxFunction name argc f) ->
        Just (CtxConst name val) ->