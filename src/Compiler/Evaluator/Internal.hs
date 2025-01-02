{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedStrings #-}
module Compiler.Evaluator.Internal
    ( getCtxItem
    , getOp
    , getQueue
    , headStack
    , isLeftAssoc
    , popStack
    , precedence
    , pushQueue
    , pushStack
    , returnError
    , runShuntingYd
    , tokenizeExpr
    , tryGetCtxItem
    , tryPopInputQueue
    , tryPopStack
    , tryPopStackN
    , Context
    , CtxItem(..)
    , ShuntingYd
    , SyNum
    , Token(..)
    ) where

import Control.Monad.RWS ( evalRWST, RWST )
import Control.Monad.Reader ( asks )
import Control.Monad.State.Lazy ( get, put, lift )
import Data.List as L ( uncons )
import Data.Map as M ( (!), fromList, keys, lookup, member, Map )
import Data.Maybe ( fromMaybe )
import Data.Text as T ( cons, head, pack, uncons, unpack, words, Text )
import Text.Read ( readMaybe )

{-|
    Represents an item that can be added supported by the shunting 
    yard when provided in a context `Map`.
-}
data CtxItem
    = Function Int ([SyNum] -> SyNum)
    | Const SyNum

{-|
    Represents a token relevant to a shunting yard algorithm 
    implementation. 
-}
data Token
    = Num SyNum
    | Op Char
    | CtxVal Text
    | Comma
    | LParen
    | RParen
    deriving (Eq, Show)

{-|
    A `Map` of named `CtxItem`s (all are either a function or a 
    constant) used to translate custom functions or values in an expression
-}
type Context = Map Text CtxItem

{-| 
    Provides read-only access to a context for providing extended
    functionality in the shunting yard implementation.
-}
-- type ContextT = ReaderT Context

{-|
    Provides three separate stacks/queues required to implement a 
    traditional shunting yard algorithm
-}
-- type ShuntingYd = StateT ([Token], [Token], [Token]) (ContextT (Either String))
type ShuntingYd = RWST Context () ([Token], [Token], [Token]) (Either String)

{-|
    Type definition to parametrize the numeric type for the shunting yard
    (in case complex number support is added.)
-}
type SyNum = Double;

-- | A list of the operators accepted by the shunting yard.
operators :: Text
operators = foldr (<>) "" (keys operatorMap)

-- | A list of tokens understood natively by the shunting yard.
tokenLiterals :: Text
tokenLiterals = operators <> ",()"

{-|
    A map of all the operators that might be referenced by the 
    shunting yard implementation.
-}
operatorMap :: Map Text (SyNum -> SyNum -> SyNum, Int, Bool)
operatorMap = fromList 
    [ ("+", ((+),     2,  False))
    , ("-", ((-),     2,  False))
    , ("*", ((*),     3,  False))
    , ("/", ((/),     3,  False))
    , ("^", ((**),    4,  True ))
    ]

{-|

-}
opData :: Char -> Maybe (SyNum -> SyNum -> SyNum, Int, Bool)
opData = flip M.lookup operatorMap . pack . show

{-| 
    The binary operation associated with each text representation of 
    an operator.
-}
getOp :: Char -> ShuntingYd (SyNum -> SyNum -> SyNum)
getOp op = if key `member` operatorMap then
        return (fst' $ operatorMap ! key)
    else
        returnError $ "found unknown binary operator: " ++ show op
    where
        key = (pack . show) op

        fst' :: (a, b, c) -> a
        fst' (x, _, _) = x

-- | The precedence of each operator that may be parsed
precedence :: Char -> Int
precedence op = maybe 1 snd' (opData op)
    where 
        snd' :: (a, b, c) -> b
        snd' (_, x, _) = x

-- | Indicates whether the given operator is left associative.
isLeftAssoc :: Char -> Bool
isLeftAssoc op = maybe False thd (opData op)
    where 
        thd :: (a, b, c) -> c
        thd (_, _, x) = x

{-|
    Converts a value to a token for use in the shunting yard 
    implementation.
-}
tokenize :: Context -> Text -> Maybe Token
tokenize _ "," = Just Comma
tokenize _ "(" = Just LParen
tokenize _ ")" = Just RParen
tokenize ctx word
    | word `member` operatorMap = Just (Op $ T.head word)
    | word `member` ctx = Just (CtxVal word)
    | otherwise = fmap Num (readMaybe . unpack $ word)

{-|
    Wraps tokens that have a literal translation to a `Token` in 
    text format with whitespace, allowing them to be separated into 
    tokens with `Data.Text.words`.
-}
punctuate :: Text -> Text
punctuate "" = ""
punctuate expr = 
    if x `elem` unpack tokenLiterals
        then " " <> cons x (" " <> punctuate xs)
        else cons x (punctuate xs)
    where 
        (x, xs) = fromMaybe (' ', "") (T.uncons expr)

{-|
    Converts an expression given in plaintext format to a list of
    `Token`s.
-}
tokenizeExpr :: Context -> Text -> Maybe [Token]
tokenizeExpr ctx = mapM (tokenize ctx) . T.words . punctuate

{-|
    Pops a token off from the input queue, returning `Nothing` if the
    input queue was empty.
-}
tryPopInputQueue :: ShuntingYd (Maybe Token)
tryPopInputQueue = do
    (i, s, o) <- get
    let iVals = L.uncons i
    case iVals of
        Nothing -> return Nothing
        Just (iHead, iTail) -> do
            put (iTail, s, o)
            return (Just iHead)

{-|
    Pushes a token onto the operator stack in the shunting yard.
-}
pushStack :: Token -> ShuntingYd ()
pushStack tok = do
    (i, s, o) <- get
    put (i, tok:s, o)

{-|
    Pops a token off of the operator stack in the shunting yard.
-}
tryPopStack :: ShuntingYd (Maybe Token)
tryPopStack = do
    (i, s, o) <- get
    let sVals = L.uncons s
    case sVals of
        Nothing -> return Nothing
        Just (sHead, sTail) -> do
            put (i, sTail, o)
            return (Just sHead)

{-|
    Pops a number of tokens off the operator stack in the shunting 
    yard, returning an error message if the desired number of values
    could not be found.
-}
tryPopStackN :: Int -> ShuntingYd [SyNum]
tryPopStackN n = do
    (_, s, _) <- get
    let sVals = take n s
    rawArgs <- if length sVals == n then 
        return sVals
    else 
        returnError $ "expected " 
            ++ show n 
            ++ " numerical arguments, but found " 
            ++ show (length sVals)
    mapM getNumericValue rawArgs 
    where 
        getNumericValue :: Token -> ShuntingYd SyNum
        getNumericValue (Num x) = return x
        getNumericValue (CtxVal name) = do
            maybeConst <- tryGetCtxItem name
            case maybeConst of
                Just (Const x) -> return x
                _ -> returnError $ "found unrecognized token: " 
                    ++ show name 
                    ++ " in function argument"
        getNumericValue _ = do
            returnError "found non-numeric Tokens function argument"

{-|
    Returns either `Just` the head token of the stack or `Nothing` if
    the stack is empty.
-}
headStack :: ShuntingYd (Maybe Token)
headStack = do
    (_, s, _) <- get
    return (fst <$> L.uncons s)

{-|
    Pops a value off of the operator stack, throwing an error if the 
    stack happens to be emtpy. If the chance to ignore an empty stack
    is desired, see `tryPopStack`.
-}
popStack :: ShuntingYd Token
popStack = do
    (i, s, o) <- get
    case L.uncons s of
        Nothing -> do
            returnError "failed to pop a value off the operator stack"
        Just (x, xs) -> do 
            put (i, xs, o)
            return x

{-|
    Pushes a token into the output queue in the shunting yard.
-}
pushQueue :: Token -> ShuntingYd ()
pushQueue tok = do
    (i, s, o) <- get
    put (i, s, tok:o)

{-|
    Returns the state of the output queue in the shunting yard.
-}
getQueue :: ShuntingYd [Token]
getQueue = do
    (_, _, o) <- get
    return o

{-|
    Fetches the definition of an item from the context given for the
    given shunting yard instance.
-}
getCtxItem :: Text -> ShuntingYd CtxItem
getCtxItem name = do
    item <- tryGetCtxItem name
    case item of
        Just x -> return x
        Nothing -> returnError $ "given value: " 
            ++ show name
            ++ " is not defined as a function or constant in context"

{-|
    Fetches an item from the provided read-only context.
-}
tryGetCtxItem :: Text -> ShuntingYd (Maybe CtxItem)
tryGetCtxItem name = asks (M.lookup name)

{-|
    Changes the final result of the shunting yard to a `Left` 
    constructor value containing the given error message.
-}
returnError :: String -> ShuntingYd b
returnError errMsg = lift $ Left errMsg

{-|
    Populates the Contextnd initial state for a `ShuntingYd` 
    monad, returning the contained `Either` value when the shunting
    yard has been evaluated.
-}
runShuntingYd :: ShuntingYd b -> Context -> [Token] -> Either String b 
runShuntingYd md ctx tokens = fst <$> evalRWST md ctx (tokens, [], [])