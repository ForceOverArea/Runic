{-# LANGUAGE InstanceSigs #-}
module Shunting
    ( Context
    , CustomToken
    , Token
    , compileExpr
    , evalRpnExpr
    ) where

import Data.Map ( Map, (!), member )
import Prelude hiding ( lookup )
import Text.Read ( readMaybe )

import Utils ( eitherFromMaybe )

type ExprArgs = Map String Double
type Context = Map String CustomToken

data CustomToken
    = Var String
    | Const Double
    | Func String Int ([Double] -> Double)
instance Show CustomToken where
    show :: CustomToken -> String
    show (Var x) = x
    show (Func s n _) = s ++ " :: " ++ foldl (++) "Double" (replicate n "Double -> ")
    show (Const x) = show x

data Token
    = Num Double
    | CusTok CustomToken
    | Op Char
    | LParen
    | RParen
    | Comma
    deriving (Show)

operators :: String
operators = "^*/+-"

primitiveTokens :: String
primitiveTokens = operators ++ "(),"

precedence :: Token -> Int
precedence (Op '^') = 4
precedence (Op '*') = 3
precedence (Op '/') = 3
precedence (Op '+') = 2
precedence (Op '-') = 2
precedence _ = 1

isLeftAssoc :: Token -> Bool
isLeftAssoc (Op '^') = True
isLeftAssoc _ = False

getOperatorAsFunc :: Char -> (Double -> Double -> Double)
getOperatorAsFunc '^' = (**)
getOperatorAsFunc '*' = (*)
getOperatorAsFunc '/' = (/)
getOperatorAsFunc '+' = (+)
getOperatorAsFunc '-' = (-)
getOperatorAsFunc _ = error "found invalid operator"

punctuate :: String -> String
punctuate [] = []
punctuate (x:xs) =
    if x `elem` primitiveTokens
    then ' ':x:' ':punctuate xs
    else x:punctuate xs

tokenize :: Context -> String -> Either String Token
tokenize _ "(" = Right LParen
tokenize _ ")" = Right RParen
tokenize _ "," = Right Comma
tokenize _ [] = Left "cannot tokenize an empty string"
tokenize ctx (x:xs)
    | x `elem` operators = Right $ Op x
    | member (x:xs) ctx = Right $ CusTok $ ctx ! (x:xs)
    | otherwise = do
        x1 <- let errMsg = "failed to tokenize unknown value: " ++ (x:xs)
            in eitherFromMaybe errMsg $ readMaybe (x:xs)
        Right $ Num x1

rpnify :: [Token] -> Either String [Token]
rpnify input = shntngYrd input [] []
    where
        shntngYrd :: [Token] -> [Token] -> [Token] -> Either String [Token]
        shntngYrd [] [] q = Right $ reverse q
        shntngYrd [] (LParen:_) _ = Left "found mismatched left parenthesis in shunting yard"
        shntngYrd [] (RParen:_) _ = Left "found mismatched right parenthesis in shunting yard"

        shntngYrd [] (s:ss) q = shntngYrd [] ss (s:q)

        shntngYrd ((Num x):xs) s q = shntngYrd xs s (Num x:q)
        shntngYrd (CusTok (Var x):xs) s q = shntngYrd xs s (CusTok (Var x):q)
        shntngYrd (CusTok (Const x):xs) s q = shntngYrd xs s (Num x:q)

        shntngYrd (CusTok (Func name i x):xs) s q = shntngYrd' xs (CusTok (Func name i x):s) q

        shntngYrd (Op x:xs) s q = processOperators (Op x:xs) s q

        shntngYrd (Comma:_) [] _ = Left "failed to find a token after a comma"
        shntngYrd (Comma:xs) (LParen:ss) q = shntngYrd' xs ss q
        shntngYrd (Comma:xs) (s:ss) q = shntngYrd' (Comma:xs) ss (s:q)

        shntngYrd (LParen:xs) s q = shntngYrd' xs (Comma:s) q

        shntngYrd (RParen:_) [] _ = Left "failed to close parenthesis in shunting yard"
        shntngYrd (RParen:xs) (LParen:(CusTok (Func name i s)):ss) q = shntngYrd xs ss (CusTok (Func name i s):q)
        shntngYrd (RParen:xs) (LParen:ss) q = shntngYrd xs ss q
        shntngYrd (RParen:xs) (s:ss) q = shntngYrd (RParen:xs) ss (s:q)

        -- Uses alternate logic for unary minus handling
        shntngYrd' :: [Token] -> [Token] -> [Token] -> Either String [Token]
        shntngYrd' (Op '-':xs) s q = shntngYrd' xs (Op '*':s) (Num (-1.0):q)
        shntngYrd' x s q = shntngYrd x s q

        processOperators :: [Token] -> [Token] -> [Token] -> Either String [Token]
        processOperators [] _ _ = Left "cannot process operators when input queue is empty"
        processOperators (o1:xs) [] q = shntngYrd' xs [o1] q
        processOperators (o1:xs) (LParen:ss) q = shntngYrd' xs (o1:LParen:ss) q
        processOperators (o1:xs) (o2:ss) q =
            if (precedence o2 > precedence o1) || (precedence o1 == precedence o2 && isLeftAssoc o1)
            then processOperators (o1:xs) ss (o2:q)
            else shntngYrd' xs (o1:LParen:ss) q

evalRpnExpr :: [Token] -> ExprArgs -> Either String Double
evalRpnExpr tokens ctx = pfEval tokens [] ctx
    where
        pfEval :: [Token] -> [Double] -> ExprArgs -> Either String Double
        pfEval [] [x] _ = Right x -- This is the state of the output stack when evaluation is complete
        pfEval [] _ _ = Left "cannot evaluate empty token stack"
        pfEval (Num q:qs) s _ = pfEval qs (q:s) ctx
        pfEval (CusTok (Const q):qs) s _ = pfEval qs (q:s) ctx
        pfEval (CusTok (Var name):qs) s _ =
            if member name ctx
            then pfEval qs ((ctx ! name):s) ctx
            else Left $ "could not find variable '" ++ name ++ "' in given context"
        pfEval (Op o:_) [] _ = Left $ "cannot apply binary operator " ++ [o] ++ " to empty value stack"
        pfEval (Op o:_) [_] _ = Left $ "cannot apply binary operator " ++ [o] ++ " to a single value"
        pfEval (Op o:qs) (n1:n2:ss) _ = pfEval qs (getOperatorAsFunc o n1 n2:ss) ctx
        pfEval (CusTok (Func _ i f):qs) s _ = pfEval qs (f (take i s) : drop i s) ctx
        pfEval token _ _ = Left $ "found unexpected token: " ++ show token ++ " while evaluating postfix expression"

compileExpr :: String -> Context -> Either String [Token]
compileExpr expr ctx = do
    tokens <- getTokens expr;
    rpnify tokens
    where
        getTokens :: String -> Either String [Token]
        getTokens = mapM (tokenize ctx) . words . punctuate
