{-# LANGUAGE Safe #-}
module Parser.Math
    ( expression
    ) where

import safe Prelude hiding (const, exponent, map, product, sum)
-- import safe Parser.Internal (runicAddToCtx, runicGetFromCtx)
import safe Parser.Lang (runicTokenParser)
import safe Text.Parsec.Token (GenTokenParser(..))
import safe Text.Parsec ((<|>))
import safe Types (RnNum, RunicT)

expression :: Monad m => RunicT m RnNum
expression = do sum
    <|> difference
    <|> product
    <|> quotient
    <|> exponent
    <|> parenthetical

parenthetical :: Monad m => RunicT m RnNum
parenthetical = parens runicTokenParser expression

sum :: Monad m => RunicT m RnNum
sum = do
    lhs <- expression
    plus
    rhs <- expression
    return $ lhs + rhs

difference :: Monad m => RunicT m RnNum
difference = do
    lhs <- expression
    minus
    rhs <- expression
    return $ lhs - rhs

product :: Monad m => RunicT m RnNum
product = do
    lhs <- expression
    multiply
    rhs <- expression
    return $ lhs * rhs

quotient :: Monad m => RunicT m RnNum
quotient = do
    lhs <- expression
    divide
    rhs <- expression
    return $ lhs / rhs

exponent :: Monad m => RunicT m RnNum
exponent = do
    lhs <- expression
    exponentiate
    rhs <- expression
    return $ lhs ** rhs

plus :: Monad m => RunicT m ()
plus = reservedOp runicTokenParser "+"

minus :: Monad m => RunicT m ()
minus = reservedOp runicTokenParser "-"

multiply :: Monad m => RunicT m ()
multiply = reservedOp runicTokenParser "*"

divide :: Monad m => RunicT m ()
divide = reservedOp runicTokenParser "/"

exponentiate :: Monad m => RunicT m ()
exponentiate = reservedOp runicTokenParser "^"