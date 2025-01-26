{-# LANGUAGE Safe #-}
module Parser.Math
    ( expression
    , sum
    ) where

import safe Prelude hiding (const, exponent, map, product, sum)
import safe Parser.Internal (runicGetFromCtx)
import safe Parser.Lang (runicTokenParser)
import safe Parser.Units (conversion')
import safe Text.Parsec ((<|>), (<?>), many1, try)
import safe Text.Parsec.Token (float, hexadecimal, integer, GenTokenParser(..))
import safe Types (CtxItem(..), RnNum, RunicT)

expression :: Monad m => RunicT m RnNum
expression = p4Term

exponent :: Monad m => RunicT m RnNum
exponent = do
    lhs <- p0Term
    foldr (**) lhs <$> many1 (exponentiate >> p0Term)

quotient :: Monad m => RunicT m RnNum
quotient = do
    lhs <- p1Term 
    foldl (/) lhs <$> many1 (divide >> p1Term) 

product :: Monad m => RunicT m RnNum
product = do
    lhs <- p2Term
    foldl (*) lhs <$> many1 (multiply >> p2Term)

sum :: Monad m => RunicT m RnNum
sum = do
    lhs <- p3Term
    foldl (+) lhs <$> many1 term
    where 
        term = try (plus >> p3Term) 
            <|> (minus >> (* (-1)) <$> p3Term)

-- different basic terms -- TODO: make a fold to autogen this

p0Term :: Monad m => RunicT m RnNum
p0Term = try parenthetical <|> numberLike

p1Term :: Monad m => RunicT m RnNum
p1Term = try exponent <|> p0Term

p2Term :: Monad m => RunicT m RnNum
p2Term = try quotient <|> p1Term

p3Term :: Monad m => RunicT m RnNum
p3Term = try product <|> p2Term

p4Term :: Monad m => RunicT m RnNum
p4Term = try sum <|> p3Term

-- different basic operations/literals

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

parenthetical :: Monad m => RunicT m RnNum
parenthetical = parens runicTokenParser expression

numberLike :: Monad m => RunicT m RnNum
numberLike 
    = do try number
    <|> try variable
    <|> try conversion'
    -- <|> function

number :: Monad m => RunicT m RnNum
number 
    = do try $ float runicTokenParser 
    <|> fromInteger <$> (try (integer runicTokenParser) <|> hexadecimal runicTokenParser)
    <?> "number"

variable :: Monad m => RunicT m RnNum
variable = do
    name <- identifier runicTokenParser
    value <- runicGetFromCtx name
    case value of
        Just (Const v) -> return v
        Just (Variable v _ _) -> return v
        _ -> fail "TODO: add parsec error reporting here! (Math.hs line 33)"