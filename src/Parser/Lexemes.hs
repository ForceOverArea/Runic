{-# LANGUAGE Safe #-}
module ParseTypes.Lexemes 
    ( constDecl
    , domainDecl
    , guessDecl
    , keep
    , on
    ) where

import safe Prelude hiding (const, map)
import safe Data.Map (map)
import safe Parser.Internal (runicAddToCtx, runicGetFromCtx)
import safe Parser.Lang (runicTokenParser)
import safe Text.Parsec (anyChar, char, endOfLine, getState, manyTill)
import safe Text.Parsec.Prim (try)
import safe Text.Parsec.Token (GenTokenParser (comma, identifier, operator, reserved))
import safe Types (CtxItem(..), RnNum, RunicT)

-- | Matches a domain expression, returning the variable name 
--   associated with the guess expression.
domainDecl :: RunicT m ()
domainDecl = do
    keep
    name <- identifier runicTokenParser
    on
    domain <- domainBounds
    _ <- endOfLine
    existing <- runicGetFromCtx name
    case existing of
        Nothing 
            -> runicAddToCtx name $ Variable 1 Nothing (Just domain)
        Just (Variable v g Nothing) 
            -> runicAddToCtx name $ Variable v g (Just domain)
        _ -> error "ligma balls"

-- | Matches a guess expression, returning the variable name 
--   associated with the guess expression.
guessDecl :: RunicT m ()
guessDecl = do
    guess
    value <- mathExprEndingWith for
    name <- identifier runicTokenParser
    existing <- runicGetFromCtx name
    case existing of
        Nothing 
            -> runicAddToCtx name $ Variable 1 (Just value) Nothing
        Just (Variable v Nothing d) 
            -> runicAddToCtx name $ Variable v (Just value) d
        _ -> error "ligma balls"

-- | Matches a const declaration, returning the constant's name 
--   and numeric value.
constDecl :: RunicT m ()
constDecl = do
    const
    name <- identifier runicTokenParser
    _ <- operator runicTokenParser
    value <- mathExprEndingWith endOfLine
    existing <- runicGetFromCtx name
    case existing of
        Nothing -> runicAddToCtx name $ Types.Const value
        Just _ -> error "ligma balls"

-- | Matches the keep keyword in the Runic language definition.
keep :: RunicT m ()
keep = reserved runicTokenParser "keep"

-- | Matches the on keyword in the Runic language definition.
on :: RunicT m ()
on = reserved runicTokenParser "on"

-- | Matches the guess keyword in the Runic language definition.
guess :: RunicT m ()
guess = reserved runicTokenParser "guess"

-- | Matches the for keyword in the Runic language definition.
for :: RunicT m ()
for = reserved runicTokenParser "for"

-- | Matches the const keyword in the Runic language definition.
const :: RunicT m ()
const = reserved runicTokenParser "const"

-- | Matches the left bracket to the right bracket of a domain 
--   declaration.
domainBounds :: RunicT m (RnNum, RnNum)
domainBounds = do
    _ <- char '['
    lowerBound <- mathExprEndingWith $ comma runicTokenParser
    upperBound <- mathExprEndingWith $ char ']'
    return (lowerBound, upperBound)
    