{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}
module Parser.Lexemes
    ( constDecl
    , domainDecl
    , guessDecl
    , keep
    , on
    , guess
    , for
    , const
    ) where

import safe Prelude hiding (const, map)
import safe Parser.Internal (runicAddToCtx, runicGetFromCtx)
import safe Parser.Lang (runicTokenParser)
import safe Parser.Math (expression)
import safe Text.Parsec (char, endOfLine)
import safe Text.Parsec.Token (GenTokenParser(..))
import safe Types (CtxItem(..), RnNum, RunicT)

-- | Matches a domain expression, returning the variable name 
--   associated with the guess expression.
domainDecl :: Monad m => RunicT m ()
domainDecl = do
    keep
    name <- identifier runicTokenParser
    on
    domain <- brackets runicTokenParser domainBounds
    _ <- endOfLine
    existing <- runicGetFromCtx name
    case existing of
        Nothing -> runicAddToCtx name $ Variable 1 Nothing (Just domain)
        Just (Variable v g Nothing) -> runicAddToCtx name $ Variable v g (Just domain)
        _ -> error "TODO: add parsec error reporting here! (Lexemes.hs line 35)"

-- | Matches a guess expression, returning the variable name 
--   associated with the guess expression.
guessDecl :: Monad m => RunicT m ()
guessDecl = do
    guess
    value <- expression
    for
    name <- identifier runicTokenParser
    _ <- endOfLine
    existing <- runicGetFromCtx name
    case existing of
        Nothing -> runicAddToCtx name $ Variable 1 (Just value) Nothing
        Just (Variable v Nothing d) -> runicAddToCtx name $ Variable v (Just value) d
        _ -> error "TODO: add parsec error reporting here! (Lexemes.hs line 51)"

-- | Matches a const declaration, returning the constant's name 
--   and numeric value.
constDecl :: Monad m => RunicT m ()
constDecl = do
    const
    name <- identifier runicTokenParser
    _ <- char '='
    value <- expression
    _ <- char ';'
    existing <- runicGetFromCtx name
    case existing of
        Nothing -> runicAddToCtx name $ Const value
        Just _ -> error "TODO: add parsec error reporting here! (Lexemes.hs line 65)"

-- | Matches the keep keyword in the Runic language definition.
keep :: Monad m => RunicT m ()
keep = reserved runicTokenParser "keep"

-- | Matches the on keyword in the Runic language definition.
on :: Monad m => RunicT m ()
on = reserved runicTokenParser "on"

-- | Matches the guess keyword in the Runic language definition.
guess :: Monad m => RunicT m ()
guess = reserved runicTokenParser "guess"

-- | Matches the for keyword in the Runic language definition.
for :: Monad m => RunicT m ()
for = reserved runicTokenParser "for"

-- | Matches the const keyword in the Runic language definition.
const :: Monad m => RunicT m ()
const = reserved runicTokenParser "const"

-- | Matches the left bracket to the right bracket of a domain 
--   declaration.
domainBounds :: Monad m => RunicT m (RnNum, RnNum)
domainBounds = do
    lowerBound <- expression 
    _ <- comma runicTokenParser
    upperBound <- expression 
    return (lowerBound, upperBound)
