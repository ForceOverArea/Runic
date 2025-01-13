{-# LANGUAGE Safe #-}
module Compiler.Parser.RunicLexemes 
    ( constDecl
    , domainDecl
    , guessDecl
    , keep
    , on
    ) where

import safe Prelude hiding (const, map)
import safe Compiler.Evaluator (evaluateExprWithCtx)
import safe Compiler.Evaluator.Internal as E (CtxItem(..))
import safe Compiler.Internal as R (CtxItem(..), RnNum, Runic)
import safe Compiler.Parser.Internal (runicAddToCtx, runicGetFromCtx)
import safe Compiler.Parser.RunicTokens (runicTokenParser)
import safe Data.Map (map)
import safe Text.Parsec (anyChar, char, endOfLine, getState, manyTill)
import safe Text.Parsec.Prim (try)
import safe Text.Parsec.Token (GenTokenParser (comma, identifier, operator, reserved))

-- | Matches a domain expression, returning the variable name 
--   associated with the guess expression.
domainDecl :: Runic ()
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
guessDecl :: Runic ()
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
constDecl :: Runic ()
constDecl = do
    const
    name <- identifier runicTokenParser
    _ <- operator runicTokenParser
    value <- mathExprEndingWith endOfLine
    existing <- runicGetFromCtx name
    case existing of
        Nothing -> runicAddToCtx name $ R.Const value
        Just _ -> error "ligma balls"

-- | Matches an unknown number of characters ending at the given 
--   lexeme. 
--
--   E.g. domainDecl matches characters to pass to the shunting yard
--   implementation until it hits a comma or a right bracket.
mathExprEndingWith :: Runic a -> Runic RnNum
mathExprEndingWith ending = do
    expr <- manyTill anyChar (try ending)
    ctx <- map toCtxItem <$> getState 
    case evaluateExprWithCtx expr ctx of
        (Right val) -> return val
        (Left _err) -> error "ligma balls"
    where
        toCtxItem :: R.CtxItem -> E.CtxItem
        toCtxItem (R.Const v) = E.Const v
        toCtxItem (Variable v _ _) = E.Const v
        toCtxItem (R.Function n f) = E.Function n f

-- | Matches the keep keyword in the Runic language definition.
keep :: Runic ()
keep = reserved runicTokenParser "keep"

-- | Matches the on keyword in the Runic language definition.
on :: Runic ()
on = reserved runicTokenParser "on"

-- | Matches the guess keyword in the Runic language definition.
guess :: Runic ()
guess = reserved runicTokenParser "guess"

-- | Matches the for keyword in the Runic language definition.
for :: Runic ()
for = reserved runicTokenParser "for"

-- | Matches the const keyword in the Runic language definition.
const :: Runic ()
const = reserved runicTokenParser "const"

-- | Matches the left bracket to the right bracket of a domain 
--   declaration.
domainBounds :: Runic (RnNum, RnNum)
domainBounds = do
    _ <- char '['
    lowerBound <- mathExprEndingWith $ comma runicTokenParser
    upperBound <- mathExprEndingWith $ char ']'
    return (lowerBound, upperBound)
    