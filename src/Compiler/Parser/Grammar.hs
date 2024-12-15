{-# LANGUAGE OverloadedStrings #-}
{-|
    A module for adding validator functions to the Runic compiler. 
    These can be used in the top-level state machine used to parse 
    the raw source for the system of equations.
-}
module Compiler.Parser.Grammar
    ( validateDomain
    , validateGuess
    ) where

import Prelude hiding ( lookup )
import Compiler.Parser.Objects ( makeRt, RToken, Token(..) )
import Compiler.Parser.Factory ( (<->), (<+>), liftStashParser, parseExpression, ParserState )

{-|
A token validation function for a domain specification in Runic 
source. This validator captures the domain bounds and name for
further processing.
-}
validateDomain :: RToken -> [RToken] -> Either String (ParserState RToken)
validateDomain = parseExpression $ 
    liftStashParser (makeRt $ Expr "") 
    <-> makeRt On 
    <-> makeRt LBrack 
    <+> makeRt (Expr "") 
    <-> makeRt RBrack

{-|
A token validation function for a guess statement in Runic source. 
This validator captures the guess expression and name for further 
processing.
-}
validateGuess :: RToken -> [RToken] -> Either String (ParserState RToken)
validateGuess = parseExpression $ 
    liftStashParser (makeRt $ Expr "") 
    <-> makeRt For 
    <+> makeRt (Expr "")
