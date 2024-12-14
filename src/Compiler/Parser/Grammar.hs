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
import Compiler.Parser.Objects ( Token(..) )
import Compiler.Parser.Factory ( (<->), (<+>), liftStashParser, parseExpression, ParserState )

{-|
A token validation function for a domain specification in Runic 
source. This validator captures the domain bounds and name for
further processing.
-}
validateDomain :: Token -> [Token] -> Either String (ParserState Token)
validateDomain = parseExpression $ liftStashParser 
    (Expr "") <-> On <-> LBrack <+> Expr "" <-> RBrack

{-|
A token validation function for a guess statement in Runic source. 
This validator captures the guess expression and name for further 
processing.
-}
validateGuess :: Token -> [Token] -> Either String (ParserState Token)
validateGuess = parseExpression $ liftStashParser
    (Expr "") <-> For <+> Expr ""
 