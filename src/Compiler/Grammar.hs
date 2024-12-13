{-# LANGUAGE OverloadedStrings #-}

module Compiler.Grammar
    ( validateDomain
    , validateGuess
    ) where

import Compiler.Objects ( Token(..) )
import Compiler.Parser
    ( (<->)
    , (<+>)
    , liftStashParser
    , parseExpression
    , ParserState
    )

validateDomain :: Token -> [Token] -> Either String (ParserState Token)
validateDomain = parseExpression $ liftStashParser 
    (Expr "") <-> On <-> LBrack <+> Expr "" <-> RBrack

validateGuess :: Token -> [Token] -> Either String (ParserState Token)
validateGuess = parseExpression $ liftStashParser
    (Expr "") <-> For <+> Expr ""