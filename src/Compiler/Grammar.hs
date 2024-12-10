{-# LANGUAGE OverloadedStrings #-}

module Compiler.Grammar
    ( validateDomain
    , validateGuess
    ) where

import Compiler.Objects ( Token(..) )
import Compiler.Parser
    ( (<->)
    , (<+>)
    , liftParser
    , parseExpression
    , ParserState
    )

validateDomain :: Token -> [Token] -> Either String (ParserState Token)
validateDomain = parseExpression $ liftParser 
    Keep <+> Expr "" <-> On <-> LBrack <+> Expr "" <-> RBrack

validateGuess :: Token -> [Token] -> Either String (ParserState Token)
validateGuess = parseExpression $ liftParser
    Guess <+> Expr "" <-> For <+> Expr ""