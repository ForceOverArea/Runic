{-# LANGUAGE Safe #-}
module Compiler.Parser 
    ( runicParser
    ) where

import safe Compiler.Internal (RnCtx, Runic)
import safe Compiler.Parser.RunicLexemes (constDecl, domainDecl, guessDecl)
import safe Text.Parsec ((<|>), getState)

runicParser :: Runic RnCtx
runicParser = do 
    constDecl <|> guessDecl <|> domainDecl -- <|> functionDecl
    getState -- return the final state as created by the parsing process