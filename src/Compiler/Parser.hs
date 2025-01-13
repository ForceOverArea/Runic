{-# LANGUAGE Safe #-}
module Compiler.Parser 
    ( runicParser
    ) where

import safe Compiler.Internal (Runic)
import safe Compiler.Parser.RunicLexemes (constDecl, domainDecl, guessDecl)
import safe Text.Parsec ((<|>))

runicParser :: Runic ()
runicParser = do 
    constDecl <|> guessDecl <|> domainDecl -- <|> functionDecl
