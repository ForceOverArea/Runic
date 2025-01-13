{-# LANGUAGE Safe #-}
module Compiler.Parser.RunicTokens 
    ( runicTokenParser
    ) where

import safe Text.Parsec ( alphaNum, char, letter, oneOf, (<|>) )
import safe Text.Parsec.Token ( makeTokenParser, GenLanguageDef(..), LanguageDef, TokenParser )

-- | Definition of the Runic equation-solver language.
runicDef :: LanguageDef st
runicDef = LanguageDef 
    { commentStart      = "/*"
    , commentEnd        = "*/"
    , commentLine       = "//"
    , nestedComments    = False
    , identStart        = letter
    , identLetter       = alphaNum <|> char '_'
    , opStart           = oneOf "^*/+-<!=>$\\"
    , opLetter          = oneOf "/=>" -- for derivatives, partial equality, and unit conversion operator
    , reservedNames     = keywords
    , reservedOpNames   = []
    , caseSensitive     = True
    }
    where
        keywords = 
            [ "keep"
            , "on"
            , "guess"
            , "for"
            , "const"
            , "fn"
            , "return"
            , "system"
            ]

-- | A token parser instantiated from the `runicDef` 
--   language definition.
runicTokenParser :: TokenParser st
runicTokenParser = makeTokenParser runicDef
