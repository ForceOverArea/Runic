{-# LANGUAGE Safe #-}
module Parser.Lang
    ( runicTokenParser
    ) where

import safe Text.Parsec (alphaNum, char, letter, oneOf, (<|>))
import safe Text.Parsec.Token (makeTokenParser, GenLanguageDef(..), GenTokenParser)

-- | Definition of the Runic equation-solver language.
runicDef :: Monad m => GenLanguageDef String st m
runicDef = LanguageDef 
    { commentStart      = "/*"
    , commentEnd        = "*/"
    , commentLine       = "//"
    , nestedComments    = False
    , identStart        = letter
    , identLetter       = alphaNum <|> char '_'
    , opStart           = oneOf "=<>!"
    , opLetter          = oneOf "=" -- for partial ordering/equality operators
    , reservedNames     = keywords
    , reservedOpNames   = operators
    , caseSensitive     = True
    }
    where
        operators = 
            [ "^"
            , "*"
            , "/"
            , "+"
            , "-"
            , "=>"
            ]
        keywords = 
            [ "keep"
            , "on"
            , "guess"
            , "for"
            , "const"
            , "fn"
            , "return"
            , "system"
            , "if"
            , "then"
            , "else"
            , "integrate"
            , "derivative"
            , "convert"
            ]

-- | A token parser instantiated from the `runicDef` 
--   language definition.
runicTokenParser :: Monad m => GenTokenParser String st m
runicTokenParser = makeTokenParser runicDef
