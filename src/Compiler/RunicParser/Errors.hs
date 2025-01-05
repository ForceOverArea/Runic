{-# LANGUAGE Safe #-}
{-# LANGUAGE InstanceSigs #-}
module Compiler.RunicParser.Errors 
    ( RunicParserError(..)
    ) where

import Compiler.RunicParser.Types ( RunicKeyword, Token, TokenTracker(..) )

data RunicParserError 
    = MismatchedToken [RunicKeyword] Token 

instance Show RunicParserError where
    show :: RunicParserError -> String
    show (MismatchedToken [expected] (TokenTracker lineNo actual)) 
        = "expected " 
        ++ show expected
        ++ " but found "
        ++ show actual 
        ++ " on line "
        ++ show lineNo
    show (MismatchedToken whitelist (TokenTracker lineNo actual))
        = "expected "
        ++ foldr 
            ((++) . (++) " or " . show) 
            (show $ head whitelist) 
            (tail whitelist)
        ++ ", but found"
        ++ show actual
        ++ " on line "
        ++ show lineNo