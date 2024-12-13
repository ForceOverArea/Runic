{-# LANGUAGE OverloadedStrings #-}
-- module Main (main) where

import Test.HUnit ( assertEqual, Counts, runTestTT, test, (~:) )
import Compiler.Parser ( ParserState(..) )
import Compiler.Objects ( Token(..) )
import Compiler.Grammar ( validateDomain )

main :: IO Counts
main = do 
    runTestTT $ test 
        -- Ensure that the correct captures are present
        [ "testValidateDomain" ~: assertEqual ""
            (Right $ ParserState RBrack [Expr "x", Expr "0, 100"] [])
            (validateDomain Keep [Expr "x", On, LBrack, Expr "0, 100", RBrack])
        
        -- Ensure that the correct error message is shown
        , "testValidateDomainFail" ~: assertEqual ""
            (Left "expected LBrack after On, but found Expr \"0, 100\"")
            (validateDomain Keep [Expr "x", On, Expr "0, 100", RBrack])
        ]
