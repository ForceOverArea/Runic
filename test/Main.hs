{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Prelude hiding (sum)
import Data.Map (difference, empty, fromList)
import Helpers (runTest, testLexeme, TestRslt, enumerate, testLexemeWithUCData)
import Parser.Lexemes (constDecl)
import Parser.Math (sum)
import Parser.Units (conversion, conversion')
import Types (Quantity(..), UCMap)

-- | 
basicConversionData :: UCMap
basicConversionData = fromList 
    [ ("m",             (1.0,    Length))
    , ("millimeter",    (0.001,  Length))
    ]

-- | 
testConstDecl :: TestRslt String
testConstDecl = do
    captured <- testLexeme constDecl source "testConstDecl"
    return (show $ difference (snd captured) empty)
    where source = "const x = 3.0 + 3.0 \n"

-- | 
testConversion :: TestRslt String
testConversion = do
    captured <- testLexemeWithUCData 
        conversion 
        basicConversionData 
        source 
        "testConversion"
    return (show $ fst captured)
    where source = "[m=>millimeter]"

-- | 
testConversion' :: TestRslt String
testConversion' = do
    captured <- testLexemeWithUCData 
        conversion' 
        basicConversionData 
        source 
        "testConversion' (Prime)"
    return (show $ fst captured)
    where source = "convert(m,millimeter)"

testSum :: String -> TestRslt String
testSum source = do
    captured <- testLexemeWithUCData 
        sum 
        basicConversionData
        source
        "testSum"
    return (show $ fst captured)

-- | 
main :: IO [()]
main = mapM runTest 
    $ enumerate
    [ testConstDecl
    , testConversion
    , testConversion'
    , testSum "4 + 5"
    , testSum "4.0 + 5.0"
    , testSum "4 + 5 - 9"
    ]
