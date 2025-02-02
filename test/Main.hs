{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Prelude hiding (sum)
import Data.Map (difference, empty, fromList)
import Helpers (runTest, testLexeme, TestRslt, enumerate, testLexemeWithUCData)
import Data.Runic.Parser.Lexemes
import Data.Runic.Parser.Math 
import Data.Runic.Parser.Units
import Data.Runic.Types (Quantity(..), UCMap)

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
    where source = "const x = 3 + 3;\
                   \const y = 4 * 4;"

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

testExpression :: String -> TestRslt String
testExpression source = do
    result <- testLexemeWithUCData
        expression
        basicConversionData
        source
        "testExpression"
    return (show $ fst result)

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
    , testExpression "4 + 5 * (9 - 3) ^ 2"
    , testExpression "4 + 5 * (3 - 9) ^ 2"
    , testExpression "4 + 5 * 3 - 9"
    , testExpression "2^2"
    , testExpression "2^3^3"
    , testExpression "2 * convert(millimeter, m)"
    ]
