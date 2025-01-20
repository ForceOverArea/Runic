module Main (main) where

import Data.Map (difference, empty)
import Helpers (testLexeme)
import Parser.Lexemes (constDecl)
import Parser.Units (conversion, conversion')

-- testConstDecl :: String
-- testConstDecl = 
--     let captured = testLexeme constDecl source "testConstDecl"
--     in show $ difference (snd captured) empty
--     where source = "const x = 3\n"

testConversion :: String
testConversion = 
    let captured = testLexeme conversion source "testConversion"
    in show $ fst captured
    where source = "[m=>millimeter]"

testConversion' :: String
testConversion' = 
    let captured = testLexeme conversion' source "testConversion'"
    in show $ fst captured
    where source = "convert(\"m\",\"millimeter\")"

main :: IO ()
main = do
    -- print testConstDecl
    print testConversion
    print testConversion'