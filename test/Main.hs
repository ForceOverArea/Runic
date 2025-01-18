module Main (main) where

import Compiler.Internal (RnCtx)
import Compiler.Parser.RunicLexemes (constDecl, domainDecl)
import Data.Map (difference, empty)
import Helpers (testLexeme)

testConstDecl :: String
testConstDecl = 
    let captured = testLexeme constDecl source "testConstDecl"
    in show $ difference captured empty
    where source = "const x = 3\n"

-- testConversion :: Double
-- testConversion = 
--     let captured = testLexeme constDecl source "testConversion"
--     in show $ difference captured
--     where source = "[m->millimeter]"

testConversion' :: Double
testConversion' = 
    let captured = testLexeme conversion' source "testConversion'"
    in show $ difference captured
    where source = "convert(\"m\",\"millimeter\")"

main :: IO ()
main = do
    print testConstDecl
    print testConversion
    print testConversion'