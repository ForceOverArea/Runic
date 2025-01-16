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



-- testDomainDecl :: String
-- testDomainDecl = 
--     let captured = testLexeme domainDecl source "testDomainDecl"
--     in show $ difference captured empty
--     where source = "keep x on []"

main :: IO ()
main = do
    print testConstDecl