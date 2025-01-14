module Main (main) where

import Compiler.Internal (RnCtx)
import Compiler.Parser.RunicLexemes (constDecl)
import Data.Map (difference, empty)
import Helpers (testLexeme)

testConstDecl :: String
testConstDecl = 
    let captured = testLexeme constDecl source "testConstDecl"
    in show $ difference captured empty
    where source = "const x = 3"

main :: IO ()
main = do
    print testConstDecl