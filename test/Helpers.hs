module Helpers 
    ( testLexeme
    ) where

import Compiler.Parser.RunicTokens (runicTokenParser)
import Compiler.Internal (RnCtx, Runic)
import qualified Data.Map as Map (empty)
import Text.Parsec (getState, runParser)

testLexeme :: Runic a -> String -> String -> RnCtx
testLexeme parser text testName = 
    case runParser (parser >> getState) Map.empty testName text of
        (Right ctx) -> ctx
        (Left err) -> error $ show err
