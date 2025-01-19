module Helpers 
    ( testLexeme
    ) where

import Parser.Lang (runicTokenParser)
import Types (RnCtx, RunicT)
import qualified Data.Map as Map (empty)
import Text.Parsec (getState, runParserT)

testLexeme :: Monad m => RunicT m a -> String -> String -> RnCtx
testLexeme parserT text testName = 
    case runParser (parser >> getState) Map.empty testName text of
        (Right ctx) -> ctx
        (Left err) -> error $ show err
