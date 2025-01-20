module Helpers 
    ( testLexeme
    ) where

import qualified Data.Map as Map (empty)
import Parser.Internal (runRunic)
import Types (RnCtx, RunicT)
import Data.Functor.Identity (Identity)

testLexeme :: RunicT Identity a -> String -> String -> (a, RnCtx)
testLexeme parser text testName =
    let result = runRunic parser Map.empty testName text
    in case result of
        (Right final) -> final
        (Left err) -> error $ show err
