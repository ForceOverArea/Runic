{-# LANGUAGE LambdaCase #-}
module Helpers 
    ( enumerate
    , runTest
    , testLexeme
    , testLexemeWithUCData
    , TestRslt
    ) where

import qualified Data.Map as M (empty)
import Data.Runic.Parser.Internal (runRunic)
import Types (RnCtx, RunicT, UCMap)
import Data.Functor.Identity (Identity)
import Text.Parsec (eof, many, ParseError)

type TestRslt = Either ParseError

testLexeme :: RunicT Identity a -> String -> String -> Either ParseError ([a], RnCtx)
testLexeme = flip testLexemeWithUCData M.empty

testLexemeWithUCData :: RunicT Identity a -> UCMap -> String -> String -> Either ParseError ([a], RnCtx)
testLexemeWithUCData parser ctx text testName = runRunic parser' ctx testName text
    where
        parser' = do
            result <- many parser
            eof
            return result

enumerate :: [a] -> [(a, Int)]
enumerate = foldl f []
    where
        f :: [(a, Int)] -> a -> [(a, Int)]
        f acc i = acc ++ [(i, length acc + 1)]

runTest :: (Show a, Show b) => (Either a b, Int) -> IO ()
runTest = \case
    (Right result, ln) -> putStrLn $ "[  OK ].....(" ++ show ln ++ ") " ++ show result 
    (Left err, ln)     -> putStrLn $ "[ NOK ].....(" ++ show ln ++ ") " ++ show err