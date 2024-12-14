module Main 
    ( main ) where

import Compiler.Preprocess ( tokenizeSource )
import System.Environment ( getArgs )
import Data.Text ( pack )
    
main :: IO ()
main = do
    args <- getArgs
    rawText <- pack <$> readFile (head args)
    let (tokens, lineNoMap) = tokenizeSource rawText
    return ()
