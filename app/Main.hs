module Main 
    ( main ) where

import Compiler.Preprocess ( tokenizeSource )
import System.Environment ( getArgs )
import Data.Aeson ()
import Data.Text ( pack )
    
    
main :: IO ()
main = do
    args <- getArgs
    unitConversions <- 
    rawText <- pack <$> readFile (head args)
    let (tokens, lineNoMap) = tokenizeSource rawText
    return ()
