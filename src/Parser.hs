{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Data.Text
    ( Text
    , replace
    , split
    )
import Control.Monad.State.Strict ()
import Control.Arrow

data VariableContext
    = Domain Double Double
    | Guess Double

data GlobalContext
    = Const String Double
    | Function Text

newtype Equation 
    = Equation Text

type RunicData = ([VariableContext], [GlobalContext], [Equation])

preProcRunicFile :: Text -> Either String RunicData
preProcRunicFile = 

parseRunicFile :: Text -> Either String RunicData
parseRunicFile t = 
    let fileLines = split (=='\n') $  $  t;
    parseLine fileLines
    where
        parseLine :: [Text] -> RunicData -> Either String RunicData
        parseLine [] rd = Right rd
        parseLine (line:lines) (vc, gc, eq)
            | line 