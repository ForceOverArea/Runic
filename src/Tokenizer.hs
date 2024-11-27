{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
module Tokenizer 
    ( initLineNoMap 
    , runicTokenize
    , rKeywords
    , RunicItem (..)
    , RunicToken (..)
    ) where

import Prelude hiding ( lines, lookup, words )

import Control.Arrow ( arr, (>>>) )
import Data.Map ( foldrWithKey, fromList, keys, lookup, Map )
import Data.Text ( replace, split, unpack, words, Text )

data RunicItem = RunicItem Int RunicToken
instance Show RunicItem where
    show :: RunicItem -> String
    show (RunicItem _ RNewline) = show RNewline
    show (RunicItem l tok) = show tok ++ " on line " ++ show l

-- | All the different token values that may exist in Runic source
data RunicToken
    = RKeep
    | ROn
    | RLBracket
    | RRBracket
    | RGuess
    | RFor
    | RConst
    | RFn
    | RReturn
    | RIf
    | RThen
    | RElse
    | RSystem
    | REnd
    -- These tokens do not have a corresponding Runic keyword:
    | RExpression Text
    | RNewline
    deriving (Eq, Ord)
    
instance Show RunicToken where
    show :: RunicToken -> String
    show tok = case tok of
        RKeep     -> keyword "keep"
        ROn       -> keyword "on"
        RLBracket -> "'['"
        RRBracket -> "']'"
        RGuess    -> keyword "guess"
        RFor      -> keyword "for"    
        RConst    -> keyword "const"  
        RFn       -> keyword "fn"     
        RReturn   -> keyword "return" 
        RIf       -> keyword "if"     
        RThen     -> keyword "then"   
        RElse     -> keyword "else"   
        RSystem   -> keyword "system" 
        REnd      -> keyword "end"    
        (RExpression "") -> "expression"
        (RExpression t) -> "expression '" ++ unpack t ++ "'"
        RNewline  -> "newline"
        where 
            keyword :: String -> String
            keyword s = "keyword '" ++ s ++ "'"

-- | Provides mapping between which substrings represent which tokens.
rKeywords :: Map Text RunicToken
rKeywords = fromList
    [ ("keep"   , RKeep)
    , ("on"     , ROn)
    , ("["      , RLBracket)
    , ("]"      , RRBracket)
    , ("guess"  , RGuess)
    , ("for"    , RFor)
    , ("const"  , RConst)
    , ("fn"     , RFn)
    , ("return" , RReturn)
    , ("returns", RReturn)
    , ("if"     , RIf)
    , ("then"   , RThen)
    , ("else"   , RElse)
    , ("system" , RSystem)
    , ("end"    , REnd)
    ]

-- | Converts keywords to Tokens understood by the compiler
rTokenize :: Text -> RunicToken
rTokenize t =
    case lookup t rKeywords of
        Nothing -> RExpression t
        Just x -> x

-- | Converts text to a List of RunicTokens 
rTokenizeRawText :: Text -> [RunicToken]
rTokenizeRawText =
    arr replace "\t" ""
    >>> replace " " ""
    >>> (\x -> foldr punctuateTokens x $ keys rKeywords)
    >>> words
    >>> map rTokenize
    where
        punctuateTokens :: Text -> Text -> Text
        punctuateTokens h n = replace n (" " <> n <> " ") h

-- | Creates a map of line numbers to their corresponding text.
initLineNoMap :: Text -> Map Int Text
initLineNoMap t =
    let rLinesOfText = split (== '\n') t
        rLineNos     = [0..length rLinesOfText - 1]
    in fromList $ zip rLineNos rLinesOfText

-- | Creates a List of RunicItems (RunicTokens with the line number
-- they originated from)
runicTokenize :: Map Int Text -> [RunicItem]
runicTokenize = foldrWithKey createRunicItems []
    where
        createRunicItems :: Int -> Text -> [RunicItem] -> [RunicItem]
        createRunicItems lineNo lineText items =
            items ++ map (RunicItem lineNo) (rTokenizeRawText lineText)
