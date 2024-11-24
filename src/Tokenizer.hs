{-# LANGUAGE OverloadedStrings #-}
module Tokenizer 
    ( initLineNoMap 
    , runicTokenize
    , RunicItem
    ) where

import Prelude hiding ( lines, lookup, words )

import Control.Arrow ( arr, (>>>) )
import Data.Map ( foldrWithKey, fromList, keys, lookup, Map )
import Data.Text ( replace, split, words, Text )

data RunicItem = RunicItem Int RunicToken

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
    deriving (Show)

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
