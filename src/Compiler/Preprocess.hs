{-# LANGUAGE OverloadedStrings #-}

module Compiler.Preprocess 
    ( tokenizeSource
    ) where

import Prelude hiding ( lookup, words )
import Compiler.Internal ( tokenMapping, RunicKeyword(..), TokenTracker(..) )
import Control.Arrow ( (>>>), (&&&), arr, first )
import Data.Map ( fromList, keys, lookup, Map )
import Data.Text ( replace, split, words, Text )

-- | Enumerates lines of text with their line numbers starting at 1
getLineNumbers :: [Text] -> [(Int, Text)]
getLineNumbers lns = zip [1..length lns + 1] lns

{-|
    Replaces keywords that are purely shorthand for things already
    expressible in Runic source code.
-}
substSimpleKeywords :: Text -> Text
substSimpleKeywords 
    = replace "negative" "on [-inf, 0]" 
    . replace "positive" "on [0, inf]"

{-|
    Looks for all tokens defined in `tokenMapping` (i.e. all legal 
    keywords in the Runic language) and pads them with additional 
    whitespace. This guarantees they are delimited by spaces to 
    create a list of tokens in a block of text.
-}
punctuate :: Text -> Text
punctuate txt = foldr puncKw txt (keys tokenMapping)
    where
        puncKw :: Text -> Text -> Text
        puncKw wrd = replace wrd (" " <> wrd <> " ")

-- | Converts a block of text to a token or an expression.
tokenize :: Text -> RunicKeyword
tokenize t = case lookup t tokenMapping of
    Just tok -> tok
    Nothing -> Expr t

-- | Converts a line of text to a list of TokenTrackers.
tokenizeLine :: Int -> Text -> [TokenTracker RunicKeyword]
tokenizeLine ln t = 
    map (TokenTracker ln . tokenize) (words $ punctuate t) ++ [TokenTracker ln NewLine]

{-|
    Produces a list of all tokens in the given block of text as well 
    as a map of line numbers to their contained text. In this case 
    the block is the Runic source code.
-}
tokenizeSource :: Text -> ([TokenTracker RunicKeyword], Map Int Text)
tokenizeSource = 
    arr substSimpleKeywords
    >>> split (== '\n')
    >>> getLineNumbers
    >>> map (uncurry tokenizeLine) &&& fromList
    >>> first concat